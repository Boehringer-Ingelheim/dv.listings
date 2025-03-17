# nolint start
SE <- local({ # _SE_rialization_helpers
  get_UTC_time_in_seconds <- function() as.numeric(structure(Sys.time(), tzone = 'UTC'))
  double_to_raw <- function(v) writeBin(v, con = raw(0), endian = 'little', useBytes = TRUE)
  integer_to_raw <- function(v) writeBin(v, con = raw(0), endian = 'little', useBytes = TRUE)
  string_to_raw <- function(v) c(integer_to_raw(nchar(v)), charToRaw(v))
  character_vector_to_raw <- function(v){
    res <- integer_to_raw(length(v))
    for(s in v) res <- c(res, string_to_raw(s))
    return(res)
  }
  integer_vector_to_raw <- function(v){
    res <- integer_to_raw(length(v))
    for(s in v) res <- c(res, integer_to_raw(s))
    return(res)
  }

  hash_data_frame_row <- function(row) {
    input <- paste(row, collapse = '\1D')
    input <- charToRaw(input)
    res <- xxhashlite::xxhash_raw(input, as_raw = TRUE)
    return(res)
  }

  read_string_from_con <- function(con){
    res <- NULL
    n <- readBin(con, integer(), 1L)
    if(length(n) > 0) res <- readBin(con, raw(), n) |> rawToChar()
    return(res)
  }

  read_character_vector_from_con <- function(con){
    res <- character(0)
    n <- readBin(con, integer(), 1L)
    for(i in seq_len(n)) res <- c(res, read_string_from_con(con))
    return(res)
  }

  read_integer_vector_from_con <- function(con){
    res <- integer(0)
    n <- readBin(con, integer(), 1L, endian = 'little')
    res <- readBin(con, integer(), n, endian = 'little')
    return(res)
  }

  read_hashes_from_con <- function(con, hash_count){
    expected_hash_byte_count <- hash_count*16L
    hash_vector <- readBin(con, raw(), expected_hash_byte_count)
    ; if(length(hash_vector) < expected_hash_byte_count) return(simpleCondition("Not enough hash data"))
    res <- array(hash_vector, dim = c(16, hash_count))
    return(res)
  }

  return(
    list(
      get_UTC_time_in_seconds = get_UTC_time_in_seconds,
      double_to_raw = double_to_raw,
      integer_to_raw = integer_to_raw,
      string_to_raw = string_to_raw,
      character_vector_to_raw = character_vector_to_raw,
      integer_vector_to_raw = integer_vector_to_raw,
      hash_data_frame_row = hash_data_frame_row,
      read_string_from_con = read_string_from_con,
      read_character_vector_from_con = read_character_vector_from_con,
      read_integer_vector_from_con = read_integer_vector_from_con,
      read_hashes_from_con = read_hashes_from_con
    )
  )
})

RE_hash_data_frame <- function(df){
  # Strip arguments from unnecessary detail
  attributes(df) <- list(class = 'data.frame', names = names(df), row.names = seq_len(nrow(df)))
  
  res <- xxhashlite::xxhash_raw(
    vec = serialize(object = df, connection = NULL, ascii = FALSE, xdr = FALSE, version = 3), 
    algo = 'xxh128', as_raw = TRUE
  )
  return(res)
}

RE_compute_base_memory <- function(df_id, df, id_vars, tracked_vars){
  checkmate::assert_string(df_id, min.chars = 1, max.chars = 65535)
  checkmate::assert_data_frame(df)
  # TODO: assert *_vars char unique col names of df
 
  id_vars <- sort(id_vars)
  tracked_vars <- sort(tracked_vars)
  
  df_hash <- RE_hash_data_frame(df)

  # row hashes
  id_hashes <- apply(df[id_vars], 1, SE$hash_data_frame_row, simplify = TRUE) # coerces all types to be the same (character?)
  ; if(!identical(dim(id_hashes), c(16L, nrow(df)))) return(simpleCondition("Internal error in id_vars hash preparation"))

  ; if(any(duplicated(id_hashes, MARGIN = 2))) return(simpleCondition("Found duplicated IDs"))

  tracked_hashes <- apply(df[tracked_vars], 1, SE$hash_data_frame_row, simplify = TRUE)
  ; if(!identical(dim(tracked_hashes), c(16L, nrow(df)))) return(simpleCondition("Internal error in tracked_vars hash preparation"))
  stopifnot(identical(dim(tracked_hashes), c(16L, nrow(df)))) # TODO: Assert
  
  # NOTE: We choose a serialization scheme with a well-known encoding. This avoid security concerns over 
  #       deserialization (https://aitap.github.io/2024/05/02/unserialize.html) and leaves the code open
  #       to alternative lightweight C implementations in case users require faster processing.
  #       It also allows us to create pure append-only files, which make it easy to avoid catastrophic
  #       loss of data in case something goes wrong, as we can look for the last correct byte on any given
  #       file, discard the remainder bytes and end up in a consistent state.
  res <- c(
    charToRaw("LISTBASE"),                          # file magic code
    as.raw(0),                                      # format version number
    as.raw(0),                                      # generation marker
    SE$double_to_raw(SE$get_UTC_time_in_seconds()), # timestamp
    df_hash,                                        # complete hash of input data.frame
    SE$string_to_raw(df_id),                        # domain string
    SE$character_vector_to_raw(id_vars),            # identifier vars
    SE$character_vector_to_raw(tracked_vars),       # tracked vars
    SE$integer_to_raw(nrow(df)),                    # row count
    id_hashes,                                      # one hash of id_vars per row
    tracked_hashes                                  # one hash of tracked_vars per row
  )
  
  return(res)
}

RE_parse_base <- function(contents){
  con <- rawConnection(contents, open = "r")
  file_magic_code <- readBin(con, raw(), 8L) |> rawToChar()
  ; if(!identical(file_magic_code, "LISTBASE")) return(simpleCondition("Wrong magic code"))
  format_version_number <- readBin(con, raw(), 1L)
  ; if(!identical(format_version_number, as.raw(0))) return(simpleCondition("Wrong format version number"))
  generation <- as.integer(readBin(con, raw(), 1L))
  ; if(!identical(generation, 0L)) return(simpleCondition("Wrong generation marker. Should be 0"))
  timestamp <- readBin(con, numeric(), 1L)
  time_delta <- SE$get_UTC_time_in_seconds() - timestamp
  ; if(time_delta < 0) return(simpleCondition("Timestamp is set in the future"))
  ; if(time_delta > 2**31) return(simpleCondition("Time delta is set too far in the past"))
  contents_hash <- readBin(con, raw(), 16L)
  domain_string <- SE$read_string_from_con(con)
  id_vars <- SE$read_character_vector_from_con(con)
  tracked_vars <- SE$read_character_vector_from_con(con)
  row_count <- readBin(con, integer(), 1L)
 
  id_hashes <- SE$read_hashes_from_con(con, row_count)
  tracked_hashes <- SE$read_hashes_from_con(con, row_count)
  
  empty_read <- readBin(con, raw(), 1L)
  ; if(length(empty_read) > 0) return(simpleCondition("Too much hash data"))
  close(con)
  
  res <- list(
    domain = domain_string,
    generation = generation,
    id_vars = id_vars,
    tracked_vars = tracked_vars,
    contents_hash = contents_hash,
    row_count = row_count,
    timestamp = timestamp,
    id_hashes = id_hashes,
    tracked_hashes = tracked_hashes
  )
  
  return(res)
}

RE_compute_delta_memory <- function(state, df){
  checkmate::assert_data_frame(df) # TODO: etc.
  
  time_delta <- as.integer(ceiling(SE$get_UTC_time_in_seconds() - state$timestamp))
  df_hash <- RE_hash_data_frame(df)

  id_vars <- state$id_vars
  id_hashes <- apply(df[id_vars], 1, SE$hash_data_frame_row, simplify = TRUE) |> c() |> array(dim = c(16, nrow(df)))
  
  merged <- cbind(state$id_hashes, id_hashes, deparse.level = 0)
  new_row_mask <- !duplicated(merged, MARGIN = 2) |> c() |> tail(n = nrow(df))
  new_row_indices <- which(new_row_mask)
 
  tracked_vars <- state$tracked_vars
  tracked_hashes <- apply(df[tracked_vars], 1, SE$hash_data_frame_row, simplify = TRUE) |> c() |> array(dim = c(16, nrow(df)))
  
  merged <- cbind(state$tracked_hashes, tracked_hashes, deparse.level = 0)
  modified_row_mask <- !duplicated(merged, MARGIN = 2) |> c() |> tail(n = nrow(df))
  modified_row_indices <- setdiff(which(modified_row_mask), new_row_indices)
  
  res <- c(
    charToRaw("LISTDELT"),                                # file magic code
    as.raw(0),                                            # format version number
    as.raw(state$generation+1L),                          # generation marker
    SE$integer_to_raw(time_delta),                        # delta timestamp (seconds since state)
    df_hash,                                              # complete hash of input data.frame
    SE$string_to_raw(state$domain),                       # domain string
    SE$integer_to_raw(length(new_row_indices)),           # new row count
    id_hashes[, new_row_indices, drop = FALSE],           # one hash of id_vars per new row
    tracked_hashes[, new_row_indices, drop = FALSE],      # one hash of tracked_vars per new row
    SE$integer_vector_to_raw(modified_row_indices),       # modified row indices
    tracked_hashes[, modified_row_indices, drop = FALSE]  # one hash of tracked_vars per modified row
  )
  
  return(res)
}

RE_parse_delta <- function(contents){
  con <- rawConnection(contents, open = "r")
  file_magic_code <- readBin(con, raw(), 8L) |> rawToChar()
  ; if(!identical(file_magic_code, "LISTDELT")) return(simpleCondition("Wrong magic code"))
  format_version_number <- readBin(con, raw(), 1L)
  ; if(!identical(format_version_number, as.raw(0))) return(simpleCondition("Wrong format version number"))
  generation <- as.integer(readBin(con, raw(), 1L))
  time_delta <- readBin(con, integer(), 1L)
  ; if(time_delta < 0) return(simpleCondition("Negative time delta"))
  contents_hash <- readBin(con, raw(), 16L)
  domain <- SE$read_string_from_con(con)
  new_row_count <- readBin(con, integer(), 1L)
  new_id_hashes <- SE$read_hashes_from_con(con, new_row_count)
  new_tracked_hashes <- SE$read_hashes_from_con(con, new_row_count)
  modified_row_count <- NA_integer_
  modified_row_indices <- SE$read_integer_vector_from_con(con)
  modified_row_count <- length(modified_row_indices)
  modified_tracked_hashes <- SE$read_hashes_from_con(con, modified_row_count)

  empty_read <- readBin(con, raw(), 1L)
  ; if(length(empty_read) > 0) return(simpleCondition("Too much hash data"))
  close(con)

  res <- list(
    domain = domain,
    generation = generation,
    contents_hash = contents_hash,
    new_row_count = new_row_count,
    new_id_hashes = new_id_hashes,
    new_tracked_hashes = new_tracked_hashes,
    modified_row_count = modified_row_count,
    modified_row_indices = modified_row_indices,
    modified_tracked_hashes = modified_tracked_hashes,
    time_delta = time_delta 
  )
  
  return(res)
}

RE_compute_review_codes_memory <- function(review_codes){
  checkmate::assert_character(review_codes, min.chars = 1)
  
  pcodes <- raw(0)
  for(code in review_codes) pcodes <- c(pcodes, SE$string_to_raw(code))
  
  res <- c(
    charToRaw("LISTCODE"), # file magic code
    as.raw(0),             # format version number
    pcodes                 # codes WITHOUT an integer saying how many of them there area
  )
  
  return(res)
}

RE_parse_review_codes <- function(contents){
  con <- rawConnection(contents, open = "r")
  file_magic_code <- readBin(con, raw(), 8L) |> rawToChar()
  ; if(!identical(file_magic_code, "LISTCODE")) return(simpleCondition("Wrong magic code"))
  format_version_number <- readBin(con, raw(), 1L)
  ; if(!identical(format_version_number, as.raw(0))) return(simpleCondition("Wrong format version number"))
  
  res <- character(0)
  code <- SE$read_string_from_con(con) 
  while(!is.null(code)){
    res <- c(res, code)
    code <- SE$read_string_from_con(con) 
  }
  
  close(con)

  return(res)
}

RE_load <- function(base, deltas){
  res <- RE_parse_base(base) 
  for(delta in deltas){
    state_delta <- RE_parse_delta(contents = delta)
    
    if(!identical(state_delta$generation, res$generation+1L))
      return(simpleCondition(paste("Wrong generation marker. Should be", res$generation+1L)))
    
    if(!identical(state_delta$domain, res$domain))
      return(simpleCondition(paste("Wrong domain. Expected", res$domain, "and got", state_delta$domain)))
   
    res$contents_hash <- state_delta$contents_hash
    res$generation <- state_delta$generation
    # new rows
    res$row_count <- res$row_count + state_delta$new_row_count
    res$id_hashes <- cbind(res$id_hashes, state_delta$new_id_hashes)
    res$tracked_hashes <- cbind(res$tracked_hashes, state_delta$new_tracked_hashes)
    # modified rows
    res$tracked_hashes[,state_delta$modified_row_indices] <- state_delta$modified_tracked_hashes
  }
  return(res)
}

# NOTE: The contents of the following conditional are a WIP of the annotation feature.
#       They are parked until user requirements and technical blockers are clarified.
if(FALSE){
  describe_and_time <- function(description, expr){
    t0 <- Sys.time()
    res <- force(expr)
    t1 <- Sys.time()
    cat(sprintf("%.2f s: %s\n", t1-t0, description))
    if(inherits(res, "condition")) stop(res)
    return(res)
  }

  # Build an arbitrarily large input dataset
  df <- local({
    df <- safetyData::adam_adae[1:1000,]
    df <- rbind(df, df, df, df, df, df, df, df, df, df) # 10k
    # df <- rbind(df, df, df, df, df, df, df, df, df, df) # 100k
    # df <- rbind(df, df, df, df, df, df, df, df, df, df) # 1000k
    
    for(i_row_block in seq_len(nrow(df)/1000)){
      fake_study_prefix <- sprintf("%05d", i_row_block)
      df[["USUBJID"]][seq((i_row_block-1)*1000 + 1, i_row_block*1000)] <- 
        sub('^01', fake_study_prefix, df[["USUBJID"]][seq((i_row_block-1)*1000 + 1, i_row_block*1000)])
    }
    return(df)
  })
 
  base_contents <- describe_and_time(
    "Derive .base file contents from initial dataset (expensive; only done the first time app is run by some user)", {
      df_id <- "ae"
      id_vars <- c("USUBJID", "AESEQ")
      tracked_vars <- setdiff(names(df), "id_vars") # tracks all non-id vars (worst case for performance)
      RE_compute_base_memory(df_id, df, id_vars, tracked_vars)
    }
  )
  
  # TODO: Write to disk
  # TODO: Read from disk

  review_codes_raw <- RE_compute_review_codes_memory(
    c("Seen", "Should look into", "Looked into", "We don't seem to agree", "Clearly artifactual")
  )
  
  if(FALSE){
    review_codes <- RE_parse_review_codes(review_codes_raw)
    
    browser() # TODO: RE_compute_role_review_memory + RE_parse_role_review
    
    review_roles <- c("TSTAT", "SP", "Safety", "CTL")
    
    for(role in review_roles){
      browser()
      
    }
  }

  review_state <- describe_and_time(
    "Initial load (base and no deltas)",
    RE_load(base_contents, deltas = list(), codes = character(0))
  )

  delta1_contents <- local({
    # Let's imagine there is an update that adds a new entry to the dataset
    df <- rbind(df, safetyData::adam_adae[1001,])
    # Furthermore, the severity of an adverse event is described now as "MODERATE" instead of "MILD"
    df[500, 'AESEV'] <- 'MODERATE'
    
    delta_contents <- describe_and_time(
      "Processing dataset update (done first time app is run after update)",
      RE_compute_delta_memory(review_state, df)
    )
    return(delta_contents)
  })
  
  review_state <- describe_and_time(
    "Initial load (base and one delta)",
    RE_load(base_contents, deltas = list(delta1_contents)) 
  )
  
  delta2_contents <- local({
    # Let's imagine there is an update that adds two new entry to the dataset
    df <- rbind(df, safetyData::adam_adae[1002:1003,])
    # Furthermore, the severity of an adverse event is described now as "SEVERE" instead of "MODERATE"
    df[500, 'AESEV'] <- 'SEVERE'
    # and another one that was "MILD" is now "MODERATE"
    df[501, 'AESEV'] <- 'SEVERE'
    
    delta_contents <- describe_and_time(
      "Processing dataset update (done first time app is run after update)",
      RE_compute_delta_memory(review_state, df)
    )
    return(delta_contents)
  })
  
  review_state <- describe_and_time(
    "Initial load (base and two deltas)",
    RE_load(base_contents, deltas = list(delta1_contents, delta2_contents)) 
  )
  
  browser() # TODO: Start the application based on base and deltas (RE_parse_base_and_deltas)
}

# nolint end

