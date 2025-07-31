BYTES_PER_TRACKED_HASH <- 2L

SH <- local({ # _S_erialization _H_elpers
  get_UTC_time_in_seconds <- function() as.numeric(structure(Sys.time(), tzone = "UTC"))
  double_to_raw <- function(v) writeBin(v, con = raw(0), endian = "little", useBytes = TRUE)
  integer_to_raw <- function(v) writeBin(v, con = raw(0), endian = "little", useBytes = TRUE)
  string_to_raw <- function(v) c(integer_to_raw(nchar(v)), charToRaw(v))
  character_vector_to_raw <- function(v) {
    res <- integer_to_raw(length(v))
    for (s in v) res <- c(res, string_to_raw(s))
    return(res)
  }
  integer_vector_to_raw <- function(v) {
    res <- integer_to_raw(length(v))
    for (s in v) res <- c(res, integer_to_raw(s))
    return(res)
  }

  ..ref_hash_id <- function(row) {
    input <- paste(row, collapse = "\1D")
    input <- charToRaw(input)
    res <- xxhashlite::xxhash_raw(input, as_raw = TRUE)
    return(res)
  }
  
..ref_hash_tracked_inner <- function(row) {
    # FIXME: Ensure that precision of numeric values does not affect serialization
    #        Maybe by using a string hex representation of their binary contents
    input <- paste(row, collapse = "\1D")
    input <- charToRaw(input)
    res <- xxhashlite::xxhash_raw(input, algo = "xxh32", as_raw = TRUE)
    return(res)
  }
  
  hash_tracked_offsets <- c(0, 2, 3)
  
  ..ref_hash_tracked <- function(row) {
    n_col <- length(row)
    
    res <- raw(BYTES_PER_TRACKED_HASH * n_col)
    for (i_col in seq(n_col)){      
      col_indices <- (((i_col - 1) + hash_tracked_offsets) %% n_col) + 1
      first <- BYTES_PER_TRACKED_HASH * (i_col - 1) + 1
      last <- BYTES_PER_TRACKED_HASH * i_col
      res[first:last] <- ..ref_hash_tracked_inner(row[col_indices])[1:BYTES_PER_TRACKED_HASH] # most significant bytes
      i_col <- i_col + 1
    }
    
    return(res)
  }

  vectorized_hash_row <- function(df, algo = "xxh128") {  
    vectorized_hash_id <- Vectorize(function(x) xxhashlite::xxhash_raw(charToRaw(x), as_raw = TRUE, algo = algo), USE.NAMES = FALSE, SIMPLIFY = FALSE)
    single_col <- do.call(function(...) paste(..., sep = "\1D"), lapply(df, as.character))
    hashed_col <- vectorized_hash_id(single_col)
    n_col <- length(hashed_col)
    n_row <- if (length(hashed_col) > 0) length(hashed_col[[1]]) else 0
    res <- matrix(unlist(vectorized_hash_id(single_col)), nrow = n_row, ncol = n_col)  
    res
  }

  hash_id <- vectorized_hash_row

  hash_tracked <- function(df) {
    n_col <- ncol(df)
    res <- list()   
    for (i_col in seq_len(n_col)) {
      col_indices <- (((i_col - 1) + hash_tracked_offsets) %% n_col) + 1
      res[[i_col]] <- vectorized_hash_row(df[col_indices], algo = "xxh32")[1:BYTES_PER_TRACKED_HASH, ] # MSBs
    }
    res <- do.call(rbind, res)
    return(res)
  }

  read_string_from_con <- function(con) {
    res <- NULL
    n <- readBin(con, integer(), 1L)
    if (length(n) > 0) res <- readBin(con, raw(), n) |> rawToChar()
    return(res)
  }

  read_character_vector_from_con <- function(con) {
    res <- character(0)
    n <- readBin(con, integer(), 1L)
    for (i in seq_len(n)) res <- c(res, read_string_from_con(con))
    return(res)
  }

  read_integer_vector_from_con <- function(con) {
    res <- integer(0)
    n <- readBin(con, integer(), 1L, endian = "little")
    res <- readBin(con, integer(), n, endian = "little")
    return(res)
  }

  read_hashes_from_con <- function(con, hash_count, hash_length) {
    expected_hash_byte_count <- hash_count * hash_length
    hash_vector <- readBin(con, raw(), expected_hash_byte_count)
    ; if (length(hash_vector) < expected_hash_byte_count) return(simpleCondition("Not enough hash data"))
    res <- array(hash_vector, dim = c(hash_length, hash_count))
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
      hash_id = hash_id,
      hash_tracked = hash_tracked,
      ..ref = list(
        hash_id = ..ref_hash_id,
        hash_tracked = ..ref_hash_tracked
      ),
      read_string_from_con = read_string_from_con,
      read_character_vector_from_con = read_character_vector_from_con,
      read_integer_vector_from_con = read_integer_vector_from_con,
      read_hashes_from_con = read_hashes_from_con
    )
  )
})

# _R_eview _S_tructures
RS_hash_data_frame <- function(df) {
  # Strip arguments from unnecessary detail
  attributes(df) <- list(class = "data.frame", names = names(df), row.names = seq_len(nrow(df)))
  
  res <- xxhashlite::xxhash_raw(
    vec = serialize(object = df, connection = NULL, ascii = FALSE, xdr = FALSE, version = 3), 
    algo = "xxh128", as_raw = TRUE
  )
  return(res)
}

RS_variable_type_encoding <- list(
  # The order of this list matters, because e.g. POSIXct variables are also numeric
  # They go from most to least restrictive
  list(code = 1,  desc = "Date",      fn = function(v) inherits(v, "Date")),
  list(code = 2,  desc = "POSIXct",   fn = function(v) inherits(v, "POSIXct")),
  list(code = 3,  desc = "POSIXlt",   fn = function(v) inherits(v, "POSIXlt")),
  list(code = 10, desc = "logical",   fn = is.logical),
  list(code = 11, desc = "factor",    fn = is.factor),
  list(code = 13, desc = "integer",   fn = is.integer),
  list(code = 14, desc = "numeric",   fn = is.numeric),
  list(code = 15, desc = "complex",   fn = is.complex),
  list(code = 16, desc = "character", fn = is.character),
  list(code = 24, desc = "raw",       fn = is.raw)
)
RS_variable_type_desc_from_code <- local({
  res <- character()
  for (elem in RS_variable_type_encoding) res[[elem[["code"]]]] <- elem[["desc"]]
  return(res)
})

RS_compute_data_frame_variable_types <- function(df, vars) {
  res <- raw(length(vars))
  for (i_var in seq_along(vars)){
    v <- 0
    var <- df[[vars[[i_var]]]]
    for (encoding in RS_variable_type_encoding){
      if (encoding[["fn"]](var)) {
        v <- encoding[["code"]]
        break
      }
    }
    checkmate::assert_true(v != 0)
    res[[i_var]] <- as.raw(v)
  }
  return(res)
}

RS_parse_data_frame_variable_types <- function(v) {
  res <- RS_variable_type_desc_from_code[as.integer(v)]
  res[is.na(res)] <- "unknown"
  return(res)
}

RS_compute_id_hashes <- function(df, id_vars) {
  return(SH$hash_id(df[id_vars]))
}

RS_compute_base_memory <- function(df_id, df, id_vars, tracked_vars) {
  checkmate::assert_string(df_id, min.chars = 1, max.chars = 65535)
  checkmate::assert_data_frame(df)
  # TODO: assert *_vars char unique col names of df
 
  id_vars <- sort(id_vars)
  tracked_vars <- sort(tracked_vars)
  
  df_hash <- RS_hash_data_frame(df)

  # row hashes
  id_hashes <- RS_compute_id_hashes(df, id_vars)
  ; if (!identical(dim(id_hashes), c(16L, nrow(df)))) return(simpleCondition("Internal error in id_vars hash preparation"))
  ; if (any(duplicated(id_hashes, MARGIN = 2))) return(simpleCondition("Found duplicated IDs"))

  tracked_hashes <- SH$hash_tracked(df[tracked_vars])
  ; if (!identical(dim(tracked_hashes), c(BYTES_PER_TRACKED_HASH * length(tracked_vars), nrow(df)))) 
    return(simpleCondition("Internal error in tracked_vars hash preparation"))
  
  # NOTE: We choose a serialization scheme with a well-known encoding. This avoid security concerns over 
  #       deserialization (https://aitap.github.io/2024/05/02/unserialize.html) and leaves the code open
  #       to alternative lightweight C implementations in case users require faster processing.
  #       It also allows us to create pure append-only files, which make it easy to avoid catastrophic
  #       loss of data in case something goes wrong, as we can look for the last correct byte on any given
  #       file, discard the remainder bytes and end up in a consistent state.
  res <- c(
    charToRaw("LISTBASE"),                                          # file magic code
    as.raw(0),                                                      # format version number
    as.raw(0),                                                      # generation marker
    SH$double_to_raw(SH$get_UTC_time_in_seconds()),                 # timestamp
    df_hash,                                                        # complete hash of input data.frame
    SH$string_to_raw(df_id),                                        # domain string
    SH$character_vector_to_raw(id_vars),                            # identifier vars (names)
    as.raw(RS_compute_data_frame_variable_types(df, id_vars)),      # identifier vars (types)
    SH$character_vector_to_raw(tracked_vars),                       # tracked vars (names)
    as.raw(RS_compute_data_frame_variable_types(df, tracked_vars)), # tracked vars (types)
    SH$integer_to_raw(nrow(df)),                                    # row count
    id_hashes,                                                      # one hash of id_vars per row
    tracked_hashes                                                  # one hash of tracked_vars per row
  )
  
  return(res)
}

RS_parse_base <- function(contents) {
  con <- rawConnection(contents, open = "r")
  file_magic_code <- readBin(con, raw(), 8L) |> rawToChar()
  ; if (!identical(file_magic_code, "LISTBASE")) return(simpleCondition("Wrong magic code"))
  format_version_number <- readBin(con, raw(), 1L)
  ; if (!identical(format_version_number, as.raw(0))) return(simpleCondition("Wrong format version number"))
  generation <- as.integer(readBin(con, raw(), 1L))
  ; if (!identical(generation, 0L)) return(simpleCondition("Wrong generation marker. Should be 0"))
  timestamp <- readBin(con, numeric(), 1L)
  time_delta <- SH$get_UTC_time_in_seconds() - timestamp
  ; if (time_delta < 0) return(simpleCondition("Timestamp is set in the future"))
  ; if (time_delta > 2**31) return(simpleCondition("Time delta is set too far in the past"))
  contents_hash <- readBin(con, raw(), 16L)
  domain_string <- SH$read_string_from_con(con)
  id_vars <- SH$read_character_vector_from_con(con)
  id_var_types <- readBin(con, raw(), length(id_vars))
  tracked_vars <- SH$read_character_vector_from_con(con)
  tracked_var_types <- readBin(con, raw(), length(tracked_vars))
  row_count <- readBin(con, integer(), 1L)
 
  id_hashes <- SH$read_hashes_from_con(con, row_count, 16L)
  tracked_hashes <- SH$read_hashes_from_con(con, row_count, BYTES_PER_TRACKED_HASH * length(tracked_vars))
  
  empty_read <- readBin(con, raw(), 1L)
  ; if (length(empty_read) > 0) return(simpleCondition("Too much hash data"))
  close(con)
  
  res <- list(
    domain = domain_string,
    generation = generation,
    id_vars = id_vars,
    id_var_types = id_var_types,
    tracked_vars = tracked_vars,
    tracked_var_types = tracked_var_types,
    contents_hash = contents_hash,
    row_count = row_count,
    timestamp = timestamp,
    id_hashes = id_hashes,
    tracked_hashes = tracked_hashes,
    row_timestamps = rep(timestamp, row_count)
  )
  
  return(res)
}

RS_compute_delta_memory <- function(state, df) {
  checkmate::assert_data_frame(df) # TODO: etc.
  
  error <- character(0)
  
  time_delta <- as.integer(ceiling(SH$get_UTC_time_in_seconds() - state$timestamp))
  df_hash <- RS_hash_data_frame(df)

  id_vars <- state$id_vars
  id_hashes <- RS_compute_id_hashes(df, id_vars) |> c() |> array(dim = c(16L, nrow(df)))

  tracked_vars <- state$tracked_vars
  # FIXME: (LUIS): Ask Miguel about the postlude
  tracked_hashes <- (SH$hash_tracked(df[tracked_vars]) |> c() |> 
                       array(dim = c(BYTES_PER_TRACKED_HASH * length(tracked_vars), nrow(df))))

  # Assert against removal of rows
  local({
    merged <- cbind(id_hashes, state$id_hashes, deparse.level = 0)
    dropped_row_mask <- !duplicated(merged, MARGIN = 2) |> c() |> tail(n = ncol(state$id_hashes))
    dropped_row_count <- sum(dropped_row_mask)
    if (dropped_row_count > 0) {
      error <<- c(error, sprintf("Dataset update is missing %s previously known row(s).\n", dropped_row_count))
    }
  })
  
  merged <- cbind(state$id_hashes, id_hashes, deparse.level = 0)
  new_row_mask <- !duplicated(merged, MARGIN = 2) |> c() |> tail(n = nrow(df))
  new_row_indices <- which(new_row_mask)
  
  new_id_hashes <- id_hashes[, new_row_indices, drop = FALSE]
  new_tracked_hashes <- tracked_hashes[, new_row_indices, drop = FALSE]

  # drop new rows
  if (length(new_row_indices)) {
    id_hashes <- id_hashes[, -new_row_indices, drop = FALSE]
    tracked_hashes <- tracked_hashes[, -new_row_indices, drop = FALSE]
  }
 
  # Sort remaining according to state$id_hashes order # TODO: Streamline for performance?
  mapping <- match(asplit(id_hashes, 2), asplit(state$id_hashes, 2))
  # FIXME(miguel): Ask Luis for details. Suggest the use of `error` to shortcircuit file actions for the case described.
  # TODO: This mapping may fail when data updates are messed. Dataset is updated, new deltas are calculated, and the
  # outdated is loaded in the app again. It does not fail gracefully, mapping contains more entries than expected and
  # an out of bounds error is thrown.
  id_hashes <- id_hashes[, mapping, drop = FALSE]
  tracked_hashes <- tracked_hashes[, mapping, drop = FALSE]

  merged <- cbind(state$tracked_hashes, tracked_hashes, deparse.level = 0)
  modified_row_mask <- !duplicated(merged, MARGIN = 2) |> c() |> tail(n = nrow(df) - length(new_row_indices))
  modified_row_indices <- which(modified_row_mask)
  
  res <- list(
    contents = c(
      charToRaw("LISTDELT"),                                # file magic code
      as.raw(0),                                            # format version number
      as.raw(state$generation + 1L),                        # generation marker
      SH$integer_to_raw(time_delta),                        # delta timestamp (seconds since state)
      df_hash,                                              # complete hash of input data.frame
      SH$string_to_raw(state$domain),                       # domain string
      SH$integer_to_raw(length(new_row_indices)),           # count of new rows
      new_id_hashes,                                        # one hash of id_vars per new row
      new_tracked_hashes,                                   # one hash of tracked_vars per new row
      SH$integer_vector_to_raw(modified_row_indices),       # modified row indices
      tracked_hashes[, modified_row_indices, drop = FALSE]  # one hash of tracked_vars per modified row
    ),
    error = error
  )
  
  return(res)
}

RS_parse_delta <- function(contents, tracked_var_count) {
  con <- rawConnection(contents, open = "r")
  file_magic_code <- readBin(con, raw(), 8L) |> rawToChar()
  ; if (!identical(file_magic_code, "LISTDELT")) return(simpleCondition("Wrong magic code"))
  format_version_number <- readBin(con, raw(), 1L)
  ; if (!identical(format_version_number, as.raw(0))) return(simpleCondition("Wrong format version number"))
  generation <- as.integer(readBin(con, raw(), 1L))
  time_delta <- readBin(con, integer(), 1L)
  ; if (time_delta < 0) return(simpleCondition("Negative time delta"))
  contents_hash <- readBin(con, raw(), 16L)
  domain <- SH$read_string_from_con(con)
  new_row_count <- readBin(con, integer(), 1L)
  new_id_hashes <- SH$read_hashes_from_con(con, new_row_count, 16L)
  new_tracked_hashes <- SH$read_hashes_from_con(con, new_row_count, BYTES_PER_TRACKED_HASH * tracked_var_count)
  modified_row_count <- NA_integer_
  modified_row_indices <- SH$read_integer_vector_from_con(con)
  modified_row_count <- length(modified_row_indices)
  modified_tracked_hashes <- SH$read_hashes_from_con(con, modified_row_count, BYTES_PER_TRACKED_HASH * tracked_var_count)

  empty_read <- readBin(con, raw(), 1L)
  ; if (length(empty_read) > 0) return(simpleCondition("Too much hash data"))
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

RS_compute_review_codes_memory <- function(review_codes) {
  checkmate::assert_character(review_codes, min.chars = 1)
  
  pcodes <- raw(0)
  for (code in review_codes) pcodes <- c(pcodes, SH$string_to_raw(code))
  
  res <- c(
    charToRaw("LISTCODE"), # file magic code
    as.raw(0),             # format version number
    pcodes                 # codes WITHOUT an integer saying how many of them there are (allows extending codes)
  )
  
  return(res)
}

RS_parse_review_codes <- function(contents) {
  con <- rawConnection(contents, open = "r")
  file_magic_code <- readBin(con, raw(), 8L) |> rawToChar()
  ; if (!identical(file_magic_code, "LISTCODE")) return(simpleCondition("Wrong magic code"))
  format_version_number <- readBin(con, raw(), 1L)
  ; if (!identical(format_version_number, as.raw(0))) return(simpleCondition("Wrong format version number"))
  
  res <- character(0)
  code <- SH$read_string_from_con(con) 
  while (!is.null(code)) {
    res <- c(res, code)
    code <- SH$read_string_from_con(con) 
  }
  
  close(con)

  return(res)
}

RS_compute_review_reviews_memory <- function(role, dataset) {
  checkmate::assert_string(role, min.chars = 1)
  
  prole <- SH$string_to_raw(role)
  
  res <- c(
    charToRaw("LISTREVI"),     # file magic code
    as.raw(0),                 # format version number
    prole,                     # role string
    SH$string_to_raw(dataset)  # domain
  )
  
  return(res)
}

RS_parse_review_reviews <- function(contents, dataset_to_state_row_mapping, expected_role, expected_domain) {
  row_count <- length(dataset_to_state_row_mapping)
  res <- data.frame(review = rep(0L, row_count), timestamp = rep(0., row_count))
  
  con <- rawConnection(contents, open = "r")
  file_magic_code <- readBin(con, raw(), 8L) |> rawToChar()
  ; if (!identical(file_magic_code, "LISTREVI")) return(simpleCondition("Wrong magic code"))
  format_version_number <- readBin(con, raw(), 1L)
  ; if (!identical(format_version_number, as.raw(0))) return(simpleCondition("Wrong format version number"))
  
  role <- SH$read_string_from_con(con)
  ; if (!identical(role, expected_role)) return(simpleCondition(sprintf("Expected role `%s`", expected_role)))
  domain <- SH$read_string_from_con(con)
  ; if (!identical(domain, expected_domain)) return(simpleCondition(sprintf("Expected domain `%s`", expected_domain)))
  
  f_cur <- seek(con, where = 0, origin = "end", rw = "read")
  f_end <- seek(con, where = f_cur, origin = "start", rw = "read")
  
  record_count <- (f_end - f_cur) / (4 + 4 + 8) # sizes correspond to integer+integer+numeric
  for (i in seq_len(record_count)){
    row_index <- readBin(con, integer(), 1L, endian = "little")
    review <- readBin(con, integer(), 1L, endian = "little")
    timestamp <- readBin(con, numeric(), 1L, endian = "little")
    # NOTE: timestamp increases monotonically with each new row, so not checking it
    row_index <- dataset_to_state_row_mapping[[row_index]]
    res[["review"]][[row_index]] <- review
    res[["timestamp"]][[row_index]] <- timestamp
  }
  
  close(con)
  return(res)
}

RS_load <- function(base, deltas) {
  res <- RS_parse_base(base) 
  base_timestamp <- res$timestamp
  for (delta in deltas){
    state_delta <- RS_parse_delta(contents = delta, tracked_var_count = length(res[["tracked_vars"]]))
    if (inherits(state_delta, "simpleCondition")) return(state_delta)
    
    if (!identical(state_delta$generation, res$generation + 1L))
      return(simpleCondition(paste("Wrong generation marker. Should be", res$generation + 1L)))
    
    if (!identical(state_delta$domain, res$domain))
      return(simpleCondition(paste("Wrong domain. Expected", res$domain, "and got", state_delta$domain)))
   
    res$contents_hash <- state_delta$contents_hash
    res$generation <- state_delta$generation
    # new rows
    res$row_count <- res$row_count + state_delta$new_row_count
    res$id_hashes <- cbind(res$id_hashes, state_delta$new_id_hashes)
    res$tracked_hashes <- cbind(res$tracked_hashes, state_delta$new_tracked_hashes)
    res$row_timestamps <- c(res$row_timestamps, rep(base_timestamp + state_delta$time_delta, state_delta$new_row_count))
    
    # modified rows
    res$tracked_hashes[, state_delta$modified_row_indices] <- state_delta$modified_tracked_hashes
    res$row_timestamps[state_delta$modified_row_indices] <- base_timestamp + state_delta$time_delta
  }
  return(res)
}

RS_append <- function(path, contents) {
  # TODO: Copy file, append to copy, rename back
  f <- file(path, open = "r+b", raw = TRUE)
  seek(f, where = 0, origin = "end", rw = "write")
  writeBin(contents, f)
  close(f)
}

# NOTE: The contents of the following conditional are a WIP of the review feature.
#       They may be useful for performance testing, but maybe a higher-level approach,
#       such as that on `tests/testthat/test-review.R`, is enough and we don't need
#       to call the `RS_*` functions directly.
if (FALSE) {
  describe_and_time <- function(description, expr) {
    t0 <- Sys.time()
    res <- force(expr)
    t1 <- Sys.time()
    cat(sprintf("%.2f s: %s\n", t1 - t0, description))
    if (inherits(res, "condition")) stop(res)
    return(res)
  }
 
  if (FALSE) {
    df <- safetyData::adam_adae[1:2, ]
    base_contents <- describe_and_time(
      "Derive .base file contents from initial dataset (expensive; only done the first time app is run by some user)", {
        df_id <- "ae"
        id_vars <- c("USUBJID", "AESEQ")
        tracked_vars <- setdiff(names(df), "id_vars") # tracks all non-id vars (worst case for performance)
        RS_compute_base_memory(df_id, df, id_vars, tracked_vars)
      }
    )
    
    review_state <- describe_and_time(
      "Initial load (base and no deltas)",
      RS_load(base_contents, deltas = list())
    )
    
    df <- safetyData::adam_adae[2:1, ] # Reverse the order of rows of the dataset
    
    delta1_contents <- describe_and_time(
      "Processing dataset update (done first time app is run after update)",
      RS_compute_delta_memory(review_state, df)[["contents"]]
    )
    
    review_state <- describe_and_time(
      "Initial load (base and one delta)",
      RS_load(base_contents, deltas = list(delta1_contents)) 
    )
    
    df[1, ][["AESEV"]] <- "SEVERE"
    delta2_contents <- describe_and_time(
      "Processing dataset update (done first time app is run after update)",
      RS_compute_delta_memory(review_state, df)[["contents"]]
    )
  

    browser()
  }
  

  # Build an arbitrarily large input dataset
  df <- local({
    df <- safetyData::adam_adae[1:1000, ]
    df <- rbind(df, df, df, df, df, df, df, df, df, df) # 10k # nolint
    # df <- rbind(df, df, df, df, df, df, df, df, df, df) # 100k # nolint
    # df <- rbind(df, df, df, df, df, df, df, df, df, df) # 1000k # nolint
    
    for (i_row_block in seq_len(nrow(df) / 1000)){
      fake_study_prefix <- sprintf("%05d", i_row_block)
      df[["USUBJID"]][seq((i_row_block - 1) * 1000 + 1, i_row_block * 1000)] <- 
        sub("^01", fake_study_prefix, df[["USUBJID"]][seq((i_row_block - 1) * 1000 + 1, i_row_block * 1000)])
    }
    return(df)
  })
 
  base_contents <- describe_and_time(
    "Derive .base file contents from initial dataset (expensive; only done the first time app is run by some user)", {
      df_id <- "ae"
      id_vars <- c("USUBJID", "AESEQ")
      tracked_vars <- setdiff(names(df), "id_vars") # tracks all non-id vars (worst case for performance)
      RS_compute_base_memory(df_id, df, id_vars, tracked_vars)
    }
  )
  
  # TODO: Write to disk
  # TODO: Read from disk

  review_codes_raw <- RS_compute_review_codes_memory(
    c("Seen", "Should look into", "Looked into", "We don't seem to agree", "Clearly artifactual")
  )
  
  if (FALSE) {
    review_codes <- RS_parse_review_codes(review_codes_raw)
    
    browser() # TODO: Maybe RS_compute_role_review_memory + RS_parse_role_review
    
    review_roles <- c("TSTAT", "SP", "Safety", "CTL")
    
    for (role in review_roles) {
      browser()
      
    }
  }

  review_state <- describe_and_time(
    "Initial load (base and no deltas)",
    RS_load(base_contents, deltas = list())
  )

  # Let's imagine there is an update that adds a new entry to the dataset
  df <- rbind(df, safetyData::adam_adae[1001, ])
  # Furthermore, the severity of an adverse event is described now as "MODERATE" instead of "MILD"
  df[500, "AESEV"] <- "MODERATE"
  
  delta1_contents <- describe_and_time(
    "Processing dataset update (done first time app is run after update)",
    RS_compute_delta_memory(review_state, df)[["contents"]]
  )
  
  review_state <- describe_and_time(
    "Initial load (base and one delta)",
    RS_load(base_contents, deltas = list(delta1_contents)) 
  )
  
  # The severity of an adverse event is described now as "SEVERE" instead of "MODERATE"
  df[500, "AESEV"] <- "SEVERE"
  # and another one that was "MILD" is now "MODERATE"
  df[501, "AESEV"] <- "SEVERE"
  # And we also _prepend_ two new entries to the dataset
  df <- rbind(safetyData::adam_adae[1002:1003, ], df)
  delta2_contents <- describe_and_time(
    "Processing dataset update (done first time app is run after update)",
    RS_compute_delta_memory(review_state, df)[["contents"]]
  )
  
  review_state <- describe_and_time(
    "Initial load (base and two deltas)",
    RS_load(base_contents, deltas = list(delta1_contents, delta2_contents)) 
  )
  
  browser() # TODO: Start the application based on base and deltas (RS_parse_base_and_deltas)
}
