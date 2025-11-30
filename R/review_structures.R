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
    # NOTE: This can't be done without versioning the file format so that old trials
    #       can still use this old function and new trials use the new one.
    #       It makes sense to postpone this improvement until we have a better reason
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
    res <- matrix(unlist(vectorized_hash_id(single_col)) %||% raw(0), nrow = n_row, ncol = n_col)  
    res
  }

  hash_id <- vectorized_hash_row

  hash_tracked <- function(df) {
    n_col <- ncol(df)
    res <- list()   
    for (i_col in seq_len(n_col)) {
      col_indices <- (((i_col - 1) + hash_tracked_offsets) %% n_col) + 1
      res[[i_col]] <- vectorized_hash_row(df[col_indices], algo = "xxh32")[1:BYTES_PER_TRACKED_HASH, , drop = FALSE] # MSBs
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

  ; if (nrow(df) == 0) return(simpleCondition("Refusing to review 0-row dataset"))
 
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
    as.raw(0),                                                      # generation marker (wraps around after 255)
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

RS_parse_base <- function(contents, only_header = FALSE) {
  con <- rawConnection(contents, open = "r")
  on.exit(close(con))
  
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

  id_hashes <- tracked_hashes <- row_timestamps <- NULL 
  if(!isTRUE(only_header)){
    id_hashes <- SH$read_hashes_from_con(con, row_count, 16L)
    tracked_hashes <- SH$read_hashes_from_con(con, row_count, BYTES_PER_TRACKED_HASH * length(tracked_vars))
    row_timestamps <- rep(timestamp, row_count)
    empty_read <- readBin(con, raw(), 1L)
    ; if (length(empty_read) > 0) return(simpleCondition("Too much hash data"))
  }
  
  res <- list(
    # HEADER
    domain = domain_string,
    generation = generation,
    id_vars = id_vars,
    id_var_types = id_var_types,
    tracked_vars = tracked_vars,
    tracked_var_types = tracked_var_types,
    contents_hash = contents_hash,
    row_count = row_count,
    timestamp = timestamp,
    # CONTENTS
    id_hashes = id_hashes,
    tracked_hashes = tracked_hashes,
    row_timestamps = row_timestamps
  )
  
  return(res)
}

RS_compute_delta_memory <- function(state, df) {
  # NOTE:
  # The logic of this function is a bit tricky to grasp. Our recommendation is that you start from the contents of the 
  # `res` variable (whose structure is described in the Data Review vignette, "Structured of stored files" section) and
  # work backwards.
  # 
  # If you want to run this function with a trivial example, look inside `tests/testhat/test-review.R` and you'll find
  # a test described as "RS_compute_delta_memory identifies new and modified rows", whose sole purpose is to trace this
  # function.
  checkmate::assert_data_frame(df)

  error <- character(0)
  
  # Glossary of variable suffixes:
  # =============================
  # _st: coming from or relative to `state`
  # _df: coming from or relative to `df` 
  # _new: new rows (not present in state)
  # _old: old rows (present in state)

  time_delta <- as.integer(ceiling(SH$get_UTC_time_in_seconds() - state$timestamp))
  hash_df <- RS_hash_data_frame(df)

  id_hashes_df <- RS_compute_id_hashes(df, state$id_vars) |> c() |> array(dim = c(16L, nrow(df)))
  tracked_hashes_df <- (SH$hash_tracked(df[state$tracked_vars]) |> c() |> 
                           array(dim = c(BYTES_PER_TRACKED_HASH * length(state$tracked_vars), nrow(df))))
  
  indices_new_df <- local({
    # NOTE: `state$id_hashes` and `id_hashes_df` are matrices of 16-byte long hashes (dim() returns "16 n" and "16 m")
    #       By column-binding them we create a longer matrix of dimensions "16 n+m"
    #       Looking for duplicates in the second axis, taking only the last "m" elements and negating the output
    #       tells us which elements of the new dataframe are _not_ present in the old one.
    merged_id_hashes <- cbind(state$id_hashes, id_hashes_df, deparse.level = 0)
    mask_new_df <- !duplicated(merged_id_hashes, MARGIN = 2) |> as.logical() |> utils::tail(n = ncol(id_hashes_df))
    return(which(mask_new_df))
  })
  
  id_hashes_df_new <- id_hashes_df[, indices_new_df, drop = FALSE]
  tracked_hashes_df_new <- tracked_hashes_df[, indices_new_df, drop = FALSE]

  id_hashes_df_old <- id_hashes_df
  tracked_hashes_df_old <- tracked_hashes_df
  if (length(indices_new_df)) {
    id_hashes_df_old <- id_hashes_df_old[, -indices_new_df, drop = FALSE]
    tracked_hashes_df_old <- tracked_hashes_df_old[, -indices_new_df, drop = FALSE]
  }
 
  # Build an index that projects repeat IDs from new `_df` into canonical `_st` indices
  index_map_st_old <- match(asplit(id_hashes_df_old, 2), asplit(state$id_hashes, 2))
  
  tracked_hashes_st_old <- state$tracked_hashes[, index_map_st_old, drop = FALSE]
  modified_mask_df_old  <- (tracked_hashes_df_old != tracked_hashes_st_old) |> apply(any, MARGIN = 2)
  modified_indices_st_old <- index_map_st_old[modified_mask_df_old]
  
  res <- list(
    contents = c(
      charToRaw("LISTDELT"),                                       # file magic code
      as.raw(0),                                                   # format version number
      as.raw((state$generation + 1L) %% 256L),                     # generation marker (wraps around after 255)
      SH$integer_to_raw(time_delta),                               # delta timestamp (seconds since state)
      hash_df,                                                     # complete hash of input data.frame
      SH$string_to_raw(state$domain),                              # domain string
      SH$integer_to_raw(length(indices_new_df)),                   # count of new rows
      id_hashes_df_new,                                            # one hash of id_vars per new row
      tracked_hashes_df_new,                                       # one hash of tracked_vars per new row
      SH$integer_vector_to_raw(modified_indices_st_old),           # modified row indices
      tracked_hashes_df_old[, modified_mask_df_old, drop = FALSE]  # one hash of tracked_vars per modified row
    ),
    error = error
  )
  
  return(res)
}

RS_parse_delta <- function(contents, tracked_var_count, only_header = FALSE) {
  con <- rawConnection(contents, open = "r")
  on.exit(close(con))
  
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
  
  new_id_hashes <- new_tracked_hashes <- modified_row_count <- modified_row_indices <- modified_tracked_hashes <- NULL
  
  if(!isTRUE(only_header)){
    new_id_hashes <- SH$read_hashes_from_con(con, new_row_count, 16L)
    new_tracked_hashes <- SH$read_hashes_from_con(con, new_row_count, BYTES_PER_TRACKED_HASH * tracked_var_count)
  
    # FIXME? It would have been better to make `modified_row_count` part of the header (before `new_id_hashes`)
    modified_row_count <- NA_integer_
    modified_row_indices <- SH$read_integer_vector_from_con(con)
    modified_row_count <- length(modified_row_indices) # TODO? Skip over indices to make this part of the header
    modified_tracked_hashes <- SH$read_hashes_from_con(con, modified_row_count, BYTES_PER_TRACKED_HASH * tracked_var_count)
    
    empty_read <- readBin(con, raw(), 1L)
    ; if (length(empty_read) > 0) return(simpleCondition("Too much hash data"))
  } else {
    # TODO: Skip over new hashes and return the modified_row_count
  }

  res <- list(
    # HEADER
    domain = domain,
    generation = generation,
    contents_hash = contents_hash,
    new_row_count = new_row_count,
    
    # CONTENTS
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
    as.raw(1),                 # format version number
    prole,                     # role string
    SH$string_to_raw(dataset)  # domain
  )
  
  return(res)
}


RS_parse_review_reviews_and_apply_undo <- function(contents, expected_role, expected_domain) {
  con <- rawConnection(contents, open = "r")
  on.exit(close(con))
  
  file_magic_code <- readBin(con, raw(), 8L) |> rawToChar()
  ; if (!identical(file_magic_code, "LISTREVI")) return(simpleCondition("Wrong magic code"))
  format_version_number <- as.integer(readBin(con, raw(), 1L))
  ; if (format_version_number > 1) # Version 1 introduces review undo actions and is backwards compatible with v0
    return(simpleCondition("Wrong format version number"))
  
  role <- SH$read_string_from_con(con)
  ; if (!identical(role, expected_role)) return(simpleCondition(sprintf("Expected role `%s`", expected_role)))
  domain <- SH$read_string_from_con(con)
  ; if (!identical(domain, expected_domain)) return(simpleCondition(sprintf("Expected domain `%s`", expected_domain)))
  
  f_after_header <- seek(con, where = 0L, origin = "end", rw = "read")
  f_end <- seek(con, where = f_after_header, origin = "start", rw = "read")
  
  integer_size <- 4L
  numeric_size <- 8L
  record_count <- (f_end - f_after_header) / (integer_size + integer_size + numeric_size)
  remainder <- (f_end - f_after_header) %% (integer_size + integer_size + numeric_size)
  ; if (remainder != 0) return(simpleCondition(sprintf("Wrong `.review` file size")))
 
  # Interpret the whole file as an array of 32-bit integers and extract all canonical and review indices
  integers <- readBin(con, integer(), record_count * 4L, endian = "little")
  dim(integers) <- c(4L, record_count)
  canonical_indices <- integers[1, ]
  review_indices <- integers[2, ]
  
  # Interpret the whole file as an array of 64-bit real values and extract all timestamps
  seek(con, where = f_after_header, origin = "start", rw = "read")
  doubles <- readBin(con, numeric(), record_count * 2L, endian = "little")
  dim(doubles) <- c(2L, record_count)
  timestamps <- doubles[2, ]
 
  # Review action undo. Introduced in version 1 of the LISTREVI file format
  undo_indices <- which(canonical_indices == 0L)
  if(length(undo_indices)){
    ; if(length(canonical_indices) %in% undo_indices) # No space left for second half of 32-bit-wide undo action
      return(error_cond <- simpleCondition("Invalid encoding of undo action in `.review` file"))
    
    indices_to_remove <- integer(0)
    for(i in undo_indices) {
      undo_action_count <- review_indices[[i]]
      ; if(length(undo_action_count) < 1) return(simpleCondition("Undo action targets non-positive amount of records"))
      
      undo_action_timestamp <- timestamps[[i]]
      
      target_canonical_row_index <- -canonical_indices[[i+1]]
      target_review_index <- review_indices[[i+1]]
      target_timestamp <- timestamps[[i+1]]
      ; if(undo_action_timestamp < target_timestamp) 
        return(simpleCondition("Undo action timestamp lower than that of target action"))
      
      first_index_to_undo <- which(target_canonical_row_index == canonical_indices & 
                                     target_review_index == review_indices & 
                                     target_timestamp == timestamps)
      ; if(length(first_index_to_undo) != 1) return(simpleCondition("Undo action found no target action"))
      
      indices_to_undo <- seq_len(undo_action_count)-1 + first_index_to_undo
      ; if(any(indices_to_undo > i)) return(simpleCondition("Undo action precedes target action"))
      
      indices_to_remove <- c(indices_to_remove, indices_to_undo, i, i+1)
    }
    
    canonical_indices <- canonical_indices[-c(indices_to_remove, i, i+1)]
    review_indices <- review_indices[-c(indices_to_remove, i, i+1)]
    timestamps <- timestamps[-c(indices_to_remove, i, i+1)]
  }
  
  return(
    list(
      format_version_number = format_version_number,
      canonical_indices = canonical_indices,
      review_indices = review_indices,
      timestamps = timestamps
    )
  )
}

RS_parse_review_reviews <- function(contents, row_count, expected_role, expected_domain) {
  internal_res <- RS_parse_review_reviews_and_apply_undo(contents, expected_role, expected_domain)
  format_version_number = internal_res[["format_version_number"]]
  canonical_indices <- internal_res[["canonical_indices"]]
  review_indices <- internal_res[["review_indices"]]
  timestamps <- internal_res[["timestamps"]]
    
  ; if (!(all(0 < canonical_indices & canonical_indices <= row_count)))
    return(simpleCondition("Invalid `.review` canonical indices"))
  
  ; if (any(diff(timestamps) < 0)) return(simpleCondition("Non-monotonically increasing `.review` timestamps"))
  
  res_reviews <- rep(0L, row_count)
  res_timestamps <- rep(0., row_count)
  res_reviews[canonical_indices] <- review_indices
  res_timestamps[canonical_indices] <- timestamps
  
  res <- list(
    format_version_number = format_version_number,
    data = data.frame(review = res_reviews, timestamp = res_timestamps)
  )
  
  return(res)
}

RS_load <- function(base, deltas) {
  res <- RS_parse_base(base) 
  base_timestamp <- res$timestamp
  res$revisions <- list(timestamps = res$timestamp, tracked_hashes = list(res$tracked_hashes))
  for (i_delta in seq_along(deltas)){
    state_delta <- RS_parse_delta(contents = deltas[[i_delta]], tracked_var_count = length(res[["tracked_vars"]]))
    if (inherits(state_delta, "simpleCondition")) return(state_delta)
    
    # NOTE: Check the vignette if this `256` confuses you :)
    if (!identical(state_delta$generation, (res$generation + 1L) %% 256L))
      return(simpleCondition(paste("Wrong generation marker. Should be", (res$generation + 1L) %% 256L)))
    
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
    
    # collect all tracked_hashes revisions to allow change attribution to specific columns
    res$revisions$timestamps <- c(res$revisions$timestamps, base_timestamp + state_delta$time_delta)
    res$revisions$tracked_hashes[[i_delta + 1]] <- res$tracked_hashes
  }
  
  # extend all revision hashes with dummy tracked_hashes for rows not present at a particular timestamp
  if (length(deltas)) {
    last_known_col_count <- ncol(res$revisions$tracked_hashes[[length(deltas) + 1]])
    for (i_revision in seq_len(length(deltas))){
      revision_row_count <- ncol(res$revisions$tracked_hashes[[i_revision]])
      if (revision_row_count < last_known_col_count) {
        missing_col_count <- last_known_col_count - revision_row_count
        row_count <- nrow(res$revisions$tracked_hashes[[i_revision]])
        extra_cols <- matrix(raw(0), row_count, missing_col_count)
        res$revisions$tracked_hashes[[i_revision]] <- cbind(res$revisions$tracked_hashes[[i_revision]], extra_cols)
      } else if (last_known_col_count < revision_row_count) {
        return( # The diagnostic message could be improved, but we don't expect this one to trigger
          simpleCondition(sprintf(
            "Integrity error: Revision %d contains %d more rows than latest revision", 
            i_revision, revision_row_count - last_known_col_count)
          )
        )
      }
    }
  }
  
  return(res)
}
