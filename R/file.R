FS <- pack_of_constants(
  WRITE_OFFSET_APPEND = -1L,
  EMPTY_LISTING = data.frame(isdir = logical(0), size = integer(0), mtime = as.POSIXct(numeric(0)))
)

# Web browser File System Access abstraction.
# Client-side coarse-grained file system API. The server sends file system commands as custom messages to the browser.
# The browser uses the (Chrome-specific, experimental) File System Access API 
#          (Intro: https://developer.chrome.com/docs/capabilities/web-apis/file-system-access,
#           Spec: https://wicg.github.io/file-system-access/)
# to perform the requested actions. It reports the results back through pre-allocated shiny inputs.
fsa_init <- function(input, input_id, session = shiny::getDefaultReactiveDomain()) {
  checkmate::assert_string(input_id, min.chars = 1)

  state <- list(
    path = character(0),
    listing = FS$EMPTY_LISTING,
    contents = new.env(parent = emptyenv()),
    error = "Not listed yet"
  ) |> list2env(parent = emptyenv())
  
  ns <- session[["ns"]]

  list_id <- paste0(input_id, "_list")
  read_id <- paste0(input_id, "_read")
  execute_IO_plan_id <- paste0(input_id, "_execute_IO_plan")

  callbacks <- list(
    list = function(v) NULL
  )
  latest_IO_plan <- list()

  callback_count <- 0L

  .list <- function(callback = function(v) NULL) {
    callbacks[["list"]] <<- callback
    session$sendCustomMessage("dv_fsa_list", list(status_input_id = ns(list_id)))
  }
  shiny::observeEvent(input[[list_id]], {
    callback_count <<- callback_count + 1L
    on.exit(callbacks[["list"]](callback_count))

    state[["error"]] <- character(0)

    v <- input[[list_id]]
    if (is.character(v[["error"]])) {
      state[["error"]] <- v[["error"]][[1]]
    } else {
      state[["path"]] <- v[["path"]]
      
      isdir <- sapply(v[["list"]], function(x) x[["kind"]] == "directory")
      size <- sapply(v[["list"]], function(x) x[["size"]] %||% 0L)
      mtime <- sapply(v[["list"]], function(x) x[["time"]] %||% 0L) |> as.POSIXct(origin = "1970-01-01", tz = "UTC")
      state[["listing"]] <- data.frame(size, isdir, mtime)
    }
  })

  .read <- function(paths, callback = function(v) NULL) {
    if (length(state[["error"]]) > 0) {
      callback_count <<- callback_count + 1L
      callback(callback_count)
      return(NULL)
    }
    callbacks[["read"]] <<- callback
    session$sendCustomMessage("dv_fsa_read", list(status_input_id = ns(read_id), paths = paths))
  }
  shiny::observeEvent(input[[read_id]], {
    callback_count <<- callback_count + 1L
    on.exit(callbacks[["read"]](callback_count))

    v <- input[[read_id]]
    if (is.character(v[["error"]])) {
      state[["error"]] <- v[["error"]][[1]]
    } else {
      for (fname in names(v[["contents"]])){
        b64_contents <- v[["contents"]][[fname]]
        decoded_contents <- base64enc::base64decode(b64_contents)
        state[["contents"]][[fname]] <- decoded_contents
      }
    }
  })

  .execute_IO_plan <- function(IO_plan, callback = function(v) NULL) {
    if (length(state[["error"]]) > 0) {
      callback_count <<- callback_count + 1L
      callback(callback_count)
      return(NULL)
    }

    callbacks[["execute_IO_plan"]] <<- callback

    latest_IO_plan <<- IO_plan # NOTE: Used on the next observeEvent to updated the local content cache

    IO_plan_base64_encode <- function(plan) {
      encoded_plan <- plan
      for (idx in seq_along(plan)) {
        if (encoded_plan[[idx]][["kind"]] == "write") {
          encoded_plan[[idx]][["contents"]] <- base64enc::base64encode(encoded_plan[[idx]][["contents"]])
        }
      }
      return(encoded_plan)
    }
    
    IO_plan_base64 <- IO_plan_base64_encode(IO_plan)

    session$sendCustomMessage(
      "dv_fsa_execute_io_plan",
      list(status_input_id = ns(execute_IO_plan_id), plan = IO_plan_base64)
    )
  }
  shiny::observeEvent(input[[execute_IO_plan_id]], {
    callback_count <<- callback_count + 1L
    on.exit(callbacks[["execute_IO_plan"]](callback_count))
    on.exit(latest_IO_plan <<- list(), add = TRUE)
    shiny::req(length(state[["error"]]) == 0)


    v <- input[[execute_IO_plan_id]]
    if (is.character(v[["error"]])) {
      state[["error"]] <- v[["error"]][[1]]
      shiny::req(FALSE)
    }

    status <- v[["status"]]
    if (length(status) != length(latest_IO_plan)) {
      state[["error"]] <- sprintf("Error in IO plan execution. Issued %d actions. Executed %d actions instead",
                                  length(latest_IO_plan), length(status))
      shiny::req(FALSE)
    }
    
    for (i in seq_along(status)){
      if (is.character(status[[i]][["error"]])) {
        state[["error"]] <- status[[i]][["error"]][[1]]
        shiny::req(FALSE)
      }
      
      path <- latest_IO_plan[[i]][["path"]]
      contents <- latest_IO_plan[[i]][["contents"]]
      offset <- status[[i]][["offset"]] # Effective offset patched by client; important in case of WRITE_OFFSET_APPEND

      # 7 - Update local cached contents # TODO: Partially repeats #eiseil
      if (length(contents) > 0) {
        cached_contents <- state[["contents"]][[path]] %||% raw(0)
        cached_contents[(offset + 1L):(offset + length(contents))] <- contents
        state[["contents"]][[path]] <- cached_contents
      }

      # 8 - Update local cached listing # TODO: Repeats #yaisei
      fs_listing <- state[["listing"]]
      
      size <- length(contents)
      mtime <- status[[i]][["mtime"]] |> as.POSIXct(origin = "1970-01-01", tz = "UTC")
      
      listing_row <- data.frame(size = size, isdir = FALSE, mtime = mtime)
      row.names(listing_row) <- path
        
      if (path %in% rownames(fs_listing)) {
        listing_index <- which(path == rownames(fs_listing))[[1]]
        state[["listing"]][listing_index, ]  <- listing_row
      } else {
        state[["listing"]] <- rbind(fs_listing, listing_row)
      }
    }
  })

  res <- list(
    state = state,
    list = .list,
    read = .read,
    execute_IO_plan = .execute_IO_plan
  )
  return(res)
}

# Server-side File System Access abstraction.
# Pure R implementation of the `fsa_init` file system API. It allows to:
# - Test the module end to end without user intervention.
# - Provide a server-only implementation for shiny hosting platforms that offer app-specific storage.
fs_init <- function(path) {
  path <- normalizePath(path, mustWork = FALSE)
  
  state <- list(
    path = path,
    listing = FS$EMPTY_LISTING,
    contents = new.env(parent = emptyenv()),
    error = "Not listed yet"
  ) |> list2env(parent = emptyenv())
  
  fs_list_paths <- function(base_path, paths) {
    res <- file.info(file.path(base_path, paths))
    rownames(res) <- paths 
    return(res[c("isdir", "size", "mtime")])
  }
  
  callback_count <- 0L
  
  fs_list <- function(callback = function(v) NULL) {
    callback_count <<- callback_count + 1L
    on.exit(callback(callback_count))
    state[["listing"]] <- FS$EMPTY_LISTING
   
    # TODO: Remove content from entries not present instead of clearing.
    #       Not useful for this module yet, so not implemented.
    rm(list = ls(state[["contents"]], all.names = TRUE), envir = state[["contents"]])
    
    error <- character(0)
    if (!is.character(path) || length(path) != 1) {
      error <- "List error: `path` argument must be character(1)"
    } else if (!dir.exists(path)) {
      error <- sprintf("List error: Path `%s` does not exist", path) 
    } else {
      res <- try({
        file_and_dir_names <- list.files(path, all.files = TRUE, include.dirs = TRUE, recursive = TRUE, no.. = TRUE)
        state[["listing"]] <- fs_list_paths(path, file_and_dir_names)
      })
      
      if (inherits(res, "try-error")) {
        error <- sprintf("List error: %s", attr(res, "condition")[["message"]])
      }
    }
    state[["error"]] <- error
    return()
  }
 
  fs_read <- function(paths, callback = function(v) NULL) {
    callback_count <<- callback_count + 1L
    on.exit(callback(callback_count))
    if (length(state[["error"]]) > 0) return()
    
    # NOTE: Logic repeats in #thaegh
    allowed_paths <- rownames(state[["listing"]])[!state[["listing"]][["isdir"]]]
    invalid_paths <- setdiff(paths, allowed_paths)
    if (length(invalid_paths) > 0) {
      state[["error"]] <- paste(
        "Read error: Paths", paste(sprintf('"%s"', invalid_paths), collapse = ", "), "are invalid"
      )
      return()
    }
    
    for (rel_path in paths){
      index <- which(rownames(state[["listing"]]) == rel_path)
      expected_size_in_bytes <- state[["listing"]][["size"]][[index]]
      contents <- try(readBin(con = file.path(path, rel_path), what = raw(0), n = expected_size_in_bytes))
      if (inherits(contents, "try-error")) {
        state[["error"]] <- sprintf("Read error for file `%s`: %s", rel_path, attr(res, "condition")[["message"]])
        return()
      }
      
      if (length(contents) != expected_size_in_bytes) {
        state[["error"]] <- sprintf("Expected %d bytes from file `%s` and got %d instead.", expected_size_in_bytes, rel_path, 
                                    length(contents))
        return()
      }
      
      state[["contents"]][[rel_path]] <- contents
    }
    
    file_info_before <- state[["listing"]][rownames(state[["listing"]]) %in% paths, ]
    file_info_after <- file.info(file.path(path, paths))
    altered <- paths[rowSums(file_info_before[c("size", "mtime")] != file_info_after[c("size", "mtime")]) != 0]
    if (length(altered) > 0) {
      state[["error"]] <- sprintf("Files changed while reading them: %s", paste(altered, collapse = ", "))
      return()
    }
    
    return()
  }
  
  read_range <- function(contents, beg, one_past_end) {
    if (length(contents) == 0) return(contents[0])
    beg <- min(beg, length(contents))
    one_past_end <- min(one_past_end, length(contents))
    
    if (beg >= one_past_end) return(contents[0])
    
    res <- contents[beg:(one_past_end - 1L)]
    return(res)
  }
  
  fs_write <- function(path, contents, offset, callback = function(v) NULL) {
    callback_count <<- callback_count + 1L
    on.exit(callback(callback_count))
    fname_path <- file.path(state[["path"]], path)
    
    # FIXME? File offsets are zero-based and `raw` offsets are one-based. Should we make that clearer through
    #        more carefully chosen variable names?
    
    res <- try({ # NOTE: Follows the same logic as #isoaxo
      # 0 - ensure folder exists
      dname <- dirname(fname_path)
      dir.create(dname, showWarnings = FALSE, recursive = TRUE)
    
      # 1 - write to temp file in the same folder, because we rely on `file.rename` to place the file on its Final Destination
      tmp_fname <- tempfile(tmpdir = dname, fileext = ".tmp")
      
      if (file.exists(fname_path)) file.copy(fname_path, tmp_fname)
      else writeBin(con = tmp_fname, raw(0))
      
      # 2 - get file size
      con <- file(tmp_fname, open = "r+b")
      seek(con, where = 0L, origin = "end", rw = "read")
      file_size <- seek(con, where = 0L, origin = "start", rw = "read")
     
      # 3 - patch and check offset
      if (offset == FS$WRITE_OFFSET_APPEND) offset <- file_size # append without checking offset
      if (offset > file_size) stop(sprintf("Write operation to offset %d would create a hole in `%s`.", offset, path))
      
      # 4 - compare known cached contents to current contents
      cached_contents <- state[["contents"]][[path]] %||% raw(0)
      range_contents_old <- read_range(cached_contents, 1L + offset, 1L + offset + length(contents))
      
      seek(con, where = offset, origin = "start", rw = "read")
      range_contents_cur <- readBin(con = con, what = raw(0), n = length(contents))
      
      if (!identical(range_contents_cur, range_contents_old))
        stop(sprintf("Write operation to `%s` would overwrite contents of unknown origin", path))

      # 5 - write proper
      seek(con, where = offset, origin = "start", rw = "write")
      writeBin(contents, con, endian = "little")
      close(con)
      
      # 6 - overwrite target file with temp contents
      file.rename(tmp_fname, fname_path)
     
      # 7 - Update local cached contents # TODO: Repeats #eiseil
      if (length(contents) > 0) {
        cached_contents[(offset + 1L):(offset + length(contents))] <- contents
        state[["contents"]][[path]] <- cached_contents
      }
     
      # 8 - Update local cached listing # TODO: Repeats #yaisei
      fs_listing <- state[["listing"]]
      listing_row <- fs_list_paths(state[["path"]], path)
      if (path %in% rownames(fs_listing)) {
        listing_index <- which(path == rownames(fs_listing))[[1]]
        state[["listing"]][listing_index, ] <- listing_row
      } else {
        state[["listing"]] <- rbind(fs_listing, listing_row)
      }
    })
    
    if (inherits(res, "try-error")) {
      state[["error"]] <- sprintf("Write error: %s", attr(res, "condition")[["message"]])
      return()
    }
  }
  
  fs_execute_IO_plan <- function(IO_plan, callback = function(v) NULL) {
    callback_count <<- callback_count + 1L
    on.exit(callback(callback_count))
    
    first_error_message <- NULL
    for (i_command in seq_along(IO_plan)) {
      command <- IO_plan[[i_command]]
      IO_plan[[i_command]][["error"]] <- NULL
      
      if (command[["kind"]] == "write") {
        fs_write(path = command[["path"]], contents = command[["contents"]], offset = command[["offset"]])
      } else {
        first_error_message <- sprintf("Command '%s' not supported yet", command[["kind"]])
        break
      }
    }
    
    if (is.character(first_error_message)) {
      IO_plan[[i_command]][["error"]] <- first_error_message
      # we ignore commands following the one that failed, in case they depend on its correct execution
      i_command <- i_command + 1
      while (i_command <= length(IO_plan)) {
        IO_plan[[i_command]][["error"]] <- sprintf("Ignored command due to previous error")
        i_command <- i_command + 1
      }
      
      state[["error"]] <- sprintf("Error during I/O operation: %s", first_error_message)
    }
  }
   
  res <- list(
    state = state,
    list = fs_list,
    read = fs_read,
    write = fs_write,
    execute_IO_plan = fs_execute_IO_plan
  )
  
  return(res)
}
