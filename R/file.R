FS <- pack_of_constants(
  WRITE_OFFSET_APPEND = -1L,
  EMPTY_LISTING = data.frame(isdir = logical(0), size = integer(0))
)

fs_update_cached_contents <- function(state_contents_env, path, offset, contents) {
  if (length(contents) > 0) {
    cached_contents <- state_contents_env[[path]] %||% raw(0)
    cached_contents[(offset + 1L):(offset + length(contents))] <- contents
    state_contents_env[[path]] <- cached_contents
  }
}

# Web browser File System Access abstraction.
# Client-side coarse-grained file system API. The server sends file system commands as custom messages to the browser.
# The browser uses the (Chrome-specific, experimental) File System Access API to perform the requested actions and
# reports the results back through pre-allocated shiny inputs.
# (Intro: https://developer.chrome.com/docs/capabilities/web-apis/file-system-access,
#  Spec: https://wicg.github.io/file-system-access/)
fsa_init <- function(input, input_id, session = shiny::getDefaultReactiveDomain()) {
  checkmate::assert_string(input_id, min.chars = 1)
  ns <- session[["ns"]]

  # `state` represents a cached copy of everything that is known about the client-side storage.
  # When the client lists the contents of `state$path`, we keep a copy of the list. Same goes for the contents of files.
  # `state$error` blocks actions other than `list` from happening.
  # This variable is exported to the user, so that they can issue one of three actions (list, read, execute_IO_plan) and
  # retrieve the results from the `state` variable itself
  state <- list(
    path = character(0),
    listing = FS$EMPTY_LISTING,
    contents = new.env(parent = emptyenv()),
    error = "Not listed yet"
  ) |> list2env(parent = emptyenv())
 
  list_id <- paste0(input_id, "_list")
  read_id <- paste0(input_id, "_read")
  execute_IO_plan_id <- paste0(input_id, "_execute_IO_plan")

  # I/O operations are asynchronous. Callers can set custom callback functions that will alert them of the availability
  # of the results. The defaults are empty placeholders.
  callbacks <- list(list = function(v) NULL, read = function(v) NULL, execute_IO_plan = function(v) NULL)
  callback_count <- 0L
  # This is value we use as only argument for callbacks (the `v` in the signatures above).
  # We increase it after every callback because we expect callers to use `reactiveVal`s instead of functions and those
  # don't trigger if the value doesn't change.
  
  # LIST ----
  # This call combines both the act of attaching a client-side folder and listing its contents.
  # It throws away all information known about the remote folder and the it populates `state$listing` with available 
  # paths and `state$contents` with the raw contents of files
  # TODO: It would make sense to call this `init_and_list` to better represent its purpose
  .list <- function(callback = function(v) NULL) {
    callbacks[["list"]] <<- callback
    session$sendCustomMessage("dv_fsa_list", list(status_input_id = ns(list_id)))
  }
  shiny::observeEvent(input[[list_id]], {
    callback_count <<- callback_count + 1L
    on.exit(callbacks[["list"]](callback_count))

    v <- input[[list_id]]
    
    state[["error"]] <- character(0)
    state[["path"]] <- v[["path"]]
    state[["listing"]] <- data.frame(size = integer(0L), isdir = logical(0L))
    # `contents` field altered and not recreated to allow downstream code to keep a stable reference to it
    rm(list = ls(state[["contents"]], all.names = TRUE), envir = state[["contents"]])
    
    if (is.character(v[["error"]])) {
      state[["error"]] <- v[["error"]][[1]]
    } else if (length(v[["list"]]) > 0) {
      isdir <- sapply(v[["list"]], function(x) x[["kind"]] == "directory")
      size <- sapply(v[["list"]], function(x) x[["size"]] %||% 0L)

      if (is.logical(isdir) && is.integer(size) && length(isdir) == length(size)) {
        state[["listing"]] <- data.frame(size = size, isdir = isdir)
      } else {
        state[["error"]] <- "Assertion failed during `list` operation"
      }
    }
  })

  # READ ----
  # Update the local contents of the files specified through `paths`.
  .read <- function(paths, callback = function(v) NULL) {
    # boilerplate
    if (length(state[["error"]]) > 0) {
      callback_count <<- callback_count + 1L
      callback(callback_count)
      return(NULL)
    }
    
    # actual behavior
    callbacks[["read"]] <<- callback
    session$sendCustomMessage(
      "dv_fsa_read", 
      list(
        status_input_id = ns(read_id), 
        paths = I(paths)) # IMPORTANT: Without `I()`, javascript would interpret each letter of `paths` as a unique path
      )
  }
  shiny::observeEvent(input[[read_id]], {
    # boilerplate
    callback_count <<- callback_count + 1L
    on.exit(callbacks[["read"]](callback_count))

    v <- input[[read_id]]
    if (is.character(v[["error"]])) {
      state[["error"]] <- v[["error"]][[1]]
      return(NULL)
    }
    
    contents_check <- checkmate::check_list(v[["contents"]], types = "character", names = "unique", null.ok = TRUE)
    if (is.character(contents_check)) {
      state[["error"]] <- sprintf("Assertion failed during `read` operation: %s", contents_check)
      return(NULL)
    }
    
    # actual behavior
    for (fname in names(v[["contents"]])){
      b64_contents <- v[["contents"]][[fname]]
      decoded_contents <- base64enc::base64decode(b64_contents)
      state[["contents"]][[fname]] <- decoded_contents
    }
  })

  # EXECUTE_IO_PLAN ----
  # Send the client a group of write operations to perform and patch cached files to reflect those changes
  latest_IO_plan <- list()
  .execute_IO_plan <- function(IO_plan, callback = function(v) NULL) {
    # boilerplate
    if (length(state[["error"]]) > 0) {
      callback_count <<- callback_count + 1L
      callback(callback_count)
      return(NULL)
    }

    # actual behavior
    callbacks[["execute_IO_plan"]] <<- callback

    latest_IO_plan <<- IO_plan # NOTE: Used on the next observeEvent to update the local content cache

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
    # boilerplate
    callback_count <<- callback_count + 1L
    on.exit(callbacks[["execute_IO_plan"]](callback_count))
    on.exit(latest_IO_plan <<- list(), add = TRUE)
    shiny::req(length(state[["error"]]) == 0)

    v <- input[[execute_IO_plan_id]]
    if (is.character(v[["error"]])) {
      state[["error"]] <- v[["error"]][[1]]
      return(NULL)
    }

    status <- v[["status"]]
    if (length(status) != length(latest_IO_plan)) {
      state[["error"]] <- sprintf("Error in IO plan execution. Issued %d actions. Executed %d actions instead",
                                  length(latest_IO_plan), length(status))
      return(NULL)
    }
    
    for (i in seq_along(status)){
      if (is.character(status[[i]][["error"]])) {
        state[["error"]] <- status[[i]][["error"]][[1]]
        return(NULL)
      }
    }

    # actual behavior
    for (i in seq_along(status)){
      path <- latest_IO_plan[[i]][["path"]]
      contents <- latest_IO_plan[[i]][["contents"]]
      offset <- status[[i]][["offset"]] # Effective offset patched by client; important in case of WRITE_OFFSET_APPEND

      # 7 - Update local cached contents
      fs_update_cached_contents(state[["contents"]], path, offset, contents)

      # 8 - Update local cached listing # Compare to #yaisei; this version can't rely on synchronous access to the files
      fs_listing <- state[["listing"]]
      
      size <- length(contents)
      
      listing_row <- data.frame(size = size, isdir = FALSE)
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
    return(res[c("isdir", "size")])
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
    altered <- paths[rowSums(file_info_before[c("size")] != file_info_after[c("size")]) != 0]
    if (length(altered) > 0) {
      state[["error"]] <- sprintf("Files changed while reading them: %s", paste(altered, collapse = ", "))
      return()
    }
    
    return()
  }
  
  read_range <- function(contents, beg, one_past_end) {
    if (length(contents) == 0) return(contents[0])
    beg <- min(beg, length(contents) + 1L)
    one_past_end <- min(one_past_end, length(contents) + 1L)
    
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
     
      # 7 - Update local cached contents
      fs_update_cached_contents(state[["contents"]], path, offset, contents)
     
      # 8 - Update local cached listing # Compare to #yaisei; this version leans on the filesystem
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
    write = fs_write, # only used by execute_IO_plan; exported here for testing
    execute_IO_plan = fs_execute_IO_plan
  )
  
  return(res)
}
