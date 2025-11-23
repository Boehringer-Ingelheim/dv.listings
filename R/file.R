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
  
  browser() # TODO: Adapt to have the same behavior as ther server-side `fs_init`

  ns <- session[["ns"]]

  attach_id <- paste0(input_id, "_attach")
  list_id <- paste0(input_id, "_list")
  read_id <- paste0(input_id, "_read")
  write_id <- paste0(input_id, "_write")
  append_id <- paste0(input_id, "_append")
  read_folder_id <- paste0(input_id, "_read_folder")
  execute_IO_plan_id <- paste0(input_id, "_execute_IO_plan")

  .attach <- function() {
    session$sendCustomMessage("dv_fsa_attach", list(status_input_id = ns(attach_id)))
  }
  shiny::observe(callbacks[["attach"]](input[[attach_id]]))

  .list <- function() {
    session$sendCustomMessage("dv_fsa_list", list(status_input_id = ns(list_id)))
  }
  shiny::observe(callbacks[["list"]](input[[list_id]]))

  .read <- function(file_name, contents) {
    session$sendCustomMessage(
      "dv_fsa_read",
      list(status_input_id = ns(read_id), file_name = file_name))
  }
  shiny::observe(callbacks[["read"]](input[[read_id]]))

  .write <- function(file_name, contents) {
    session$sendCustomMessage(
      "dv_fsa_write",
      list(status_input_id = ns(write_id), file_name = file_name, contents = contents))
  }
  shiny::observe(callbacks[["write"]](input[[write_id]]))

  .append <- function(file_name, contents) {
    session$sendCustomMessage(
      "dv_fsa_append",
      list(status_input_id = ns(read_id), file_name = file_name, contents = contents))
  }
  shiny::observe(callbacks[["append"]](input[[append_id]]))

  .read_folder <- function(subfolder_candidates) {
    session$sendCustomMessage(
      "dv_fsa_read_folder",
      list(status_input_id = ns(read_folder_id), subfolder_candidates = base::I(subfolder_candidates)))
  }
  shiny::observe({
    folder_structure_base64_decode <- function(encoded_struct) {
      decoded_struct <- encoded_struct
      for (dataset_nm in names(encoded_struct)) {    
        for (file_nm in names(encoded_struct[[dataset_nm]])) {
          encoded_contents <- encoded_struct[[dataset_nm]][[file_nm]][["contents"]]
          if (!is.null(encoded_contents)) {
            decoded_contents <- base64enc::base64decode(encoded_contents) 
          } else {
            decoded_contents <- NULL
          }
          decoded_struct[[dataset_nm]][[file_nm]][["contents"]] <- decoded_contents
        }
      }
      return(decoded_struct)
    }
    
    encoded_folder_contents <- input[[read_folder_id]]
    shiny::req(is.list(encoded_folder_contents))
    decoded_folder_contents <- folder_structure_base64_decode(encoded_folder_contents)
    callbacks[["read_folder"]](decoded_folder_contents)
  })

  .execute_IO_plan <- function(IO_plan) {
    IO_plan_base64_encode <- function(plan) {  
      encoded_plan <- plan
      for (idx in seq_along(plan)) {
        if (encoded_plan[[idx]][["kind"]] == "write_file") {
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
  shiny::observe(callbacks[["execute_IO_plan"]](input[[execute_IO_plan_id]]))

  .show_overlay <- function(message) { # nolint
    session$sendCustomMessage("dv_fsa_show_overlay", list(message = message))
  }

  .hide_overlay <- function() { # nolint
    session$sendCustomMessage("dv_fsa_hide_overlay", list())
  }

  res <- list(
    attach = .attach, list = .list, read = .read, write = .write, append = .append, read_folder = .read_folder,
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
      size_in_bytes <- state[["listing"]][["size"]][[index]]
      contents <- try(readBin(con = file.path(path, rel_path), what = raw(0), n = size_in_bytes))
      if (inherits(contents, "try-error")) {
        state[["error"]] <- sprintf("Read error for file `%s`: %s", rel_path, attr(res, "condition")[["message"]])
        return()
      }
      
      if (length(contents) != size_in_bytes) {
        state[["error"]] <- sprintf("Expected %d bytes from file `%s` and got %d instead.", size_in_bytes, rel_path, 
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
    
    # FIXME? File offsets are zero-based and `raw` offsets are one based. Should we make that clearer through
    #        more carefully chosen variable names?
    
    res <- try({
      # ensure folder exists
      dname <- dirname(fname_path)
      dir.create(dname, showWarnings = FALSE, recursive = TRUE)
    
      # write to temp file in the same folder, because we rely on `file.rename to move the file to its Final Destination
      tmp_fname <- tempfile(tmpdir = dname, fileext = ".tmp")
      
      if (file.exists(fname_path)) file.copy(fname_path, tmp_fname)
      else writeBin(con = tmp_fname, raw(0))
      
      con <- file(tmp_fname, open = "r+b")
      seek(con, where = 0L, origin = "end", rw = "read")
      file_size <- seek(con, where = 0L, origin = "start", rw = "read")
     
      if (offset == FS$WRITE_OFFSET_APPEND) offset <- file_size # append without checking offset
      
      if (offset > file_size) stop(sprintf("Write operation to offset %d would create a hole in `%s`.", offset, path))
      
      cached_contents <- state[["contents"]][[path]] %||% raw(0)
      range_contents_old <- read_range(cached_contents, 1L + offset, 1L + offset + length(contents))
      
      seek(con, where = offset, origin = "start", rw = "read")
      range_contents_new <- readBin(con = con, what = raw(0), n = length(contents))
      
      if (!identical(range_contents_new, range_contents_old))
        stop(sprintf("Write operation to `%s` would overwrite contents of unknown origin", path))
      
      seek(con, where = offset, origin = "start", rw = "write")
      
      writeBin(contents, con, endian = "little")
     
      close(con)
      
      file.rename(tmp_fname, fname_path)
     
      # Update local cached contents 
      if (length(contents) > 0) {
        cached_contents[(offset + 1L):(offset + length(contents))] <- contents
        state[["contents"]][[path]] <- cached_contents
      }
     
      # Update local cached listing 
      fs_listing <- state[["listing"]]
      listing_row <- fs_list_paths(state[["path"]], path)
      if (path %in% rownames(fs_listing)) {
        listing_index <- which(path == rownames(fs_listing))[[1]]
        state[["listing"]][listing_index, ]  <- listing_row
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
