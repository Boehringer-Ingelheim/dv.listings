# Web browser File System Access abstraction.
# Client-side coarse-grained file system API. The server sends file system commands as custom messages to the browser.
# The browser uses the (Chrome-specific, experimental) File System Access API 
#          (Intro: https://developer.chrome.com/docs/capabilities/web-apis/file-system-access,
#           Spec: https://wicg.github.io/file-system-access/)
# to perform the requested actions. It reports the results back through pre-allocated shiny inputs.
fsa_init <- function(input, input_id, callbacks, session = shiny::getDefaultReactiveDomain()) {

  checkmate::assert_string(input_id, min.chars = 1)

  checkmate::assert_list(callbacks, types = "function") # in shiny apps, members will likely be reactiveVals
  checkmate::assert_set_equal(
    names(callbacks), c("attach", "list", "read", "write", "append", "read_folder", "execute_IO_plan")
  )
  
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

  .execute_IO_plan <- function(IO_plan, is_init = FALSE) {
    IO_plan_base64_encode <- function(plan) {  
      encoded_plan <- plan
      for (idx in seq_along(plan)) {
        if (encoded_plan[[idx]][["type"]] == "write_file") {
          encoded_plan[[idx]][["contents"]] <- base64enc::base64encode(encoded_plan[[idx]][["contents"]])
        }
      }
      return(encoded_plan)
    }
    
    IO_plan_base64 <- IO_plan_base64_encode(IO_plan)

    session$sendCustomMessage(
      "dv_fsa_execute_io_plan",
      list(status_input_id = ns(execute_IO_plan_id), plan = IO_plan_base64, is_init = is_init)
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
# - Provide a server-only implementation for shiny hosting services that offer app-specific storage.
fs_init <- function(callbacks, path) {
  checkmate::assert_list(callbacks, types = "function") # in shiny apps, these will likely be reactiveVals
  checkmate::assert_set_equal(
    names(callbacks), c("attach", "list", "read", "write", "append", "read_folder", "execute_IO_plan")
  )
  
  path <- normalizePath(path) # remove trailing slash, etc.
  
  res <- list(
    attach = function() {
      v <- list(connected = TRUE, name = basename(path), error = NULL)
      callbacks[["attach"]](v)
    },
    list = function() {
      callbacks[["list"]](error = "Not implemented")
    },
    read = function(file_name, contents) {
      callbacks[["read"]](error = "Not implemented")
    },
    write = function(file_name, contents) {
      callbacks[["write"]](error = "Not implemented")
    },
    append = function(file_name, contents) {
      callbacks[["append"]](error = "Not implemented")
    },
    read_folder = function(subfolder_candidates) {
      # NOTE: Adapted from:
      # https://github.com/dull-systems/yours_truelib/blob/441740eb02fc9a9029c63c6e3c1d56c5ad638d97/YT.R#L153-L166
      read_file_set <- function(paths) {
        # Provides a consistent view of a set of files by checking that they don't change while we read them.
        # Gets their mtimes and sizes; reads their contents; asserts that mtimes and sizes have not changed;
        # returns contents, mtimes and sizes.
        res <- list()
        file_info <- file.info(paths)
        no_size <- paths[!is.finite(file_info$size)]
        if (length(no_size) > 0) 
          return(simpleCondition(sprintf("Could not get file size for: `%s`.", paste(no_size, collapse = ", "))))
        
        for (path in paths){ 
          res[[basename(path)]] <- list(
            size = file_info[path, "size"], 
            time = as.numeric(file_info[path, "mtime"]),
            contents = readBin(con = path, what = raw(), n = file_info[path, "size"]),
            error = NULL
          )
        }
        file_info_after <- file.info(paths)
        altered <- paths[rowSums(file_info[c("size", "mtime")] != file_info_after[c("size", "mtime")]) != 0]
        if (length(altered) > 0)
          return(simpleCondition(sprintf("Files changed while reading them: %s", paste(altered, collapse = ", "))))
        return(res)
      }
        
      v <- list()
      subfolders <- file.path(path, subfolder_candidates)
      for (subfolder in subfolders) {
        contents <- read_file_set(list.files(subfolder, full.names = TRUE, recursive = FALSE))
        if (inherits(contents, "condition")) {
          # NOTE: early out
          callbacks[["read_folder"]](list(error = contents[["message"]]))
          return()
        }
        v[[basename(subfolder)]] <- contents
      }
      callbacks[["read_folder"]](v)
    },
    execute_IO_plan = function(IO_plan, is_init = FALSE) {
      first_error_message <- NULL
      for (i_command in seq_along(IO_plan)) {
        command <- IO_plan[[i_command]]
        IO_plan[[i_command]][["error"]] <- NULL
        if (command[["type"]] == "write_file") {
          if (command[["mode"]] != "bin") {
            first_error_message <- "The only supported write mode is `bin`"
            break
          }
          fname <- file.path(path, command[["path"]], command[["fname"]])
          dname <- dirname(fname)
          dir.create(dname, showWarnings = FALSE, recursive = TRUE)
          writeBin(command[["contents"]], fname) # TODO: Checks
        } else if (command[["type"]] == "append_file") {
          if (command[["mode"]] != "bin") {
            first_error_message <- "The only supported append mode is `bin`"
            break
          }
          fname <- file.path(path, command[["path"]], command[["fname"]])
          con <- file(fname, open = "ab")
          on.exit(close(con))
          writeBin(command[["contents"]], con) # TODO: Checks
        } else {
          first_error_message <- sprintf("Command type '%s' not supported yet", command[["type"]])
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
      }
      
      callbacks[["execute_IO_plan"]](list(status = IO_plan, is_init = is_init))
    }
  )
  
  return(res)
}
