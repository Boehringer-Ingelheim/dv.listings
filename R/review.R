REV <- pack_of_constants(
  ID = pack_of_constants(
    DROPDOWN = "review_dropdown",
    REVIEW_UI = "review_ui",
    REVIEW_COL = "__review__",
    ROLE_COL = "__role__",
    STATUS_COL = "__status__",
    REVIEW_TIMESTAMP_COL = "__review_timestamp__",
    DATA_TIMESTAMP_COL = "__data_timestamp__",
    LATEST_REVIEW_COL = "__latest_review__",
    REVIEW_SELECT = "rev_id",
    ROLE = "rev_role",
    DEV_EXTRA_COLS_SELECT = "dev_extra_cols_select",
    CONNECT_STORAGE = "connect_storage"
  ),
  LABEL = pack_of_constants(
    DROPDOWN = "Annotation",
    REVIEW_COLS = c("Latest Review", "Latest Reviewer", "Status", "Latest Reviews")
  ),
  STATUS_LEVELS = pack_of_constants(
    PENDING = "Pending",
    LATEST_OUTDATED = "Latest Outdated",
    CONFLICT = "Conflict",
    CONFLICT_ROLE = "Conflict I can fix",
    OK = "OK"
  )
)

REV_time_from_timestamp <- function(v) {  
  non_breaking_hyphen <- "\U2011"
  template <- paste0("%Y", non_breaking_hyphen, "%m", non_breaking_hyphen, "%d %H:%M:%S")
  v <- as.POSIXct(v, origin = "1970-01-01", tz = "UTC")  
  res <- format(v, template)  
  res[is.na(v)] <- ""
  return(res)
}

REV_include_review_info <- function(annotation_info, data, col_names, extra_col_names) {
  if (nrow(data) < nrow(annotation_info)) {
    filter_mask <- attr(data, "filter_mask")
    annotation_info <- annotation_info[filter_mask, ]
  }

  if (nrow(data) > nrow(annotation_info)) browser() # Should not happen
  
  reviews <- annotation_info[["review"]]
  roles <- annotation_info[["role"]]
  status <- NA_character_
  latest_reviews <- annotation_info[["latest_reviews"]]
  
  # TODO: Introduce something to this effect
  # > shiny::validate(shiny::need(nrow(data) <= length(reviews), "Error: Inconsistency between review data and loaded datasets"))
 
  # include review-related columns
  res <- data.frame(reviews, roles) # FIXME: (maybe) Can't pass latest review as argument. List confuses data.frame
  res[["status"]] <- status
  res[["latest_reviews"]] <- latest_reviews
  names(res)[[1]] <- REV$ID$REVIEW_COL
  names(res)[[2]] <- REV$ID$ROLE_COL
  names(res)[[3]] <- REV$ID$STATUS_COL
  names(res)[[4]] <- REV$ID$LATEST_REVIEW_COL
  res_col_names <- c(REV$LABEL$REVIEW_COLS)
 
  # add actual data
  res <- cbind(res, data)
  res_col_names <- c(res_col_names, col_names)

  return(list(data = res, col_names = res_col_names))
}

REV_UI <- function(ns, roles) {
  choices <- setNames(c("", roles), c("<select reviewer role>", roles))

  res <- list()
  res[["ui"]] <- shiny::tagList(
    shiny::div(style = "text-align:right",
               shiny::actionButton(inputId = ns(REV$ID$CONNECT_STORAGE), label = "<Attach folder>"),
    ),
    shiny::selectInput(
      inputId = ns(REV$ID$ROLE), label = "Role:", choices = choices
    )
  )
  res[["input_ids_to_exclude_from_bookmarking"]] <- c(ns(REV$ID$CONNECT_STORAGE), ns(REV$ID$ROLE), 
                                                      ns(REV$ID$DEV_EXTRA_COLS_SELECT))

  return(res)
}

REV_load_annotation_info <- function(folder_contents, review, dataset_lists) {
  loaded_annotation_info <- list()

  error <- character()

  # TODO: Chop it in a set of consecutive observers with the app blocked with an overlay
  # consecutive observers should allow given client time in an asynch way
  # consider including the global lock for the whole process until released
  # A queue should exist or retry in the client that is activated when the global lock is released
  # Maybe a one second wait on a general state 
  # NOTE(miguel): I think it's possible the above comment predates the IO_action plan. The expensive operations
  #               are all I/O related and I think it's OK for the server to wait for all of them to be performed
  #               in the client. The client can figure out which and how many blockers to put up to block the user
  #               from interacting with the app in the meantime. After that, a single notification to the server
  #               should be enough.
  # TODO(miguel): Discuss the two comments above with the team. Maybe collapse them into an architectural comment.

  folder_IO_plan <- list()
  append_IO_action <- function(action) {    
    folder_IO_plan <<- c(folder_IO_plan, list(action))
  }
  
  for (dataset_lists_name in names(dataset_lists)) {
    sub_res <- list()
    dataset_list <- dataset_lists[[dataset_lists_name]]

    if (!dataset_lists_name %in% names(folder_contents)) {      
      append_IO_action(
        list(
          type = "create_dir",
          dname = dataset_lists_name
        )
      )
      folder_contents[[dataset_lists_name]] <- list()
    }

    # review.codes (common to all datasets)
    fname <- "review.codes"
    if (fname %in% names(folder_contents[[dataset_lists_name]])) {
      contents <- folder_contents[[dataset_lists_name]][[fname]][["contents"]]
      review_info <- RS_parse_review_codes(contents)
      if (!identical(review_info, review[["choices"]])) {
        error <- c(
          error, 
          paste0(
            "Review choices should remain stable during the course of a trial.\n",
            "The original review choices are: ", paste(sprintf('"%s"', review_info), collapse = ", "), ".\n",
            "This restriction is likely to be lifted in a future revision of the review feature."
          )
          # TODO: Combine new review[["choices"]] with old `review.codes`
          #       while preserving original associated integer codes
        )
      }
    } else {
      contents <- RS_compute_review_codes_memory(review[["choices"]])
      append_IO_action(
        list(
          type = "write_file",
          mode = "bin",
          path = dataset_lists_name,
          fname = fname,
          contents = contents
        )
      )                  
    }
      
    for (dataset_review_name in names(review[["datasets"]])){
      dataset <- dataset_list[[dataset_review_name]]
    
      role_factor <- factor("", levels = c("", review[["roles"]]))
     
      row_count <- nrow(dataset)
     
      default_review <- factor(review[["choices"]][[1]], levels = review[["choices"]])
      dataset_review <- data.frame(review = rep(default_review, row_count),
                                   timestamp = numeric(row_count), 
                                   role = rep(role_factor, row_count), 
                                   data_timestamp = numeric(row_count))
      
      id_vars <- review[["datasets"]][[dataset_review_name]][["id_vars"]]
      tracked_vars <- setdiff(review[["datasets"]][[dataset_review_name]][["tracked_vars"]], id_vars)
     
      base_timestamp <- NA_real_
      data_timestamps <- rep(NA_real_, row_count)
      # <domain>_000.base
      fname <- paste0(dataset_review_name, "_000.base")
      if (fname %in% names(folder_contents[[dataset_lists_name]])) {
        contents <- folder_contents[[dataset_lists_name]][[fname]][["contents"]]        

        delta_fnames <- local({
          dataset_fnames <- names(folder_contents[[dataset_lists_name]])
          pattern <- sprintf("^%s_[0-9]*.delta", dataset_review_name)
          sort(grep(pattern, dataset_fnames, value = TRUE))
        })        
        
        deltas <- local({
          res <- list()
          for (fname in delta_fnames){            
            # TODO: Control for file errors?
            res[[length(res) + 1]] <- folder_contents[[dataset_lists_name]][[fname]][["contents"]]
          }
          return(res)
        })
        base_info <- RS_load(contents, deltas) # TODO? Rename as `RS_load_memory` and make `RS_load` that works with fnames?
     
        # Data stability checks 
        local({
          OK <- c(id_vars = TRUE, tracked_vars = TRUE) 
          
          # Check `id_vars` stability
          if (!identical(base_info[["id_vars"]], sort(id_vars))) {
            OK[["id_vars"]] <- OK[["tracked_vars"]] <- FALSE
            error <<- c(
              error, 
              paste0(
                "[", dataset_review_name, "] ", "`id_vars` should remain stable during the course of a trial.\n",
                "The original value is: ", paste(sprintf('"%s"', base_info[["id_vars"]]), collapse = ", "), ".\n"
              )
            )
          }
          
          # Check tracked variables stability
          if (OK[["id_vars"]]) {
            # This code is guarded by a conditional because if `id_vars` is modified, `tracked_vars` will likely be 
            # affected as a side effect. In that situation, this error is insignificant, so we don't notify it.
            cur_tracked_vars <- base_info[["tracked_vars"]]
            new_tracked_vars <- sort(tracked_vars)
            if (!identical(cur_tracked_vars, new_tracked_vars)) {
              extra_vars <- setdiff(new_tracked_vars, cur_tracked_vars)
              if (length(extra_vars)) {
                OK[["tracked_vars"]] <- FALSE
                error <<- c(
                  error, 
                  paste0(
                    "[", dataset_review_name, "] ",
                    "The following variables were not available on a previous iteration of the review process: ",
                    paste(sprintf('"%s"', extra_vars), collapse = ", "), ".\n",
                    "Please, exclude them from the \"tracked_vars\" parameter."
                  )
                )
              } 
              missing_vars <- setdiff(cur_tracked_vars, new_tracked_vars)
              if (length(missing_vars)) {
                OK[["tracked_vars"]] <- FALSE
                error <<- c(
                  error, 
                  paste0(
                    "[", dataset_review_name, "] ",
                    "The following variables have not been specified as `tracked_vars`: ",
                    paste(sprintf('"%s"', missing_vars), collapse = ", "), ".\n",
                    "Previous runs of this tool were instructed to track them. Please, reinstate them."
                  )
                )
              }
            }
          }
          
          # Check `id_vars` and `tracked_vars` type stability
          vars <- character(0)
          prev_types_raw <- raw(0)
          cur_types_raw <- raw(0)
          if (OK[["id_vars"]]) {
            vars <- sort(id_vars)
            prev_types_raw <- base_info[["id_var_types"]]
          }
          
          if (OK[["tracked_vars"]]) {
            vars <- c(vars, sort(tracked_vars))
            prev_types_raw <- c(prev_types_raw, base_info[["tracked_var_types"]])
          }
          
          cur_types_raw <- as.raw(RS_compute_data_frame_variable_types(dataset, vars))
          
          if (!identical(prev_types_raw, cur_types_raw)) {
            error_string <- paste0(
              "[", dataset_review_name, "] ",
              "The following variables have changed type (VAR_NAME: BEFORE, AFTER): \n"
            )
            prev_types <- RS_parse_data_frame_variable_types(prev_types_raw)
            cur_types <- RS_parse_data_frame_variable_types(cur_types_raw)
            for (i_var in seq_along(vars)){
              var <- vars[[i_var]]
              prev_type <- prev_types[[i_var]]
              cur_type <- cur_types[[i_var]]
              if (prev_type != cur_type) {
                error_string <- paste0(error_string, sprintf("%s: %s, %s\n", var, prev_type, cur_type))
              }
            }
            error_string <- paste(error_string, "Please use the types provided originally.")
            
            error <<- c(error, error_string)
          }
        })
        
        dataset_hash <- RS_hash_data_frame(dataset)
        if (!identical(dataset_hash, base_info[["contents_hash"]])) {
            new_delta_and_errors <- RS_compute_delta_memory(state = base_info, dataset)
           
            error_strings <- new_delta_and_errors[["error"]]
            if (length(error_strings)) {
              error <- c(error, paste0("[", dataset_review_name, "] ", error_strings))
            }
            
            new_delta <- new_delta_and_errors[["contents"]]
            
            deltas[[length(deltas) + 1]] <- new_delta
            base_info <- RS_load(contents, deltas)

            delta_number <- length(delta_fnames) + 1
            fname <- sprintf("%s_%03d.delta", dataset_review_name, delta_number)
            append_IO_action(
              list(
                type = "write_file",
                mode = "bin",
                path = dataset_lists_name,
                fname = fname,
                contents = new_delta
              )
            )
        }
      } else {
        contents <- RS_compute_base_memory(dataset_review_name, dataset, id_vars, tracked_vars)
        base_info <- RS_parse_base(contents)
        append_IO_action(
            list(
              type = "write_file",
              mode = "bin",
              path = dataset_lists_name,
              fname = fname,
              contents = contents
            )
          )  
        base_timestamp <- base_info[["timestamp"]] # TODO: Consider providing timestamp to RS_compute_base_memory instead?
        data_timestamps <- base_info[["row_timestamps"]]
      }
      
      base_timestamp <- base_info[["timestamp"]]
      data_timestamps <- base_info[["row_timestamps"]]
      
      # This probably should live alongside RS_* functions
      # NOTE(miguel): I didn't consider the possibility of row reordering in the original design of the review file
      #               formats. As a consequence, I missed the need for this row map (that makes it possible to assign
      #               reviews from row indices to id_hashes). This vector could be appended to `delta` files, at the
      #               cost of four bytes per row. I think the superior approach would be to speed up data.frame row 
      #               hashing (by dropping down to C?), as the initial hashing would also benefit from it.
      #               That's why we recompute the hashes here:
      state_to_dataset_row_mapping <- local({ # TODO: Is this the right name?
        id_vars <- base_info[["id_vars"]]
        id_hashes <- RS_compute_id_hashes(dataset, id_vars)
        mapping <- match(asplit(id_hashes, 2), asplit(base_info[["id_hashes"]], 2))
        return(mapping)
      })
      
      # Compute reverse mapping (which is a more useful representation for the running app)
      dataset_to_state_row_mapping <- local({ # TODO: Is this the right name?
        res <- integer(length(state_to_dataset_row_mapping))
        res[state_to_dataset_row_mapping] <- seq_along(state_to_dataset_row_mapping)
        return(res)
      }) 
     
      dataset_review[["timestamp"]] <- base_timestamp
      dataset_review[["data_timestamp"]] <- data_timestamps[state_to_dataset_row_mapping]
      
      # <domain>_<ROLE>.review      
      all_latest_reviews <- local({
        role_list <- rep_len(list(), length.out = length(review[["roles"]]))
        names(role_list) <- review[["roles"]]
        role_timestamp_list <- list(reviews = role_list, data_timestamp = NULL)
        rep_len(list(role_timestamp_list), length.out = nrow(dataset_review))
      })

      for (role in review[["roles"]]){
        fname <- paste0(dataset_review_name, "_", role, ".review")
        if (fname %in% names(folder_contents[[dataset_lists_name]])) {          
          contents <- folder_contents[[dataset_lists_name]][[fname]][["contents"]]          
        } else { 
          contents <- RS_compute_review_reviews_memory(role, dataset_review_name)
          append_IO_action(
            list(
              type = "write_file",
              mode = "bin",
              path = dataset_lists_name,
              fname = fname,
              contents = contents
            )
          )
        }

        # NOTE: each role keeps their own decisions...
        role_review <- RS_parse_review_reviews(contents, dataset_to_state_row_mapping = dataset_to_state_row_mapping, 
                                               expected_role = role, expected_domain = dataset_review_name)
        # NOTE: and we combine them to display the latest one, but we could...
        # TODO: ...make reviews by all roles available to the user? (could be done through separate columns)


        # Progressive update of all roles through the mask
        update_mask <- (role_review[["timestamp"]] > dataset_review[["timestamp"]])
        
        if (any(update_mask)) {
          review_indices <- role_review[update_mask, ][["review"]]         
          dataset_review[update_mask, ][["review"]] <- review[["choices"]][review_indices]
          dataset_review[update_mask, ][["timestamp"]] <- role_review[update_mask, ][["timestamp"]]
          dataset_review[update_mask, ][["role"]] <- role
        }
        # compact all in lists
        # Replace by list of roles so it is a single columns and we can directly iterate over it
        
        all_latest_reviews <- local({          
          reviewed_idx <- which(role_review[["timestamp"]] > 0)          
          for (idx in reviewed_idx) {
            review_char <- review[["choices"]][role_review[["review"]][[idx]]]         
            curr_crr <- list(role = role, review = review_char, timestamp = role_review[["timestamp"]][[idx]], reviewed_at_least_once = TRUE)            
            all_latest_reviews[[idx]][["reviews"]][[role]] <- curr_crr            
          } 

          for (idx in seq_len(nrow(dataset_review))) {            
            all_latest_reviews[[idx]][["data_timestamp"]] <- dataset_review[["data_timestamp"]][[idx]]
          } 
          all_latest_reviews        
        })
      }

      dataset_review[["latest_reviews"]] <- all_latest_reviews
           
      # FIXME? Mapping attached as attribute to avoid rewriting prototype-level code
      # Add latest roles columns      
      sub_res[[dataset_review_name]] <- dataset_review[c("review", "timestamp", "role", "data_timestamp", "latest_reviews")]
      attr(sub_res[[dataset_review_name]], "state_to_dataset_row_mapping") <- state_to_dataset_row_mapping
      # FIXME? Base timestamp attached as attribute to avoid rewriting prototype-level code
      attr(sub_res[[dataset_review_name]], "base_timestamp") <- base_timestamp
      # Add tracked_hashes for each revision of the dataset to be able to attribute row changes to specific columns
      attr(sub_res[[dataset_review_name]], "revisions") <- base_info[["revisions"]]
    }
    loaded_annotation_info[[dataset_lists_name]] <- sub_res
  }

  res <- list(
    loaded_annotation_info = loaded_annotation_info,
    folder_IO_plan = folder_IO_plan,
    error = error
  )

  return(res)
}
    
REV_logic_1 <- function(state, input, review, datasets, fs_client, fs_callbacks) { # TODO: Rename
  # TODO: Flesh out the state machine. Right now there are only default selections for quick iteration
  state[["connected"]] <- shiny::reactiveVal(FALSE)
  state[["contents_ready"]] <- shiny::reactiveVal(FALSE)
  state[["folder"]] <- NULL
  state[["annotation_info"]] <- NULL

  shiny::observeEvent(input[[REV$ID$CONNECT_STORAGE]], {    
    fs_client[["attach"]]()    
  }, ignoreNULL = FALSE, ignoreInit = TRUE) # TODO: Remove

  shiny::observeEvent(fs_callbacks[["attach"]](), {
    attach_status <- fs_callbacks[["attach"]]()
    shiny::req(is.list(attach_status))
    state[["connected"]](attach_status[["connected"]])
    state[["folder"]] <- attach_status[["name"]]    
    shiny::updateActionButton(inputId = REV$ID$CONNECT_STORAGE, label = paste("Storage:", state[["folder"]]))        

    if (attach_status[["connected"]] == TRUE) {
      fs_client[["read_folder"]](names(datasets))      
    } else {
      if (!is.null(attach_status[["error"]])) shiny::showNotification(attach_status[["error"]], type = "error")
      state[["annotation_info"]] <- NULL
    }
  }, ignoreNULL = FALSE, ignoreInit = TRUE) # TODO: Remove

  shiny::observeEvent(fs_callbacks[["read_folder"]](), {
    folder_contents <- fs_callbacks[["read_folder"]]()
    shiny::req(is.list(folder_contents))
    load_results <- REV_load_annotation_info(folder_contents, review, datasets)
    if (length(load_results[["error"]])) {
      showNotification(
        ui = shiny::HTML(
          paste(
            "<h4>FAILED TO START REVIEW INTERFACE</h4>",
            paste(
              paste("\u2022", load_results[["error"]]), 
              collapse = "<br>")
          )
        ),
        duration = NULL, closeButton = TRUE, type = "error"
      )
      # NOTE: We remain in this state while we wait for the user to select an appropriate alternative folder
    } else {
      state[["annotation_info"]] <- load_results[["loaded_annotation_info"]]
      fs_client[["execute_IO_plan"]](IO_plan = load_results[["folder_IO_plan"]], is_init = TRUE)
    }
  }, ignoreNULL = FALSE, ignoreInit = TRUE) # TODO: Remove

  # TODO: fs_client[["execute_IO_plan"]][["v"]] if we use the execute IO plan in more places this observer will run
  # more times than it should. Check how this can be avoided, including extra element in status, create a new input?
  shiny::observeEvent(fs_callbacks[["execute_IO_plan"]](),
    {
      plan_result <- fs_callbacks[["execute_IO_plan"]]()
      if (isTRUE(plan_result[["is_init"]])) {
        plan_status <- plan_result[["status"]]
        error <- FALSE
        for (entry in plan_status) {
          if (!is.null(entry[["error"]])) {
            error <- TRUE
            break
          }
        }

        if (!error) {
          state[["contents_ready"]](TRUE)
        } else {
          shiny::showNotification("Error in initial read and write operation", type = "error")
          stop("Error reading and writing")
        }
      }
    },
    ignoreNULL = FALSE,
    ignoreInit = TRUE
  )
}

REV_logic_2 <- function(ns, state, input, review, datasets, selected_dataset_list_name, selected_dataset_name, data,
                        dt_proxy, fs_execute_IO_plan) { # TODO: Rename
  shiny::observeEvent(input[[REV$ID$REVIEW_SELECT]], {
    role <- input[[REV$ID$ROLE]]

    if (!checkmate::test_string(role, min.chars = 1)) {
      msg <- "Attempted write with unset role"
      shiny::showNotification(msg, type = "warning")
      warning(msg)
      shiny::req(FALSE)
    }
    

    dataset_list_name <- selected_dataset_list_name() 
    dataset_name <- selected_dataset_name()
    
    new_data <- data()
    
    info <- input[[REV$ID$REVIEW_SELECT]]

    # Replace in full bulk operation
    if ("bulk" %in% names(info) && identical(info[["bulk"]], "filtered")) {
      info[["row"]] <- input[[paste0(TBL$TABLE_ID, "_rows_all")]]
    }
    shiny::req(length(info[["row"]]) > 0)

    i_row <- as.numeric(info[["row"]])

    defiltered_i_row <- local({
      # `i_row` is relative to the filtered data sent to the client ...
      filter_mask <- attr(new_data, "filter_mask")
      res <- which(filter_mask)[i_row]
      return(res)
    })
    
    stored_i_row <- local({
      # ... and that `i_row` needs to be mapped into a base+deltas (stable) index
      row_map <- attr(state[["annotation_info"]][[dataset_list_name]][[dataset_name]], "state_to_dataset_row_mapping")
      res <- row_map[defiltered_i_row]
      return(res)
    })
    option <- as.integer(info[["option"]])
   
    timestamp <- SH$get_UTC_time_in_seconds()
        
    extra_col_names <- input[[REV$ID$DEV_EXTRA_COLS_SELECT]]
    
    # NOTE: Partially repeats #weilae 
    # NOTE: We could cache the modified table and avoid repeating this operation 
    #       if it turns out to be a performance bottleneck
    changes <- REV_include_review_info(
      annotation_info = state[["annotation_info"]][[dataset_list_name]][[dataset_name]],
      data = data(), col_names = list(), extra_col_names = extra_col_names
    )
    new_data <- changes[["data"]]

    contents <- c()

    # TODO: This loop can be too long when there are too many rows
    # Writing is done in one step but by row update is done one by one.
    for (idx in seq_along(i_row)) {

      curr_i_row <- i_row[[idx]]
      curr_defiltered_i_row <- defiltered_i_row[[idx]]
      curr_stored_i_row <- stored_i_row[[idx]]

      last_review_entry <- new_data[curr_i_row, ][[REV$ID$LATEST_REVIEW_COL]][[1]]
      last_review_entry[["reviews"]][[role]][["role"]] <- role
      last_review_entry[["reviews"]][[role]][["review"]] <- review[["choices"]][[option]]
      last_review_entry[["reviews"]][[role]][["timestamp"]] <- timestamp

      # Fixed columns
      new_data[curr_i_row, ][[REV$ID$REVIEW_COL]] <- review[["choices"]][[option]]
      new_data[curr_i_row, ][[REV$ID$ROLE_COL]] <- role
      new_data[curr_i_row, ][[REV$ID$LATEST_REVIEW_COL]][[1]] <- last_review_entry
      
      # - data_time does not change when reviewed
      
      # `REV_load_annotation_info()` would return this same (modified) state, but we do manual synchronization
      # to avoid potentially expensive data reloading
      row_contents <- state[["annotation_info"]][[dataset_list_name]][[dataset_name]][curr_defiltered_i_row, ]
      row_contents[["review"]] <- review[["choices"]][[option]]
      row_contents[["timestamp"]] <- timestamp
      row_contents[["role"]] <- role    
      row_contents[["latest_reviews"]][[1]] <- last_review_entry
      # > row_contents[["data_timestamp"]] # unchanged
      
      state[["annotation_info"]][[dataset_list_name]][[dataset_name]][curr_defiltered_i_row, ] <- row_contents

      contents <- c(
        contents,
        SH$integer_to_raw(curr_stored_i_row),
        SH$integer_to_raw(option),
        SH$double_to_raw(timestamp)
      )

    }

    # TODO: Benchmark to decide if this is a bottleneck for bigger datasets
    new_data[[REV$ID$STATUS_COL]] <- REV_compute_status(new_data, role)
    new_data[[REV$ID$LATEST_REVIEW_COL]] <- REV_review_var_to_json(new_data[[REV$ID$LATEST_REVIEW_COL]])

   
    # If we were doing pure client-side rendering of DT, maybe we could do a lighter upgrade with javascript:
    # > var table = $('#DataTables_Table_0').DataTable();
    # > var tmp = table.row(5).data();
    # > table.columns()[0].length;
    # > tmp[9] = '2';
    # > table.row(5).data(tmp).invalidate();
    DT::replaceData(dt_proxy, new_data, resetPaging = FALSE, clearSelection = "none")    
    
    fname <- paste0(dataset_name, "_", role, ".review")

    IO_plan <- list(
      list(
        type = "append_file",
        mode = "bin",
        path = dataset_list_name,
        fname = fname,
        contents = contents
      )
    )

    fs_execute_IO_plan(IO_plan, is_init = FALSE)
  })
  
  return(NULL)
}

REV_review_var_to_json <- function(col) {
  res <- vector(mode = "character", length = length(col))
  for (idx in seq_along(col)) {
    curr_entry <- col[[idx]]
    # Careful autounbox works with this particular structure
    # TODO: (Optimize?) toJSON by hand given the simplicity of the structure
    res[[idx]] <- jsonlite::toJSON(curr_entry, auto_unbox = TRUE)
  }
  res
}

REV_compute_status <- function(dataset_review, role) {

  # Does this function make sense with no role? Yes it does because the latest review is the one that maybe outdated,
  # conflicting, unreviewed, etc.
  # Optionally we could indicate if the current role does have a conflict or is it someone else?
  # We can indicate who conflicts with the latest review
  # Include the button if the selected role has this problem, basically we have a different review and we want to 
  # change to the currently selected.

  # Should conflict only appear with respect to the selected role? Then this column should be recalculated everytime.
  # Conflict with me conflict with others?
  # For now we say conflict but we don't say with whom.

  res <- dataset_review
  unclassed_status_levels <- unclass(REV$STATUS_LEVELS) # Get rid of poc class, otherwise factor levels errors
  res[[REV$ID$STATUS_COL]] <- factor(rep(REV$STATUS_LEVELS$OK, length = nrow(res)), levels = unclassed_status_levels)
  pending_mask <- dataset_review[[REV$ID$REVIEW_COL]] == levels(dataset_review[[REV$ID$REVIEW_COL]])[[1]] # First level is always default
  reviewed_status_idx <- which(dataset_review[[REV$ID$ROLE_COL]] != "")
  
  conflict_with_latest_mask <- rep_len(FALSE, nrow(res))
  conflict_with_role_mask <- rep_len(FALSE, nrow(res))
  outdated_latest_mask <- rep_len(FALSE, nrow(res))
  outdated_role_mask <- rep_len(FALSE, nrow(res))
  for (idx in reviewed_status_idx) {
    if (dataset_review[[REV$ID$ROLE_COL]][[idx]] != "") { # There has been at least one review
      curr_reviews <- dataset_review[[REV$ID$LATEST_REVIEW_COL]][[idx]][["reviews"]]
      data_timestamp <- dataset_review[[REV$ID$LATEST_REVIEW_COL]][[idx]][["data_timestamp"]]
      latest_review <- dataset_review[[REV$ID$REVIEW_COL]][[idx]]
      latest_reviewer <- dataset_review[[REV$ID$ROLE_COL]][[idx]]
      latest_timestamp <- curr_reviews[[as.character(latest_reviewer)]][["timestamp"]]

      # latest is outdated      
      outdated_latest_mask[[idx]] <- data_timestamp > latest_timestamp

      for (role_nm in names(curr_reviews)) {
        curr_entry <- curr_reviews[[role_nm]]
        if (!is.null(curr_entry)) { # role_nm has reviewed this row
          curr_review_timestamp <- curr_entry[["timestamp"]]
          curr_review <- curr_entry[["review"]]
          # current is outdated
          conflict_with_latest_mask[[idx]] <- conflict_with_latest_mask[[idx]] || curr_review != latest_review
          if (!is.na(role) && role_nm == role) {
            outdated_role_mask[[idx]] <- data_timestamp > curr_review_timestamp
            conflict_with_role_mask[[idx]] <- curr_entry[["review"]] != latest_review
          }
        }
      }
    }
  }

  res[[REV$ID$STATUS_COL]][pending_mask] <- REV$STATUS_LEVELS$PENDING
  res[[REV$ID$STATUS_COL]][outdated_latest_mask] <- REV$STATUS_LEVELS$LATEST_OUTDATED
  res[[REV$ID$STATUS_COL]][conflict_with_latest_mask & !outdated_latest_mask] <- REV$STATUS_LEVELS$CONFLICT
  res[[REV$ID$STATUS_COL]][conflict_with_role_mask & !outdated_latest_mask] <- REV$STATUS_LEVELS$CONFLICT_ROLE
    
  return(res[[REV$ID$STATUS_COL]])
}

# Collect hashes that were known prior to the times indicated by `review_timestamps` 
REV_collect_latest_review_hashes <- function(revisions, review_timestamps) {
  res <- revisions$tracked_hashes[[1]]
  
  revision_count <- length(revisions$tracked_hashes)
  i_revision <- 2
  while (i_revision <= revision_count) {
    ts <- revisions$timestamps[[i_revision]]
    hashes <- revisions$tracked_hashes[[i_revision]]
    update_mask <- (ts < review_timestamps)
    res[, update_mask] <- hashes[, update_mask]
    i_revision <- i_revision + 1
  }
  return(res)
}

# Infer which cells changed based of two matrices of old (`h0`) and new (`h1`) hashes
# Returns pairs of (row, col) based on the ordering of h0
# The hashes are kept in canonical order, so that's the order this function outputs
REV_report_changes <- function(h0, h1, verbose = FALSE) {
  res <- list()
  if (nrow(h0) != nrow(h1) || nrow(h0) %% 2 != 0) {
    stop(paste("Hashes of tracked columns are expected to be multiples of 16 bits",
               "and the count of tracked columns should remain the same across the",
               "lifetime of the stury"))
  }
  
  offsets <- c(0, 2, 3)
  
  n_col <- nrow(h1) %/% 2L
  
  row_diff_indices <- which(apply(h0 != h1, 2, any))
  for (i_row in row_diff_indices) {
    prev <- as.integer(h0[, i_row])
    cur <- as.integer(h1[, i_row])
    diff <- (prev != cur)
    diff <- apply(matrix(diff, ncol = BYTES_PER_TRACKED_HASH, byrow = TRUE), 1, any)
    evidence <- integer(n_col)
    for (i in seq_len(n_col)){
      v <- diff[[i]]
      affected_indices <- (((i - 1) + offsets) %% n_col) + 1
      delta <- isTRUE(v)
      evidence[affected_indices] <- evidence[affected_indices] + delta
      
      if (verbose) print(evidence)
    }
    inferred_change_count <- ceiling(sum(diff) / length(offsets))
    
    # removes false negatives at the cost of false positives
    threshold <- min(head(sort(evidence, decreasing = TRUE), inferred_change_count))
    col_indices <- which(evidence >= threshold)
    
    for (i_col in col_indices){
      res[[length(res) + 1]] <- c(i_row, i_col)
    }
  }
  return(res)
}
