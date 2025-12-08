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
    UNDO = "undo",
    UNDO_DESCRIPTION_ANCHOR = "undo_description_anchor",
    UNDO_DESCRIPTION = "undo_description",
    ROLE = "rev_role",
    CONNECT_STORAGE = "connect_storage",
    HIGHLIGHT_SUFFIX = "_highlight__",
    APP_ID_prefix = "APP_ID-"
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
  ),
  HIGHLIGHT_ALL_TRACKED_COLUMNS_IF_MORE_THAN_N_COLUMNS_HAVE_CHANGED = 4
)

REV_time_from_timestamp <- function(v) {  
  non_breaking_hyphen <- "\U2011"
  template <- paste0("%Y", non_breaking_hyphen, "%m", non_breaking_hyphen, "%d %H:%M:%S")
  v <- as.POSIXct(v, origin = "1970-01-01", tz = "UTC")  
  res <- format(v, template)  
  res[is.na(v)] <- ""
  return(res)
}

REV_include_review_info <- function(annotation_info, data, col_names) {
  if (nrow(data) < nrow(annotation_info)) {
    filter_mask <- attr(data, "filter_mask")
    annotation_info <- annotation_info[filter_mask, ]
  }

  if (nrow(data) > nrow(annotation_info)) 
    return(
      simpleCondition("Internal error in `REV_include_review_info`: Annotation info has fewer rows than data listing.")
    )
  
  reviews <- annotation_info[["review"]]
  roles <- annotation_info[["role"]]
  status <- NA_character_
  latest_reviews <- annotation_info[["latest_reviews"]]
  
  # include review-related columns
  res <- data.frame(reviews, roles) # FIXME: (maybe) Can't pass latest review as argument. List confuses data.frame
  res[["status"]] <- rep(status, nrow(res)) # Explicit `rep` avoids assignment error when `nrow(res) == 0`
  res[["latest_reviews"]] <- latest_reviews
  names(res)[[1]] <- REV$ID$REVIEW_COL
  names(res)[[2]] <- REV$ID$ROLE_COL
  names(res)[[3]] <- REV$ID$STATUS_COL
  names(res)[[4]] <- REV$ID$LATEST_REVIEW_COL
  res_col_names <- c(REV$LABEL$REVIEW_COLS)
 
  # add actual data
  res <- cbind(res, data)
  res_col_names <- c(res_col_names, col_names)
 
  attributes_to_restore <- setdiff(ls(attributes(data)), c("class", "names"))
  for (e in attributes_to_restore) attr(res, e) <- attr(data, e)

  return(list(data = res, col_names = res_col_names))
}

REV_include_highlight_info <- function(table_data, annotation_info, tracked_vars) {
  data <- table_data[["data"]]
  # Compute dataset changes that make current reviews obsolete
  row_col_changes_st <- local({
    revisions <- attr(annotation_info, "revisions")
    h0 <- REV_collect_latest_review_hashes(
      revisions = revisions, 
      review_timestamps = annotation_info[["timestamp"]]
    )
    h1 <- revisions$tracked_hashes[[length(revisions$tracked_hashes)]]
    
    if (nrow(data) < nrow(annotation_info)) {
      filter_mask <- attr(data, "filter_mask")
      h0 <- h0[, filter_mask, drop = FALSE]
      h1 <- h1[, filter_mask, drop = FALSE]
    }
    
    res <- REV_report_changes(h0, h1)
    for (i_row in seq_along(res)){
      cols <- res[[i_row]][["cols"]]
      if (length(cols) > REV$HIGHLIGHT_ALL_TRACKED_COLUMNS_IF_MORE_THAN_N_COLUMNS_HAVE_CHANGED)
        res[[i_row]][["cols"]] <- seq_len(length(tracked_vars)) # consider all tracked_vars as modified
    }
    return(res)
  })
  
  highlight_col_names <- paste0("__", sort(tracked_vars), REV$ID$HIGHLIGHT_SUFFIX)
  table_data[["col_names"]] <- c(table_data[["col_names"]], highlight_col_names)
  row_count <- nrow(data)
  for (col_name in highlight_col_names) 
    data[[col_name]] <- rep(FALSE, row_count) # Explicit `rep` avoids assignment error when `nrow(data) == 0`
 
  map_canonical_indices_into_current_order <- attr(annotation_info, "map_canonical_indices_into_current_order")
  for (row_cols_st in row_col_changes_st){
    i_row_df <- map_canonical_indices_into_current_order(row_cols_st[["row"]])
    if (i_row_df > 0 && data[[REV$ID$STATUS_COL]][[i_row_df]] == REV$STATUS_LEVELS$LATEST_OUTDATED) {
      col_names <- highlight_col_names[row_cols_st[["cols"]]]
      data[i_row_df, col_names] <- TRUE
    }
  }
  table_data[["data"]] <- data
  
  return(table_data)
}

REV_UI <- function(ns, roles) {
  choices <- stats::setNames(c("", roles), c("<select reviewer role>", roles))

  res <- list()
  res[["ui"]] <- shiny::tagList(
    shiny::div(style = "text-align:right",
               shiny::actionButton(inputId = ns(REV$ID$CONNECT_STORAGE), label = "<Attach folder>"),
    ),
    shiny::selectInput(
      inputId = ns(REV$ID$ROLE), label = "Role:", choices = choices
    )
  )
  res[["input_ids_to_exclude_from_bookmarking"]] <- c(ns(REV$ID$CONNECT_STORAGE), ns(REV$ID$ROLE))

  return(res)
}

REV_load_annotation_info <- function(folder_contents, review, dataset_lists) {
  loaded_annotation_info <- list()

  error <- character()

  IO_plan <- list()
  append_IO_action <- function(action) {    
    IO_plan <<- c(IO_plan, list(action))
  }
  
  for (dataset_lists_name in names(dataset_lists)) {
    sub_res <- list()
    dataset_list <- dataset_lists[[dataset_lists_name]]
    
    # review.codes (common to all datasets)
    file_path <- file.path(dataset_lists_name, "review.codes")
    if (file_path %in% names(folder_contents)) {
      contents <- folder_contents[[file_path]]
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
      append_IO_action(list(kind = "write", path = file_path, contents = contents, offset = 0L))
    }
      
    for (dataset_review_name in names(review[["datasets"]])){
      dataset <- dataset_list[[dataset_review_name]]
    
      role_factor <- factor("", levels = c("", review[["roles"]]))
     
      row_count <- nrow(dataset)
     
      default_review <- factor(review[["choices"]][[1]], levels = review[["choices"]])
      
      # Glossary of variable suffixes:
      # =============================
      # _st: coming from or relative to `state` (contains all rows)
      # _df: coming from or relative to `df` (contains only rows present in currently available data)
      dataset_review_df <- data.frame(review = rep(default_review, row_count),
                                      timestamp = numeric(row_count), 
                                      role = rep(role_factor, row_count), 
                                      data_timestamp = numeric(row_count))
      
      id_vars <- review[["datasets"]][[dataset_review_name]][["id_vars"]]
      tracked_vars <- setdiff(review[["datasets"]][[dataset_review_name]][["tracked_vars"]], id_vars)
     
      base_timestamp <- NA_real_
      data_timestamps_st <- rep(NA_real_, row_count)
      # <domain>_000.base
      file_path <- file.path(dataset_lists_name, paste0(dataset_review_name, "_000.base"))
      if (file_path %in% names(folder_contents)) {
        contents <- folder_contents[[file_path]]        

        sorted_delta_file_paths <- local({
          pattern <- sprintf("^%s_[0-9]*.delta", file.path(dataset_lists_name, dataset_review_name))
          sort(grep(pattern, names(folder_contents), value = TRUE))
        })        
        
        deltas <- local({
          res <- list()
          for (file_path in sorted_delta_file_paths){            
            # TODO: Control for file errors?
            res[[length(res) + 1]] <- folder_contents[[file_path]]
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
            if (length(error_strings)) { # Error conditions prevent generation of delta files
              error <- c(error, paste0("[", dataset_review_name, "] ", error_strings))
            } else {
              new_delta_contents <- new_delta_and_errors[["contents"]]
              
              deltas[[length(deltas) + 1]] <- new_delta_contents
              base_info <- RS_load(contents, deltas)
              
              delta_number <- length(sorted_delta_file_paths) + 1
              file_path <- file.path(dataset_lists_name, sprintf("%s_%03d.delta", dataset_review_name, delta_number))
              append_IO_action(list(kind = "write", path = file_path, contents = new_delta_contents, offset = 0L))
            }
        }
      } else {
        contents <- RS_compute_base_memory(dataset_review_name, dataset, id_vars, tracked_vars)
        if (inherits(contents, "simpleCondition")) {
          # IMPORTANT: Not being able to compute the base info is too severe an error to recover from, so we error out
          return(list(error = c(error, contents[["message"]])))
        } else {
          base_info <- RS_load(base = contents, deltas = list())
          append_IO_action(list(kind = "write", path = file_path, contents = contents, offset = 0L))
        }
      }
      
      base_timestamp <- base_info[["timestamp"]]
      data_timestamps_st <- base_info[["row_timestamps"]]
      
      # This probably should live alongside RS_* functions
      # NOTE(miguel): I didn't consider the possibility of row reordering in the original design of the review file
      #               formats. As a consequence, I missed the need for this row map (that makes it possible to assign
      #               reviews from row indices to id_hashes). This vector could be appended to `delta` files, at the
      #               cost of four bytes per row. I think the superior approach would be to speed up data.frame row 
      #               hashing (by dropping down to C?), as the initial hashing would also benefit from it.
      #               That's why we recompute the hashes here:
      
      # Map data from `_st` order into `_df` order through `data_st[st_map_df]`
      # Map indices from `_df` order into `st` order through `st_map_df[indices_df]`
      # Notice how the "st_" and "_df" prefix and suffix match the type of the operand to their left or right
      st_map_df <- local({
        id_vars <- base_info[["id_vars"]]
        id_hashes <- RS_compute_id_hashes(dataset, id_vars)
        mapping <- match(asplit(id_hashes, 2), asplit(base_info[["id_hashes"]], 2))
        return(mapping)
      })
      map_canonical_data_into_current_order <- function(data) {
        if (is.data.frame(data)) data[st_map_df, , drop = FALSE]
        else data[st_map_df]
      }
      map_current_indices_into_canonical_order <- function(indices) st_map_df[indices]
      
      # Map data from `_df` order into `_st` order through `data_df[df_map_st]`
      # Map indices from `_st` order into `df` order through `df_map_st[indices_st]`
      # Notice how the "df_" and "_st" prefix and suffix match the type of the operand to their left or right
      df_map_st <- local({
        row_count <- ncol(base_info[["id_hashes"]])
        res <- integer(row_count)
        res[st_map_df] <- seq_along(st_map_df)
        return(res)
      }) 
      map_current_data_into_canonical_order <- function(data) {
        if (is.data.frame(data)) data[df_map_st, , drop = FALSE]
        else data[df_map_st]
      }
      map_canonical_indices_into_current_order <- function(indices) df_map_st[indices]
     
      dataset_review_df[["timestamp"]] <- base_timestamp
      dataset_review_df[["data_timestamp"]] <- map_canonical_data_into_current_order(data_timestamps_st)
      
      # <domain>_<ROLE>.review      
      all_latest_reviews_df <- local({
        role_list <- rep_len(list(), length.out = length(review[["roles"]]))
        names(role_list) <- review[["roles"]]
        role_timestamp_list <- list(reviews = role_list, data_timestamp = NULL)
        rep_len(list(role_timestamp_list), length.out = nrow(dataset_review_df))
      })

      for (role in review[["roles"]]){
        file_path <- file.path(dataset_lists_name, paste0(dataset_review_name, "_", role, ".review"))
        if (file_path %in% names(folder_contents)) {          
          contents <- folder_contents[[file_path]]
        } else { 
          contents <- RS_compute_review_reviews_memory(role, dataset_review_name)
          append_IO_action(list(kind = "write", path = file_path, contents = contents, offset = 0L))
        }

        # NOTE: each role keeps their own decisions and we combine them to display the latest one
        row_count <- ncol(base_info[["id_hashes"]])
        role_review_st_v_data <- RS_parse_review_reviews(contents, row_count = row_count,
                                                         expected_role = role, expected_domain = dataset_review_name)
        if (inherits(role_review_st_v_data, "simpleCondition")) {
          # If there's something wrong with prior reviews, we can't add further reviews on top. So, we stop.
          error <- c(error, sprintf("Error while processing `%s`: %s", file_path, role_review_st_v_data[["message"]]))
          return(list(error = error))
        }
        
        # Upgrade review files from version 0 to version 1 so to support undoing actions
        version_number <- role_review_st_v_data[["format_version_number"]]
        if (version_number == 0L) {
          append_IO_action(
            list(kind = "write", path = file_path, offset = 0L, contents = c(charToRaw("LISTREVI"), as.raw(1)))
          )
        }
        
        role_review_st <- role_review_st_v_data[["data"]]
        role_review_df <- map_canonical_data_into_current_order(role_review_st)
        
        # Progressive update of all roles through the mask
        update_mask_df <- (role_review_df[["timestamp"]] > dataset_review_df[["timestamp"]])
        if (any(update_mask_df)) {
          review_indices <- role_review_df[["review"]][update_mask_df]
          dataset_review_df[["review"]][update_mask_df] <- review[["choices"]][review_indices]
          dataset_review_df[["timestamp"]][update_mask_df] <- role_review_df[update_mask_df, ][["timestamp"]]
          dataset_review_df[["role"]][update_mask_df] <- role
        }
        # compact all in lists
        # Replace by list of roles so it is a single column and we can directly iterate over it
        all_latest_reviews_df <- local({          
          reviewed_idx <- which(role_review_df[["timestamp"]] > 0)
          for (idx in reviewed_idx) {
            review_char <- review[["choices"]][role_review_df[["review"]][[idx]]]         
            curr_crr <- list(role = role, review = review_char, timestamp = role_review_df[["timestamp"]][[idx]], reviewed_at_least_once = TRUE)            
            all_latest_reviews_df[[idx]][["reviews"]][[role]] <- curr_crr            
          } 

          for (idx in seq_len(nrow(dataset_review_df))) {            
            all_latest_reviews_df[[idx]][["data_timestamp"]] <- dataset_review_df[["data_timestamp"]][[idx]]
          } 
          all_latest_reviews_df
        })
      }

      dataset_review_df[["latest_reviews"]] <- all_latest_reviews_df
           
      # Add latest roles columns      
      sub_res[[dataset_review_name]] <- dataset_review_df[c("review", "timestamp", "role", "data_timestamp", "latest_reviews")]
      attr(sub_res[[dataset_review_name]], "map_canonical_data_into_current_order") <- 
        map_canonical_data_into_current_order
      attr(sub_res[[dataset_review_name]], "map_current_indices_into_canonical_order") <- 
        map_current_indices_into_canonical_order
      attr(sub_res[[dataset_review_name]], "map_current_data_into_canonical_order") <- 
        map_current_data_into_canonical_order
      attr(sub_res[[dataset_review_name]], "map_canonical_indices_into_current_order") <- 
        map_canonical_indices_into_current_order
      
      attr(sub_res[[dataset_review_name]], "base_timestamp") <- base_timestamp
      # Add tracked_hashes for each revision of the dataset to be able to attribute row changes to specific columns
      attr(sub_res[[dataset_review_name]], "revisions") <- base_info[["revisions"]]
    }
    loaded_annotation_info[[dataset_lists_name]] <- sub_res
  }

  res <- list(
    loaded_annotation_info = loaded_annotation_info,
    IO_plan = IO_plan,
    error = error
  )

  return(res)
}

REV_compute_storage_folder_error_message <- function(paths, app_id) {
  error_message <- character(0)

  direct_children_mask <- (dirname(paths) == ".")
  direct_children_names <- paths[direct_children_mask]
  if (any(endsWith(direct_children_names, ".base")) || any(endsWith(direct_children_names, ".review")) || 
      any(endsWith(direct_children_names, ".codes"))) {
    error_message <- paste(
      "The selected storage folder is a subfolder of the target folder.",
      "Please select its parent instead."
    )
  } else if (any(startsWith(paths, REV$ID$APP_ID_prefix))) {
    storage_app_id_fname <- paths[startsWith(paths, REV$ID$APP_ID_prefix)][[1]]
    storage_app_id <- gsub(paste0("^", REV$ID$APP_ID_prefix), "", storage_app_id_fname)
    if (nchar(app_id) > 0 && # This check allows users that run the application locally to skip this test
        !identical(storage_app_id, app_id)) {
      error_message <- shiny::HTML(
        paste(
          "This storage folder seems to belong to a different application.<br>",
          sprintf("<small>The ID of the <b>current running application</b> is: <tt>%s</tt>.<br>", app_id),
          sprintf("The ID of the <b>application that created that storage folder</b> is: <tt>%s</tt>.<br>", storage_app_id),
          "If the ID of the application as been accidentally updated, you can",
          "ask the application administrator to restore it to its old value.</small>"
        )
      )
    }
  }
  
  return(error_message)
}
    
REV_main_logic <- function(state, input, review, datasets, fs_client) {
  state[["connected"]] <- shiny::reactiveVal(FALSE)
  state[["contents_ready"]] <- shiny::reactiveVal(FALSE)
  state[["folder"]] <- NULL
  state[["annotation_info"]] <- NULL

  fs_state <- fs_client[["state"]]
  fs_contents <- fs_state[["contents"]]
  
  shiny::observeEvent(input[[REV$ID$CONNECT_STORAGE]], {    
    fs_client[["list"]](callback = list_callback)
  }, ignoreNULL = FALSE, ignoreInit = TRUE)

  list_callback <- shiny::reactiveVal(0L)
  shiny::observeEvent(list_callback(), {
    shiny::req(list_callback() > 0L)
    
    connected <- (length(fs_state[["error"]]) == 0)
    state[["connected"]](connected)
    state[["folder"]] <- fs_state[["path"]]
    shiny::updateActionButton(inputId = REV$ID$CONNECT_STORAGE, label = paste("Storage:", state[["folder"]]))        

    if (!isTRUE(connected)) {
      error_message <- "Could not connect to storage"
      if (length(fs_state[["error"]]) > 0) error_message <- paste0(error_message, ": ", fs_state[["error"]][[1]])
      shiny::showNotification(error_message, type = "error")
      state[["annotation_info"]] <- NULL
      state[["contents_ready"]](FALSE) # Edge case where a correct folder was chosen before
      shiny::req(FALSE)
    }
    
    error_message <- REV_compute_storage_folder_error_message(
      paths = rownames(fs_state[["listing"]]), app_id = Sys.getenv("CONNECT_CONTENT_GUID")
    )
    
    if (length(error_message) == 0) {
      listing <- fs_state[["listing"]]
      paths <- rownames(listing[!listing[["isdir"]], ])
      paths_to_read_mask <- (endsWith(paths, ".base") | endsWith(paths, ".delta") | 
                               endsWith(paths, ".review") | endsWith(paths, ".codes"))
      paths_to_read <- paths[paths_to_read_mask]
      fs_client[["read"]](paths = paths_to_read, callback = read_callback)
    } else {
      shiny::showNotification(error_message, duration = NULL, closeButton = TRUE, type = "error")
      state[["annotation_info"]] <- NULL
      state[["folder"]] <- NULL
      shiny::updateActionButton(inputId = REV$ID$CONNECT_STORAGE, label = "Storage:")
      # If we leave the original attach value and the user selects the same folder, the reactiveVal 
      # will optimize the change away and the user will not see the error message a second time.
      list_callback(0L)
    }
  })

  read_callback <- shiny::reactiveVal(0L)
  shiny::observeEvent(read_callback(), {
    shiny::req(read_callback() > 0L)

    load_results <- NULL
    error_messages <- fs_state[["error"]]
    if (length(error_messages) == 0) {
      load_results <- REV_load_annotation_info(fs_contents, review, datasets)
      error_messages <- load_results[["error"]]
    }
      
    if (length(error_messages) > 0) {
      showNotification(
        ui = shiny::HTML(
          paste(
            "<h4>FAILED TO START REVIEW INTERFACE</h4>",
            paste(
              paste("\u2022", error_messages), 
              collapse = "<br>")
          )
        ),
        duration = NULL, closeButton = TRUE, type = "error"
      )
      # NOTE: We remain in this state while we wait for the user to select an appropriate alternative folder
    } else {
      # extend `IO_plan` to write the APP_ID file if necessary
      connect_id <- Sys.getenv("CONNECT_CONTENT_GUID")
      if (nchar(connect_id) > 0) {
        file_name_listing <- rownames(fs_state[["listing"]])
        app_id_fname <- paste0(REV$ID$APP_ID_prefix, connect_id)
        if (!(app_id_fname %in% file_name_listing)) {
          load_results[["IO_plan"]][[length(load_results[["IO_plan"]]) + 1]] <- list(
            kind = "write", path = app_id_fname, offset = 0L, contents = raw(0)
          )
        }
      }
      
      state[["annotation_info"]] <- load_results[["loaded_annotation_info"]]
      fs_client[["execute_IO_plan"]](IO_plan = load_results[["IO_plan"]], callback = execute_IO_plan_callback)
    }
  })

  execute_IO_plan_callback <- shiny::reactiveVal(0L)
  shiny::observeEvent(execute_IO_plan_callback(), {
    shiny::req(execute_IO_plan_callback() > 0L)
    
    error_messages <- fs_state[["error"]]
    if (length(error_messages) > 0) {
      showNotification(
        ui = shiny::HTML(
          paste(
            "<h4>ERROR IN INITIAL READ AND WRITE OPERATION</h4>",
            paste(
              paste("\u2022", error_messages), 
              collapse = "<br>")
          )
        ),
        duration = NULL, closeButton = TRUE, type = "error"
      )
    } else {
      state[["contents_ready"]](TRUE)
    }
  })
}

REV_produce_IO_plan_for_review_action <- function(
    canonical_row_indices, role, choice_index, timestamp, dataset_list_name, dataset_name
) {
  contents <- raw(0)
  for (row_index in canonical_row_indices) {
    contents <- c(
      contents,
      SH$integer_to_raw(row_index),
      SH$integer_to_raw(choice_index),
      SH$double_to_raw(timestamp)
    )
  }
  
  IO_plan <- list(
    list(
      kind = "write",
      path = file.path(dataset_list_name, paste0(dataset_name, "_", role, ".review")),
      contents = contents,
      offset = FS$WRITE_OFFSET_APPEND
    )
  )
  
  return(IO_plan)
}

# Testing on 0-row, 1-row and multi-row inputs would have uncovered some bugs we've already addressed
REV_compute_review_changes <- function(data, row_indices, annotation_info, choices, choice_index, role, timestamp,
                                       dataset_list_name, dataset_name) {
  res <- list()
  
  defiltered_row_indices <- local({
    # `row_indices` relative to the filtered data sent to the client ...
    filter_mask <- attr(data, "filter_mask")
    res <- which(filter_mask)[row_indices]
    return(res)
  })
  
  canonical_row_indices <- local({
    # ... and that `row_indices` needs to be mapped into a base+deltas (stable) index
    map_current_indices_into_canonical_order <- attr(annotation_info, "map_current_indices_into_canonical_order")
    res <- map_current_indices_into_canonical_order(defiltered_row_indices)
    return(res)
  })
  
  IO_plan <- REV_produce_IO_plan_for_review_action(
    canonical_row_indices, role, choice_index, timestamp, dataset_list_name, dataset_name
  )
  
  # NOTE: We could cache the modified table and avoid repeating this operation 
  #       if it turns out to be a performance bottleneck
  # TODO: This loop can be too long when there are too many rows
  # Writing is done in one step but by row update is done one by one.
  for (i in seq_along(row_indices)) {
    curr_i_row <- row_indices[[i]]
    curr_defiltered_i_row <- defiltered_row_indices[[i]]
    
    last_review_entry <- data[curr_i_row, ][[REV$ID$LATEST_REVIEW_COL]][[1]]
    last_review_entry[["reviews"]][[role]][["role"]] <- role
    last_review_entry[["reviews"]][[role]][["review"]] <- choices[[choice_index]]
    last_review_entry[["reviews"]][[role]][["timestamp"]] <- timestamp
    
    # Fixed columns
    data[curr_i_row, ][[REV$ID$REVIEW_COL]] <- choices[[choice_index]]
    data[curr_i_row, ][[REV$ID$ROLE_COL]] <- role
    data[curr_i_row, ][[REV$ID$LATEST_REVIEW_COL]][[1]] <- last_review_entry
    
    # - data_time does not change when reviewed
    
    # `REV_load_annotation_info()` would return this same (modified) state, but we do manual synchronization
    # to avoid potentially expensive data reloading
    row_contents <- annotation_info[curr_defiltered_i_row, ]
    row_contents[["review"]] <- choices[[choice_index]]
    row_contents[["timestamp"]] <- timestamp
    row_contents[["role"]] <- role    
    row_contents[["latest_reviews"]][[1]] <- last_review_entry
    # > row_contents[["data_timestamp"]] # unchanged
    
    annotation_info[curr_defiltered_i_row, ] <- row_contents
  }
  
  res[["data"]] <- data 
  res[["annotation_info"]] <- annotation_info
  res[["IO_plan"]] <- IO_plan
  
  return(res)
}

REV_compute_undo_action_info <- function(contents, role, domain) {
  internal_res <- RS_parse_review_reviews_and_apply_undo(contents, expected_role = role, expected_domain = domain)
  canonical_indices <- internal_res[["canonical_indices"]]
  review_indices <- internal_res[["review_indices"]]
  timestamps  <- internal_res[["timestamps"]]
  
  res <- list(canonical_indices = integer(0), review_decision = NULL, timestamp = NULL)
  if (length(timestamps) > 0) {
    last_review_index <- review_indices[[length(timestamps)]]
    last_timestamp <- timestamps[[length(timestamps)]]
    
    last_timestamp <- timestamps[[length(timestamps)]]
    last_review_index <- review_indices[[length(timestamps)]]
    last_action_indices <- which(timestamps == last_timestamp & review_indices == last_review_index)
    
    ; if (length(last_action_indices) > 1) {
      contiguous <- (all(diff(last_action_indices)) == 1)
      if (!isTRUE(contiguous)) {
        error_message <-  paste0("Found several actions to undo, but they are not contiguous.<br>",
                                 "This is somewhat unexpected, so the undo functionality has been disabled.<br>",
                                 "If you believe this is a problem, please contact the package maintainer.")
        return(simpleCondition(error_message)) # NOTE: Early out
      }
    }
    
    res <- list(
      canonical_indices = canonical_indices[last_action_indices],
      review_decision = last_review_index,
      timestamp = last_timestamp
    )
  }
  
  return(res)
}

REV_describe_undo_action <- function(
    review, REV_state, # TODO? Narrow down to what's explicitly needed
    fs_contents, dataset_list_name, dataset_name, role) {
  
  review_path <- file.path(dataset_list_name, sprintf("%s_%s.review", dataset_name, role))
  contents <- fs_contents[[review_path]]
  
  res <- list(
    text = character(0),
    info = REV_compute_undo_action_info(contents = contents, role = role, domain = dataset_name)
  )
  
  if (inherits(res[["info"]], "simpleCondition")) {
    res[["text"]] <- shiny::HTML(res[["info"]][["message"]])
  } else if (length(res[["info"]][["canonical_indices"]]) == 0) {
    res[["text"]] <- "No action to undo"
  } else {
    canonical_indices <- res[["info"]][["canonical_indices"]]
    current_row_index_from_canonical_row_index <- attr(
      REV_state[["annotation_info"]][[dataset_list_name]][[dataset_name]], "map_canonical_indices_into_current_order"
    )
    current_row_indices <- current_row_index_from_canonical_row_index(canonical_indices)
    if (any(is.na(current_row_indices))) {
      browser() # TODO: Not all indices are necessarily present. We can't show those that are not because we no longer
      # have access to the original data, so we should filter them out
    }
    
    data <- review[["data"]][[dataset_list_name]][[dataset_name]]
    id_vars <- review[["datasets"]][[dataset_name]][["id_vars"]]
    target_data <- data[current_row_indices, ]
    undo_table <- target_data[id_vars]
    #> undo_table[["Previous review"]] <- second_to_last_review_choices # TODO? Would be nice to see the old values, but not mandatory
    
    undo_table_s <- capture.output(print(undo_table, row.names = FALSE))
    undo_table_s <- sprintf("<pre>%s</pre>", paste(undo_table_s, collapse = "<br>"))
    
    # TODO: Replace ID column names with labels if available
    
    last_review_choice <- review[["choices"]][[res[["info"]][["review_decision"]]]]
    last_timestamp <- res[["info"]][["timestamp"]]
    time <- structure(last_timestamp, class = c("POSIXct", "POSIXt"), tzone = "UTC")
    undo_header <- paste('<p style="margin:10px">', "Marked as <b>", last_review_choice,
                         "</b> on <b>", time, "UTC</b></p>")
    
    text <- shiny::HTML(paste(undo_header, undo_table_s))
    res[["text"]] <- text
  }
  
  return(res)
}

REV_serialize_undo_action <- function(undo_info, timestamp) {
  UNDO_MARKER <- 0L 
  action_count <- length(undo_info[["canonical_indices"]])
 
  canonical_indices <- undo_info[["canonical_indices"]][[1]]
  review_decision <- undo_info[["review_decision"]]
  original_timestamp <- undo_info[["timestamp"]]
  
  contents <- c(
    # FIRST HALF
    SH$integer_to_raw(UNDO_MARKER),
    SH$integer_to_raw(action_count),
    SH$double_to_raw(timestamp),
    # SECOND HALF
    SH$integer_to_raw(-canonical_indices[[1]]),
    SH$integer_to_raw(review_decision),
    SH$double_to_raw(original_timestamp)
  )
  
  return(contents)
}

REV_produce_IO_plan_for_review_undo_action <- function(undo_info, timestamp, role, dataset_list_name, dataset_name) {
  contents <- REV_serialize_undo_action(undo_info = undo_info, timestamp)
  
  IO_plan <- list(
    list(
      kind = "write",
      path = file.path(dataset_list_name, paste0(dataset_name, "_", role, ".review")),
      contents = contents,
      offset = FS$WRITE_OFFSET_APPEND
    )
  )
    
  return(IO_plan)
}


REV_replace_undo_description <- function(ns, contents) {
  shiny::removeUI(selector = paste0("#", ns(REV$ID$UNDO_DESCRIPTION)))
  shiny::insertUI(selector = paste0("#", ns(REV$ID$UNDO_DESCRIPTION_ANCHOR)), where = "afterEnd", 
                  ui = shiny::div(contents, id = ns(REV$ID$UNDO_DESCRIPTION))
  )
}

REV_respond_to_user_review <- function(ns, state, input, review, selected_dataset_list_name, selected_dataset_name, data,
                                       dt_proxy, fs_execute_IO_plan, fs_contents) {
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

    i_rows <- as.numeric(info[["row"]])
    annotation_info <- state[["annotation_info"]][[dataset_list_name]][[dataset_name]]
    
    changes <- REV_include_review_info(annotation_info = annotation_info, data = new_data, col_names = list())
    if (inherits(changes, "simpleCondition")) {
      shiny::showNotification(changes[["message"]], type = "warning")
      warning(changes[["message"]])
      shiny::req(FALSE)
    }
    
    new_data <- changes[["data"]]
    timestamp <- SH$get_UTC_time_in_seconds()
    choice_index <- as.integer(info[["option"]])
    
    changes <- REV_compute_review_changes(
      data = new_data, row_indices = i_rows, annotation_info = annotation_info, 
      choices = review[["choices"]], choice_index = choice_index,  role = role, 
      timestamp = timestamp, dataset_list_name = dataset_list_name, dataset_name = dataset_name
    )
    
    new_data <- changes[["data"]]
    annotation_info <- changes[["annotation_info"]]
    state[["annotation_info"]][[dataset_list_name]][[dataset_name]] <- annotation_info
    IO_plan <- changes[["IO_plan"]]

    # TODO: Benchmark to decide if this is a bottleneck for bigger datasets
    new_data[[REV$ID$STATUS_COL]] <- REV_compute_status(new_data, role)
    new_data[[REV$ID$LATEST_REVIEW_COL]] <- REV_review_var_to_json(new_data[[REV$ID$LATEST_REVIEW_COL]])

    new_data <- local({
      # TODO: rewrite REV_include_highlight_info to avoid this clumsy wrapper
      table_data <- list(data = new_data, col_names = character(0))
      table_data <- REV_include_highlight_info(
        table_data, annotation_info, 
        tracked_vars = review[["datasets"]][[dataset_name]][["tracked_vars"]]
      )
      return(table_data[["data"]])
    })
   
    # If we were doing pure client-side rendering of DT, maybe we could do a lighter upgrade with javascript:
    # > var table = $('#DataTables_Table_0').DataTable();
    # > var tmp = table.row(5).data();
    # > table.columns()[0].length;
    # > tmp[9] = '2';
    # > table.row(5).data(tmp).invalidate();
    DT::replaceData(dt_proxy, new_data, resetPaging = FALSE, clearSelection = "none")
   
    fs_execute_IO_plan(IO_plan)
   
    undo_desc <- REV_describe_undo_action(review, REV_state = state, fs_contents, 
                                          selected_dataset_list_name(), selected_dataset_name(), role)
    
    REV_replace_undo_description(ns, undo_desc[["text"]])
  })
  
  shiny::observeEvent(input[[REV$ID$UNDO]], {
    role <- input[[REV$ID$ROLE]]
    
    dataset_list_name <- selected_dataset_list_name()
    dataset_name <- selected_dataset_name()
   
    undo_desc <- REV_describe_undo_action(review, REV_state = state, fs_contents, dataset_list_name, dataset_name, role)
    
    action_count <- length(undo_desc[["info"]][["canonical_indices"]])
    
    shiny::req(action_count > 0)
    
    timestamp <- SH$get_UTC_time_in_seconds()
    
    contents <- REV_serialize_undo_action(undo_info = undo_desc[["info"]], timestamp)
    
    IO_plan <- list(
      list(
        kind = "write",
        path = file.path(dataset_list_name, paste0(dataset_name, "_", role, ".review")),
        contents = contents,
        offset = FS$WRITE_OFFSET_APPEND
      )
    )
    
    fs_execute_IO_plan(IO_plan)
    
    # NOTE: compute data and reload through proxy
    if (TRUE) { # FIXME: Partially repeats #weilae 
      datasets <- review[["data"]]
      load_results <- REV_load_annotation_info(fs_contents, review, datasets)
      state[["annotation_info"]] <- load_results[["loaded_annotation_info"]]
      annotation_info <- state[["annotation_info"]][[dataset_list_name]][[dataset_name]]
      
      data <- data()
      changes <- REV_include_review_info(annotation_info = annotation_info, data = data, col_names = list())
      shiny::validate(shiny::need(!inherits(changes, "simpleCondition"), changes[["message"]]))
      
      changes[["data"]][[REV$ID$STATUS_COL]] <- REV_compute_status(changes[["data"]], role)
      changes[["data"]][[REV$ID$LATEST_REVIEW_COL]] <- REV_review_var_to_json(changes[["data"]][[REV$ID$LATEST_REVIEW_COL]])        
      
      data <- changes[["data"]]
      
      # TODO: rewrite REV_include_highlight_info to avoid this clumsy wrapper
      table_data <- list(data = data, col_names = character(0))
      table_data <- REV_include_highlight_info(
        table_data, annotation_info, tracked_vars = review[["datasets"]][[dataset_name]][["tracked_vars"]]
      )
      
      DT::replaceData(dt_proxy, table_data[["data"]], resetPaging = FALSE, clearSelection = "none")
    }
    
    undo_desc <- REV_describe_undo_action(review, REV_state = state, fs_contents, dataset_list_name, dataset_name, role)
    REV_replace_undo_description(ns, undo_desc[["text"]])
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

  # Does this function make sense with no role? Yes it does because the latest review is the one that may be outdated,
  # conflicting, unreviewed, etc.
  # Optionally, we could indicate if the current role does have a conflict or is it someone else?
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
    threshold <- min(utils::head(sort(evidence, decreasing = TRUE), inferred_change_count))
    col_indices <- which(evidence >= threshold)
    
    res[[length(res) + 1]] <- list(row = i_row, cols = col_indices)
  }
  return(res)
}

#' Early error feedback function for the optional review parameter
#'
#' @param datasets `[list(data.frame)]`
#'
#' Available datasets for review.
#' 
#' @param dataset_names `[character(n)]`
#'
#' Names of the datasets provided by the previous parameter.
#'   
#' @param review `[list()]`
#' 
#' Configuration of the experimental data review feature. Please refer to `vignette("data_review")`.
#'
#' @param error `[environment]`
#' This environment has at least one element named "messages". It is a character vector. Diagnostic messages related to
#' the configuration of the review parameter will be placed here.
#' 
#' @export
check_review_parameter <- function(datasets, dataset_names, review, err) {
  if (is.null(review)) return(NULL)
  ok <- CM$assert(
    container = err,
    cond = (checkmate::test_list(review, names = "unique") &&
              checkmate::test_subset(c("datasets", "choices", "roles"), names(review))),
    msg = "`review` should be a list with at least three elements: `datasets`, `choices` and `roles`"
  ) &&
    CM$assert(
      container = err,
      cond = (checkmate::test_list(review[["datasets"]], names = "unique") &&
                checkmate::test_subset(names(review[["datasets"]]), dataset_names)),
      msg = sprintf(
        "`review$datasets` should be a list and its elements should be named after the following dataset names: %s",
        paste(dataset_names, collapse = ", ")
      )
    ) &&
    CM$assert(
      container = err,
      cond = checkmate::test_character(review[["choices"]], min.len = 1, min.chars = 1, unique = TRUE),
      msg = "`review$choices` should be a non-empty character vector of unique, non-empty strings"
    ) &&
    CM$assert(
      container = err,
      cond = checkmate::test_character(review[["roles"]], min.len = 1, min.chars = 1, unique = TRUE),
      msg = "`review$roles` should be a non-empty character vector of unique, non-empty strings"
    )
  
  if (!ok) return(NULL)
  for (domain in names(review[["datasets"]])){
    info <- review[["datasets"]][[domain]]            
    
    dataset <- datasets[[domain]]
    
    vars_OK <- CM$assert(
      container = err,
      cond = (checkmate::test_list(review, names = "unique") &&
                checkmate::test_subset(c("id_vars", "tracked_vars"), names(info))),
      msg = sprintf("`review$datasets$%s` should be a list with two elements named `id_vars` and `tracked_vars`",
                    domain)
    ) &&
      CM$assert(
        container = err,
        cond = (checkmate::test_character(info[["id_vars"]], min.len = 1, min.chars = 1, unique = TRUE) &&
                  checkmate::test_subset(info[["id_vars"]], names(dataset))),
        msg = sprintf(
          paste(
            "`review$datasets$%s$id_vars` should be a character vector listing a subset of the columns",
            "available in dataset `%s`"
          ), domain, domain
        )
      ) &&
      CM$assert(
        container = err,
        cond = nrow(dataset[info[["id_vars"]]]) == nrow(unique(dataset[info[["id_vars"]]])),
        msg = sprintf("`review$datasets$%s$id_vars` should identify uniquely every row of the dataset `%s`", 
                      domain, domain)
      ) &&
      CM$assert(
        container = err,
        cond = (checkmate::test_character(info[["tracked_vars"]], min.chars = 1, min.len = 3, unique = TRUE) &&
                  checkmate::test_subset(info[["tracked_vars"]], names(dataset))),
        msg = sprintf(
          paste(
            "`review$datasets$%s$tracked_vars` should be a character vector listing a subset of",
            " at least three columns available in dataset `%s`"
          ), domain, domain
        )
      )
    
    if (vars_OK) {
      common_vars <- intersect(info[["id_vars"]], info[["tracked_vars"]])
      
      CM$assert(
        container = err,
        cond = length(common_vars) == 0,
        msg = sprintf(
          paste(
            "Variables should be assigned <b>exclusively</b> to either <tt>review$datasets$%s$id_vars</tt> or",
            "<tt>review$datasets$%s$tracked_vars</tt>. However both of those parameters include the following variables:",
            "%s.<br>If those are indeed variables that uniquely identify dataset rows and are not subject to", 
            "change, our recommendation is that they are preserved as <tt>id_vars</tt> and excluded from <tt>tracked_vars</tt>."
          ), domain, domain, paste(sprintf("`%s`", common_vars), collapse = ", ")
        )
      )
      
    }
  }
}

