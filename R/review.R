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
  MESSAGE = pack_of_constants(
    LOADING_REVIEW_DATA = "Loading review data...",
    MULTIPLE_REVIEW = "Applying reviews...",
    UNDO_REVIEW = "Undoing latest review..."
  ),
  STATUS_LEVELS = pack_of_constants(
    PENDING = "Pending",
    LATEST_OUTDATED = "Latest Outdated",
    CONFLICT = "Conflict",
    CONFLICT_ROLE = "Conflict I can fix",
    OK = "OK"
  ),
  CONSTANT = pack_of_constants(
    HIGHLIGHT_ALL_TRACKED_COLUMNS_IF_MORE_THAN_N_COLUMNS_HAVE_CHANGED = 4L,
    DEFAULT_REVIEW_VALUE = 1L,
    MULTIPLE_REVIEW_THRESHOLD = 30L
  )
)

REV_show_blocker <- function(id, message, session = shiny::getDefaultReactiveDomain()) {
  session$sendCustomMessage("dv-listings-toggle-dt-processing", list(id = id, show = TRUE, msg = message))
}

REV_hide_blocker <- function(id, session = shiny::getDefaultReactiveDomain()) {
  session$sendCustomMessage("dv-listings-toggle-dt-processing", list(id = id, show = FALSE))
}

REV_time_from_timestamp <- function(v) {  
  non_breaking_hyphen <- "\U2011"
  template <- paste0("%Y", non_breaking_hyphen, "%m", non_breaking_hyphen, "%d %H:%M:%S")
  v <- as.POSIXct(v, origin = "1970-01-01", tz = "UTC")  
  res <- format(v, template)  
  res[is.na(v)] <- ""
  return(res)
}

# Prepends review columns to those of the `data` table
# Prepends review column names to `col_names`
REV_compute_main_review_columns <- function(annotation_info) {
  reviews <- annotation_info[["review"]]
  roles <- annotation_info[["role"]]
  status <- NA_character_
  
  # include review-related columns
  res <- data.frame(reviews, roles) # FIXME: (maybe) Can't pass latest review as argument. List confuses data.frame
  res[["status"]] <- rep(status, nrow(res)) # Explicit `rep` avoids assignment error when `nrow(res) == 0`
  names(res)[[1]] <- REV$ID$REVIEW_COL
  names(res)[[2]] <- REV$ID$ROLE_COL
  names(res)[[3]] <- REV$ID$STATUS_COL
  return(res)
}

# Append columns named "__<NAME_OF_COLUMN>_highlight__" for each of the tracked columns, indicating
# altered cell data. Example:
# >   ID TRACKED_1 TRACKED_2 UNTRACKED __TRACKED_1_highlight__ __TRACKED_2_highlight__ 
# > 1  1         3         3         7                    TRUE                   FALSE 
# > 2  2         4         7        14                   FALSE                   FALSE 
# > 3  3         6         9        21                   FALSE                   FALSE 
# Row 1 of the TRACKED_1 column has been altered after review.
REV_compute_highlight_info <- function(annotation_info, tracked_vars, status) {
  # Compute dataset changes that make current reviews obsolete
  row_col_changes <- local({
    revisions <- attr(annotation_info, "revisions")
    h0 <- REV_collect_latest_review_hashes(
      revisions = revisions, 
      review_timestamps = annotation_info[["timestamp"]]
    )
    h1 <- revisions$tracked_hashes[[length(revisions$tracked_hashes)]]
    
    res <- REV_report_changes(h0, h1)
    for (i_row in seq_along(res)){
      cols <- res[[i_row]][["cols"]]
      if (length(cols) > REV$CONSTANT$HIGHLIGHT_ALL_TRACKED_COLUMNS_IF_MORE_THAN_N_COLUMNS_HAVE_CHANGED)
        res[[i_row]][["cols"]] <- seq_along(tracked_vars) # consider all tracked_vars as modified
    }
    return(res)
  })
  
  highlight_col_names <- paste0("__", sort(tracked_vars), REV$ID$HIGHLIGHT_SUFFIX)
  row_count <- nrow(annotation_info)
  data <- data.frame(matrix(nrow = row_count, ncol = 0))
  for (col_name in highlight_col_names) 
    data[[col_name]] <- rep(FALSE, row_count) # Explicit `rep` avoids assignment error when `nrow(data) == 0`
 
  for (row_cols in row_col_changes){
    row_i <- row_cols[["row"]]
    if (status[[row_i]] == REV$STATUS_LEVELS$OK) {
      stop(sprintf("Detected inconsistency in outdated column highlight calculation (row %d)", row_i))
    }
   
    # TODO? Here we could try highlighting all states other than PENDING and OK, to provide more information based
    #       on the latest review of the current role. So even in the case of a review conflict, the current role would
    #       see what has changed since their last review
    if (status[[row_i]] == REV$STATUS_LEVELS$LATEST_OUTDATED) {
      col_names <- highlight_col_names[row_cols[["cols"]]]
      data[row_i, col_names] <- TRUE
    }
    # FIXME: There is still space for inconsistency between rows tagged as outdated and column highlights.
    #       - Review a row
    #       - Data refresh changes that row
    #       - Second data refresh reverts the change to that row
    #       Outdated row tagging relies on timestamps, so it will flag a change
    #       Highlighting will see no differences in hashes and thus highlight no columns
  }
  
  return(data)
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

  # IMPORTANT: The structure returned by this function is patched in response user actions (see #aimowa)
  #            If this structure changes, those pieces of code will need some work
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
        # See if the new reviews can be appended cleanly to the old ones
        new_contents <- RS_compute_review_codes_memory(review[["choices"]])
        new_review_options_extend_old_ones <- (length(contents) < length(new_contents) && 
                                                 identical(contents, new_contents[seq_along(contents)]))
        if (new_review_options_extend_old_ones) {
          epilogue <- new_contents[(length(contents) + 1):length(new_contents)]
          append_IO_action(list(kind = "write", path = file_path, contents = epilogue, offset = FS$WRITE_OFFSET_APPEND))
        } else {
          choices_diff_report <- local({
            old_choices <- review_info
            new_choices <- review[["choices"]]
            max_len <- max(length(old_choices), length(new_choices))
            length(old_choices) <- max_len
            length(new_choices) <- max_len
            df <- data.frame(`Old choices` = old_choices, `New choices` = new_choices, check.names = FALSE)
            return(capture.output(print(df)))
          })
          undo_table_s <- paste0("<pre style='max-height: 12rem;'>", paste(choices_diff_report, collapse = "<br>"), "</pre>")
          
          error <- c(
            error, 
            paste0(
              "Review choices cannot be removed or reordered during the course of a trial.<br>",
              "Each choice has an associated integer value that should remain constant. These are the old and new ",
              "review choices:<br>",
              undo_table_s,
              "The recommended action is to restore the previous review choices:<br>",
              paste0("<pre>choices = c(", sprintf('"%s"', review_info) |> paste(collapse = ","), ")</pre>"),
              "and append any extra desired choices at the end."
            )
          )
        }
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
                                      data_timestamps = numeric(row_count))
      
      id_vars <- review[["datasets"]][[dataset_review_name]][["id_vars"]]
      tracked_vars <- setdiff(review[["datasets"]][[dataset_review_name]][["tracked_vars"]], id_vars)
     
      base_timestamp <- NA_real_
      data_timestamps_st <- rep(NA_real_, row_count)
      
      # <domain>_0000.base
      # - Older versions of the review functionality devoted three digits to the `.base` and `.delta` sequence numbers.
      #   The current version uses four digits. Here we detect the one that was used for this particular domain (if it
      #   exists) and use it for the associated delta files.
      base_file_path_pattern <- sprintf("^%s_0+.base$", file.path(dataset_lists_name, dataset_review_name))
      base_file_path <- grep(base_file_path_pattern, names(folder_contents), value = TRUE)
      if (length(base_file_path) > 1L) {
        error <- c(error, paste0("[", dataset_review_name, "] ", "Multiple `.base` files found:\n",
                                 paste(sprintf("`%s`", base_file_path), collapse = ", "), ".\n"))
        base_file_path <- sort(base_file_path)[[1]]
      }
      
      if (length(base_file_path) == 1) { # existing `.base` file
        contents <- folder_contents[[base_file_path]]        

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
            new_delta_and_errors <- RS_compute_delta_memory(state = base_info, dataset, review[["allow_row_deletion"]])
           
            error_strings <- new_delta_and_errors[["error"]]
            if (length(error_strings)) { # Error conditions prevent generation of delta files
              error <- c(error, paste0("[", dataset_review_name, "] ", error_strings))
              return(list(error = error))
            } else {
              new_delta_contents <- new_delta_and_errors[["contents"]]
              
              deltas[[length(deltas) + 1]] <- new_delta_contents
              base_info <- RS_load(contents, deltas)
              
              delta_number <- length(sorted_delta_file_paths) + 1
              revision_digit_count <- nchar(sub(".*_(0+)\\.base$", "\\1", base_file_path))
              file_path <- file.path(
                dataset_lists_name, sprintf("%s_%.*d.delta", dataset_review_name, revision_digit_count, delta_number)
              )
              append_IO_action(list(kind = "write", path = file_path, contents = new_delta_contents, offset = 0L))
            }
        }
      } else { # new `.base` file
        base_file_path <- file.path(dataset_lists_name, paste0(dataset_review_name, "_0000.base"))
        contents <- RS_compute_base_memory(dataset_review_name, dataset, id_vars, tracked_vars)
        if (inherits(contents, "simpleCondition")) {
          # IMPORTANT: Not being able to compute the base info is too severe an error to recover from, so we error out
          return(list(error = c(error, contents[["message"]])))
        } else {
          base_info <- RS_load(base = contents, deltas = list())
          append_IO_action(list(kind = "write", path = base_file_path, contents = contents, offset = 0L))
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
      map_canonical_data_into_current_order <- local({
        st_map_df <- st_map_df
        function(data) {
          if (is.data.frame(data)) data[st_map_df, , drop = FALSE]
          else data[st_map_df]
        }
      })
      map_current_indices_into_canonical_order <- local({
        st_map_df <- st_map_df
        function(indices) st_map_df[indices]
      })
      
      # Map data from `_df` order into `_st` order through `data_df[df_map_st]`
      # Map indices from `_st` order into `df` order through `df_map_st[indices_st]`
      # Notice how the "df_" and "_st" prefix and suffix match the type of the operand to their left or right
      df_map_st <- local({ # nolint
        row_count <- ncol(base_info[["id_hashes"]])
        res <- integer(row_count)
        res[st_map_df] <- seq_along(st_map_df)
        return(res)
      }) 
      map_current_data_into_canonical_order <- local({
        df_map_st <- df_map_st
        function(data) {
          if (is.data.frame(data)) data[df_map_st, , drop = FALSE]
          else data[df_map_st]
        }
      })
      map_canonical_indices_into_current_order <- local({
        df_map_st <- df_map_st
        function(indices) df_map_st[indices]
      })
     
      dataset_review_df[["timestamp"]] <- rep(base_timestamp, nrow(dataset_review_df)) # rep for `nrow(...) == 0`
      dataset_review_df[["data_timestamps"]] <- map_canonical_data_into_current_order(data_timestamps_st)
      
      # <domain>_<ROLE>.review      
      all_latest_reviews <- local({
        role_review <- list(review = rep_len(NA_character_, nrow(dataset_review_df)), 
                            timestamp = rep_len(NA_real_, nrow(dataset_review_df)))
        res <- list()
        for (role in review[["roles"]]) res[[role]] <- role_review
        return(res)
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
        
        # Upgrade review files from version 0 to version 1 to support undoing actions
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
        all_latest_reviews <- local({
          reviewed_idx <- which(role_review_df[["timestamp"]] > 0)
          
          reviews_int <- role_review_df[["review"]][reviewed_idx]
          reviews_char <- review[["choices"]][reviews_int]
          all_latest_reviews[[role]][["review"]][reviewed_idx] <- reviews_char
          all_latest_reviews[[role]][["timestamp"]][reviewed_idx] <- role_review_df[["timestamp"]][reviewed_idx]
          return(all_latest_reviews)
        })
      }

      # Add latest roles columns      
      sub_res[[dataset_review_name]] <- dataset_review_df[c("review", "timestamp", "role", "data_timestamps")]
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
      attr(sub_res[[dataset_review_name]], "revisions") <- local({
        res <- base_info[["revisions"]]
        for (i in seq_along(res[["tracked_hashes"]])) { # map from canonical to current order
          res[["tracked_hashes"]][[i]] <- res[["tracked_hashes"]][[i]][, st_map_df, drop = FALSE]
        }
        return(res)
      })
      
      attr(sub_res[[dataset_review_name]], "latest_reviews") <- all_latest_reviews
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
    
REV_loader_state_machine <- function(ns, state, input, review, datasets, fs_client) {
  state[["connected"]] <- shiny::reactiveVal(FALSE)
  state[["contents_ready"]] <- shiny::reactiveVal(FALSE)
  state[["folder"]] <- NULL
  state[["annotation_info"]] <- NULL

  fs_state <- fs_client[["state"]]
  fs_contents <- fs_state[["contents"]]
  
  shiny::observeEvent(input[[REV$ID$CONNECT_STORAGE]], {
    fs_client[["list"]](callback = list_callback)
  }, ignoreNULL = TRUE, ignoreInit = TRUE)

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
      REV_show_blocker(ns(TBL$TABLE_ID), message = paste(REV$MESSAGE$LOADING_REVIEW_DATA))
      on.exit(REV_hide_blocker(ns(TBL$TABLE_ID)))
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
  
  canonical_row_indices <- local({
    # `row_indices` need to be mapped into a base+deltas (stable) indices
    map_current_indices_into_canonical_order <- attr(annotation_info, "map_current_indices_into_canonical_order")
    res <- map_current_indices_into_canonical_order(row_indices)
    return(res)
  })
  
  IO_plan <- REV_produce_IO_plan_for_review_action(
    canonical_row_indices, role, choice_index, timestamp, dataset_list_name, dataset_name
  )
  
  data[[REV$ID$REVIEW_COL]][row_indices] <- choices[[choice_index]]
  data[[REV$ID$ROLE_COL]][row_indices] <- role
    
  latest_reviews <- attr(annotation_info, "latest_reviews")
  latest_reviews[[role]][["review"]][row_indices] <- choices[[choice_index]]
  latest_reviews[[role]][["timestamp"]][row_indices] <- timestamp
  
  attr(annotation_info, "latest_reviews") <- latest_reviews
  
  # `REV_load_annotation_info()` would return this same (modified) state, but we do manual synchronization
  # to avoid potentially expensive data reloading (see #aimowa)
  annotation_info[["review"]][row_indices] <- choices[[choice_index]]
  annotation_info[["timestamp"]][row_indices] <- timestamp
  annotation_info[["role"]][row_indices] <- role
 
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
    review, REV_state, # TODO? Narrow down to what's explicitly needed instead of using the whole `REV_state`
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
    if (any(current_row_indices == 0)) {
      # NOTE: Some of the canonical indices are not present in the current revision of the dataset.
      #       This means we can't display any data associated to them.
      # TODO: Explain the situation to the user?
      #       We haven't done this because it adds some complexity for very little value. We expect most undo actions to
      #       target mistaken bulk actions, and not to target actions that happened on a prior session, while reviewing
      #       an older version of the dataset.
    }
    
    data <- review[["data"]][[dataset_list_name]][[dataset_name]]
    id_vars <- review[["datasets"]][[dataset_name]][["id_vars"]]
    target_data <- data[current_row_indices, ]
    undo_table <- target_data[id_vars]
    #> undo_table[["Previous review"]] <- second_to_last_review_choices # TODO? Would be nice to see the old values, but not mandatory
   
    if (nrow(undo_table) <= 11L) {
      undo_table_s <- utils::capture.output(print(undo_table, row.names = FALSE))
    } else {
      head_rows <- utils::capture.output(print(utils::head(undo_table, n = 5L), row.names = FALSE))
      tail_rows <- utils::capture.output(print(utils::tail(undo_table, n = 5L), row.names = FALSE)) |> utils::tail(n = 5L)
      tail_rows <- utils::tail(tail_rows, n = 5) # discards column names
      undo_table_s <- c(head_rows, sprintf("(omitted %d rows)", nrow(undo_table) - 10L), tail_rows)
    }
    undo_table_s <- paste0("<pre>", paste(undo_table_s, collapse = "<br>"), "</pre>") 
    
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
                                       dt_proxy, fs_execute_IO_plan, fs_state) {
  fs_contents <- fs_state[["contents"]]
  
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
    
    # NOTE: This local computes updates to the state of the app (annotation_info, IO_plan), independent from rendering
    changes_based_on_unfiltered_data <- local({
      info <- input[[REV$ID$REVIEW_SELECT]]
      
      # Replace in full bulk operation
      if ("bulk" %in% names(info) && identical(info[["bulk"]], "filtered")) {
        info[["row"]] <- input[[paste0(TBL$TABLE_ID, "_rows_all")]]
      }
      shiny::req(length(info[["row"]]) > 0)
      
      if (length(info[["row"]]) >= REV$CONSTANT$MULTIPLE_REVIEW_THRESHOLD) {
        REV_show_blocker(ns(TBL$TABLE_ID), message = paste(REV$MESSAGE$MULTIPLE_REVIEW))
        on.exit(REV_hide_blocker(ns(TBL$TABLE_ID)))
      }
      
      annotation_info <- state[["annotation_info"]][[dataset_list_name]][[dataset_name]]
      new_data <- REV_compute_main_review_columns(annotation_info = annotation_info)
      
      timestamp <- SH$get_UTC_time_in_seconds()
      choice_index <- as.integer(info[["option"]])
      
      # TODO: We could send the unfiltered indices along with the information we send to DT
      #       Then we wouldn't have to undo the filtering here
      defiltered_row_indices <- local({
        row_indices <- as.integer(info[["row"]]) # relative to the filtered data sent to the client
        filter_mask <- attr(data(), "filter_mask")
        res <- which(filter_mask)[row_indices]
        return(res)
      })
      
      changes <- REV_compute_review_changes(
        data = new_data, row_indices = defiltered_row_indices, annotation_info = annotation_info, 
        choices = review[["choices"]], choice_index = choice_index, role = role, 
        timestamp = timestamp, dataset_list_name = dataset_list_name, dataset_name = dataset_name
      )
      
      new_data <- changes[["data"]]
      annotation_info <- changes[["annotation_info"]]
      return(list(annotation_info = annotation_info, IO_plan = changes[["IO_plan"]]))
    })
    
    # IMPORTANT: overwrites global state; it has to be the _unfiltered_ annotation_info!
    state[["annotation_info"]][[dataset_list_name]][[dataset_name]] <- 
      changes_based_on_unfiltered_data[["annotation_info"]]
    IO_plan <- changes_based_on_unfiltered_data[["IO_plan"]]
    
    table_data <- list(data = data(), col_names = list())
    table_data <- REV_include_review_interface(
      table_data = table_data,
      annotation_info = state[["annotation_info"]][[dataset_list_name]][[dataset_name]],
      role = role,
      tracked_vars = review[["datasets"]][[selected_dataset_name()]][["tracked_vars"]]
    )
    new_data <- table_data[["data"]]
   
    # If we were doing pure client-side rendering of DT, maybe we could do a lighter upgrade with javascript:
    # > var table = $('#DataTables_Table_0').DataTable();
    # > var tmp = table.row(5).data();
    # > table.columns()[0].length;
    # > tmp[9] = '2';
    # > table.row(5).data(tmp).invalidate();
    rownames(new_data) <- NULL # otherwise row numbers returned from DT are not relative to presented table
    DT::replaceData(dt_proxy, new_data, resetPaging = FALSE, clearSelection = "none")
    
    fs_execute_IO_plan(IO_plan, callback = update_undo_description_callback)
    
    update_undo_resolved_reactives <<- list(
      dataset_list_name = dataset_list_name, dataset_name = dataset_name, role = role
    )
    
    REV_replace_undo_description(ns, "Computing undo description...") # overwritten by #deihee
  })
  
  update_undo_description_callback <- shiny::reactiveVal(0L)
  update_undo_resolved_reactives <- list()
  shiny::observeEvent(update_undo_description_callback(), {
    shiny::req(update_undo_description_callback() > 0L)
    
    error_messages <- fs_state[["error"]]
    if (length(error_messages) > 0) {
      showNotification(
        ui = shiny::HTML(
          paste("<h4>ERROR DURING REVIEW</h4>", paste(paste("\u2022", error_messages), collapse = "<br>"))
        ), duration = NULL, closeButton = TRUE, type = "error"
      )
    }

    undo_desc <- REV_describe_undo_action(
      review = review, REV_state = state, fs_contents = fs_contents,
      dataset_list_name = update_undo_resolved_reactives[["dataset_list_name"]], 
      dataset_name = update_undo_resolved_reactives[["dataset_name"]],
      role = update_undo_resolved_reactives[["role"]]
    )
    REV_replace_undo_description(ns, undo_desc[["text"]]) # overwrites #deihee
  })
  
  shiny::observeEvent(input[[REV$ID$UNDO]], {
    role <- input[[REV$ID$ROLE]]
    
    dataset_list_name <- selected_dataset_list_name()
    dataset_name <- selected_dataset_name()
   
    undo_desc <- REV_describe_undo_action(review, REV_state = state, fs_contents, dataset_list_name, dataset_name, role)
    
    action_count <- length(undo_desc[["info"]][["canonical_indices"]])
    
    shiny::req(action_count > 0)
    
    timestamp <- SH$get_UTC_time_in_seconds()
    
    IO_plan <- REV_produce_IO_plan_for_review_undo_action(undo_desc[["info"]], timestamp, role, dataset_list_name, dataset_name)
    fs_execute_IO_plan(IO_plan, callback = update_table_and_undo_description_callback)
    
    update_table_and_undo_resolved_reactives <<- list(
      dataset_list_name = dataset_list_name, dataset_name = dataset_name, role = role
    )
    REV_replace_undo_description(ns, "Computing undo description...") # overwritten by #eegega
  })
  
  update_table_and_undo_description_callback <- shiny::reactiveVal(0L)
  update_table_and_undo_resolved_reactives <- list()
  shiny::observeEvent(update_table_and_undo_description_callback(), {
    shiny::req(update_table_and_undo_description_callback() > 0L)
    
    error_messages <- fs_state[["error"]]
    if (length(error_messages) > 0) {
      showNotification(
        ui = shiny::HTML(
          paste("<h4>ERROR DURING UNDO</h4>", paste(paste("\u2022", error_messages), collapse = "<br>"))
        ), duration = NULL, closeButton = TRUE, type = "error"
      )
    }

    REV_show_blocker(ns(TBL$TABLE_ID), message = paste(REV$MESSAGE$UNDO_REVIEW))
    on.exit(REV_hide_blocker(ns(TBL$TABLE_ID)))
    
    if (TRUE) { # NOTE: recompute table and reload through DT proxy
      datasets <- review[["data"]]
      
      dataset_list_name <- update_table_and_undo_resolved_reactives[["dataset_list_name"]]
      dataset_name <- update_table_and_undo_resolved_reactives[["dataset_name"]]
      role <- update_table_and_undo_resolved_reactives[["role"]]
      
      load_results <- REV_load_annotation_info(fs_contents, review, datasets)
      state[["annotation_info"]] <- load_results[["loaded_annotation_info"]]

      table_data <- list(data = data(), col_names = list())
      table_data <- REV_include_review_interface(
        table_data = table_data,
        annotation_info = state[["annotation_info"]][[dataset_list_name]][[dataset_name]], # IMPORTANT: Includes changes based on review
        role = role,
        tracked_vars = review[["datasets"]][[selected_dataset_name()]][["tracked_vars"]]
      )
      new_data <- table_data[["data"]]

      rownames(new_data) <- NULL # otherwise row numbers returned from DT are not relative to presented table
      DT::replaceData(dt_proxy, new_data, resetPaging = FALSE, clearSelection = "none")
    }
    
    undo_desc <- REV_describe_undo_action(review, REV_state = state, fs_contents, dataset_list_name, dataset_name, role)
    REV_replace_undo_description(ns, undo_desc[["text"]]) # overwritten by #eegega
  })
  
  return(NULL)
}

REV_review_var_to_json <- function(latest_reviews, data_timestamps) {
  # Output has this format (newlines added for legibility):
  # > '{
  # >   "reviews":{
  # >     "ROLE_1":{"review":"Reviewed with no issues","timestamp":1771937907.9553},
  # >     "ROLE_2":{},"ROLE_3":{},"ROLE_4":{}
  # >    },
  # >   "data_timestamp":1769537142.1378
  # >  }'
 
  elem_count <- length(data_timestamps)
  review_pieces <- list() 
  for (role in names(latest_reviews)){
    na_mask <- is.na(latest_reviews[[role]][["review"]])
    s <- character(elem_count)
    s[na_mask] <- sprintf('"%s":{}', role)
    s[!na_mask] <- sprintf(
      '"%s":{"review":"%s", "timestamp":%.3f}',
      role, latest_reviews[[role]][["review"]][!na_mask], latest_reviews[[role]][["timestamp"]][!na_mask]
    )
    review_pieces <- c(review_pieces, list(s))
  }
  reviews <- do.call(paste, c(review_pieces, sep = ","))
  
  res <- sprintf('{"reviews":{%s},"data_timestamp":%.3f}', reviews, data_timestamps)
  return(res)
}

REV_compute_status <- function(dataset_review, role, latest_reviews_by_role, data_timestamps, modified_row_mask) {
  # Does this function make sense with no role? Yes it does because the latest review is the one that may be outdated,
  # conflicting, unreviewed, etc.
  # Optionally, we could indicate if the current role does have a conflict or is it someone else?
  # We can indicate who conflicts with the latest review
  # Include the button if the selected role has this problem, basically we have a different review and we want to 
  # change to the currently selected.

  # Should conflict only appear with respect to the selected role? Then this column should be recalculated every time.
  # Conflict with me conflict with others?
  # For now we indicate conflicts but not with whom.

  row_count <- nrow(dataset_review)
  
  pending_mask <- dataset_review[[REV$ID$REVIEW_COL]] == levels(dataset_review[[REV$ID$REVIEW_COL]])[[1]] # First level is always default
 
  outdated_latest_mask <- local({
    latest_review_timestamps <- rep(-Inf, row_count)
    for (rev in latest_reviews_by_role){
      latest_review_timestamps <- pmax(latest_review_timestamps, rev[["timestamp"]], na.rm = TRUE)
    }
    res <- (data_timestamps > latest_review_timestamps) & !pending_mask
    return(res)
  })
  
  conflict_with_latest_mask <- local({
    res <- rep_len(FALSE, row_count)
    for (rev in latest_reviews_by_role){
      mask <- dataset_review[[REV$ID$REVIEW_COL]] != rev[["review"]]
      res <- pmax(res, mask, na.rm = TRUE)
    }
    res <- as.logical(res)
    return(res)
  })
  
  conflict_with_role_mask <- local({
    res <- rep_len(FALSE, row_count)
    if (!is.na(role)) {
      role_review <- latest_reviews_by_role[[role]][["review"]]
      res <- ((role_review != REV$CONSTANT$DEFAULT_REVIEW_VALUE) & (role_review != dataset_review[[REV$ID$REVIEW_COL]]))
    }
    return(res)
  })
 
  # NOTE: Assignments in ascending order of priority (0..4)
  # [0] All rows are OK...
  res <- factor(rep(REV$STATUS_LEVELS$OK, length = nrow(dataset_review)), 
                levels = unclass(REV$STATUS_LEVELS)) # Strips POC class away to prevent factor level errors
  # [1] Except for pending rows
  res[pending_mask] <- REV$STATUS_LEVELS$PENDING
  # [2] A conflicting row deserves attention
  res[conflict_with_latest_mask] <- REV$STATUS_LEVELS$CONFLICT
  # [3] A conflict in which the current role participates deserves even more attention
  res[conflict_with_role_mask] <- REV$STATUS_LEVELS$CONFLICT_ROLE
  # [4] But a review based on outdated information is the most relevant
  res[outdated_latest_mask & modified_row_mask] <- REV$STATUS_LEVELS$LATEST_OUTDATED
  
  return(res)
}

# Collect hashes that were known prior to the times indicated by `review_timestamps` 
REV_collect_latest_review_hashes <- function(revisions, review_timestamps) {
  for (th in revisions[["tracked_hashes"]]) {
    if (ncol(th) != length(review_timestamps)) {
      stop(("REV_collect_latest_review_hashes: shape mismatch between `revisions` and `review_timestamps` arguments"))
    }
  }

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


REV_include_review_interface <- function(table_data, annotation_info, role, tracked_vars) {
  main_review_columns <- REV_compute_main_review_columns(annotation_info = annotation_info)

  # NOTE: The purpose of `modified_row_mask` is to address discrepancies between the "Status" column and the 
  # orange highlighting of individual tracked columns:
  # - The contents of the "Status" column are calculated looking only at data and review timestamps. A review
  #   is marked as outdated if it precedes a data change.
  # - The orange highlighting is calculated based on the hash of the _contents_ of the cells, instead.
  #
  # There is a situation in which these two ways of computing review status disagree:
  # - there was a review of row A
  # - there was a dataset update that modified row A
  # - there was a dataset update that reverted row A to the state it had prior to the review
  # 
  # In this case, the review of row A will be tagged as "Outdated" but no highlighting will appear.
  #
  # Here we take a shortcut to address this discrepancy. We create an `modified_row_mask` mask based on the
  # information provided by the column highlighting routine (which is based on the contents of a row) and
  # use it to filter out the misleading "Outdated" tags of rows that have been modified and rolled back.
  modified_row_mask <- local({
    highlight_columns_tmp <- REV_compute_highlight_info(
      annotation_info = annotation_info, 
      tracked_vars = tracked_vars,
      status = rep(REV$STATUS_LEVELS$LATEST_OUTDATED, nrow(main_review_columns))
    )
    return(as.logical(rowSums(highlight_columns_tmp)))
  })
  
  main_review_columns[[REV$ID$STATUS_COL]] <- REV_compute_status(
    dataset_review = main_review_columns, 
    role = role, 
    latest_reviews_by_role = attr(annotation_info, "latest_reviews"), 
    data_timestamps = annotation_info[["data_timestamps"]],
    modified_row_mask
  )
  
  main_review_columns[[REV$ID$LATEST_REVIEW_COL]] <- REV_review_var_to_json(
    latest_reviews = attr(annotation_info, "latest_reviews"), 
    data_timestamps = annotation_info[["data_timestamps"]]
  )
  
  for (i in seq_along(main_review_columns))
    attr(main_review_columns[[i]], "label") <- c(REV$LABEL$REVIEW_COLS[[i]])
  
  highlight_columns <- REV_compute_highlight_info(
    annotation_info = annotation_info, 
    tracked_vars = tracked_vars,
    status = main_review_columns[[REV$ID$STATUS_COL]]
  )
  
  filter_mask <- attr(table_data[["data"]], "filter_mask")
  if (!all(filter_mask)) {
    main_labels <- get_labels(main_review_columns)
    highlight_labels <- get_labels(highlight_columns)
    
    main_review_columns <- main_review_columns[filter_mask, ]
    highlight_columns <- highlight_columns[filter_mask, ]
    
    main_review_columns <- set_labels(main_review_columns, main_labels)
    highlight_columns <- set_labels(highlight_columns, highlight_labels)
  } 
  
  # inject columns into the (possibly) filtered table
  table_data[["col_names"]] <- c(
    REV$LABEL$REVIEW_COLS, 
    names(table_data[["data"]]), 
    names(highlight_columns)
  )
  table_data[["data"]] <- cbind(main_review_columns, table_data[["data"]], highlight_columns)
  
  return(table_data)
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
#' @param err `[environment]`
#' This environment has at least one element named "messages". It is a character vector. Diagnostic messages related to
#' the configuration of the review parameter will be placed here.
#'
#' @param afmm
#' 
#' Pass-through of the server afmm parameter.
#'
#' @export
check_review_parameter <- function(datasets, dataset_names, review, err, afmm = NULL) {
  # NOTE: This function is also used by `dv.tables::mod_tplyr_table`, so think about backwards and forwards 
  #       compatibility in that broader context before modifying it
  if (is.null(review)) return(NULL)
  ok <- CM$assert(
    container = err,
    cond = (checkmate::test_list(review, names = "unique") &&
              checkmate::test_subset(c("datasets", "choices", "roles"), names(review))),
    msg = "`review` should be a list with at least three elements: `datasets`, `choices` and `roles`"
  ) &&
    CM$assert(
      container = err,
      cond = (checkmate::test_list(review[["datasets"]]) &&
                checkmate::test_subset(names(review[["datasets"]]), dataset_names)),
      msg = sprintf(
        "`review$datasets` should be a list and its elements should be named after the following dataset names: %s.",
        paste(dataset_names, collapse = ", ")
      )
    ) &&
    CM$assert(
      container = err,
      cond = checkmate::test_list(review[["datasets"]], names = "unique"),
      msg = local({
        res <- sprintf(
          "`review$datasets` should be a list and its elements should be <b>uniquely</b> named after the 
          following dataset names: %s.<br>", paste(dataset_names, collapse = ", "))
      
        dataset_names <- names(review[["datasets"]])
        repeat_names <- unique(sort(dataset_names[duplicated(dataset_names)]))
        res <- paste0(res, sprintf("However, the following dataset names appear more than once: %s", 
                                   paste(repeat_names, collapse = ",")))
        return(res)
      })
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
    ) &&
    CM$assert(
      container = err,
      cond = checkmate::test_logical(review[["allow_row_deletion"]], len = 1, null.ok = TRUE, any.missing = FALSE),
      msg = "`review$allow_row_deletion` should be TRUE, FALSE or NULL"
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
      
      all_vars <- union(info[["id_vars"]], info[["tracked_vars"]])
      
      encodings <- RS_compute_data_frame_variable_types(dataset, all_vars)
      
      CM$assert(
        container = err,
        cond = !any(encodings == UNKNOWN_VARIABLE_TYPE_ENCODING),
        msg = local({
          res <- "The following variables are of types currently not supported by the review feature:"
          indices <- which(encodings == UNKNOWN_VARIABLE_TYPE_ENCODING)
          for (index in indices){
            var_name <- all_vars[[index]]
            res <- paste(res, var_name, sprintf("(class: %s)", paste(class(dataset[[var_name]]), collapse = ",")))
          }
          
          res <- paste0(res, ".<br>")
          
          supported_data_types <- character(0)
          for (encoding in RS_variable_type_encoding){
            if (encoding[["code"]] != UNKNOWN_VARIABLE_TYPE_ENCODING) { 
              supported_data_types <- c(supported_data_types, encoding[["desc"]])
            }
          }
          res <- paste(res, sprintf("Supported data types are: %s.", paste(supported_data_types, collapse = ", ")))
          
          return(res)
        })
      )
    }
  }
  if (!ok) return(NULL)
 
  # https://en.wikipedia.org/wiki/Filename#Problematic_characters 
  problematic_chars <- c("/", "\\\\", "?", "%", "*", ":", "|", '"', "<", ">", ".", ",", ";", "=")
  problematic_chars_regexp <- paste("([", paste(problematic_chars, collapse = ""), "])")
  
  report_problematic_names <- function(message_template, v) {
    res <- character(0)
    for (e in v){
      matches <- character(0)
      for (pc in problematic_chars) if (grepl(pc, e, fixed = TRUE)) {
        single_char <- substr(pc, 1, 1) # deals with backslash
        matches <- c(matches, single_char)
      }
      if (length(matches)) {
        res <- c(res, sprintf(message_template, e, paste(sprintf("'<b>%s</b>'", matches), collapse = ", ")))
      }
    }
    res <- paste(res, collapse = "<br>")
    return(res)
  }
  if (!is.null(afmm)) {
    dataset_list_names <- names(afmm[["data"]])
    CM$assert(
      container = err,
      cond = !any(grepl(problematic_chars_regexp, dataset_list_names)),
      msg = report_problematic_names(
        paste('The dataset list name "<b>%s</b>" contains characters (%s) incompatible with the review functionality.',
              "That string would be used as part of review-related folder names and those characters could cause problems",
              '<a href="https://en.wikipedia.org/wiki/Filename#Problematic_characters" target="_blank">(details)<a>.',
              "Please, exclude them."),
        dataset_list_names)
    ) 
  }
  dataset_names <- names(review[["datasets"]])
  CM$assert(
    container = err,
    cond = !any(grepl(problematic_chars_regexp, dataset_names)),
    msg = report_problematic_names(
      paste('The dataset name "<b>%s</b>" contains characters (%s) incompatible with the review functionality.',
            "That string would be used as part of review-related file names and those characters could cause problems",
            '<a href="https://en.wikipedia.org/wiki/Filename#Problematic_characters" target="_blank">(details)<a>.',
            "Please, exclude them."),
      dataset_names)
  )
}

REV_check_review_info_parameter <- function(review_info) {
  if (!is.null(review_info)) {
    checkmate::assert(
      checkmate::check_list(review_info),
      checkmate::check_environment(review_info[["state"]]),
      checkmate::check_class(review_info[["role"]], "reactive"),
      checkmate::check_class(review_info[["filter_mask"]], "reactive"),
      combine = "and"
    )
  }
  return(NULL)
}

REV_include_review_info_in_exported_data <- function(export_data, annotation_info, review_role, filter_mask, 
                                                     tracked_vars) {
  # exporting the `status` of the latest review is the most finicky bit of the whole process
  review_reviewer_status_df <- local({
    attr(export_data[["data"]], "filter_mask") <- filter_mask
    res <- REV_include_review_interface(export_data, annotation_info, review_role, tracked_vars)
    return(res[["data"]][c(REV$ID$REVIEW_COL, REV$ID$ROLE_COL, REV$ID$STATUS_COL)])
  })
 
  export_data[["data"]] <- data.frame(review_reviewer_status_df, export_data[["data"]], check.names = FALSE)
  export_data[["col_names"]] <- c(REV$LABEL$REVIEW_COLS[1:3], export_data[["col_names"]])
  return(export_data)
}

REV_include_review_info_in_exported_data_if_available <- function(
    export_data, review_info, dataset_list_name, domain_name, tracked_vars
) {
  # this function resolves all reactives and calls the plain `REV_include_review_info_in_exported_data` function
  review_state <- review_info[["state"]]
  can_export_review_info <- ("contents_ready" %in% names(review_state) && review_state[["contents_ready"]]())
  if (can_export_review_info) {
    review_role <- review_info[["role"]]()
    filter_mask <- review_info[["filter_mask"]]()

    annotation_info <- review_state[["annotation_info"]][[dataset_list_name]][[domain_name]]

    export_data <- shiny::maskReactiveContext(
      REV_include_review_info_in_exported_data(
        export_data, 
        annotation_info = annotation_info,
        review_role = review_role,
        filter_mask = filter_mask,
        tracked_vars = tracked_vars
      )
    )
  }

  return(export_data)
}
