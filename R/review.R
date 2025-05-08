REV <- pack_of_constants(
  ID = pack_of_constants(
    DROPDOWN = "review_dropdown",
    REVIEW_UI = "review_ui",
    REVIEW_COL = "__review__",
    ROLE_COL = "__role__",
    STATUS_COL = "__status__",
    REVIEW_TIMESTAMP_COL = "__review_timestamp__",
    DATA_TIMESTAMP_COL = "__data_timestamp__",
    DATA_UPDATE_COUNT_COL = "__data_update_count__",
    REVIEW_COUNT_COL = "__review_count__",
    CONFLICT_COL = "__conflict__",
    REVIEW_SELECT = "rev_id",
    ROLE = "rev_role",
    DEV_EXTRA_COLS_SELECT = "dev_extra_cols_select",
    CONNECT_STORAGE = "connect_storage"
  ),
  LABEL = pack_of_constants(
    DROPDOWN = "Annotation",
    REVIEW_COLS = c("Review", "Role")
  ),
  STATUS_LEVELS = c(
    PENDING = "Pending", 
    UP_TO_DATE = "Up to date", 
    OUTDATED = "Outdated"
  ),
  DEV_EXTRA_COLS = c(
    "Review Status" = "review_status",
    "Review time" = "review_time",
    "Data time" = "data_time",
    "Data update count" = "data_update_count",
    "Review count" = "review_count",
    "Conflict" = "conflict"
  )
)

REV_time_from_timestamp <- function(v) {
  non_breaking_hyphen <- "\U2011"
  template <- paste0("%Y", non_breaking_hyphen, "%m", non_breaking_hyphen, "%d %H:%M:%S")
  v <- as.POSIXct(v, origin = "1970-01-01", tz = "UTC")
  res <- format(v, template)
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
  status <- annotation_info[["status"]]
  
  # TODO: Introduce something to this effect
  # > shiny::validate(shiny::need(nrow(data) <= length(reviews), "Error: Inconsistency between review data and loaded datasets"))
 
  # include review-related columns
  res <- data.frame(reviews, roles)
  names(res)[[1]] <- REV$ID$REVIEW_COL
  names(res)[[2]] <- REV$ID$ROLE_COL
  res_col_names <- REV$LABEL$REVIEW_COLS
  
  # add extra requested review-related columns # TODO: table-drive
  for (col in extra_col_names){
    if (col == "review_status") {
      res <- cbind(res, status)
      names(res)[[length(res)]] <- REV$ID$STATUS_COL
      res_col_names <- c(res_col_names, "Review Status")
    } else if (col == "review_time") {
      review_times <- REV_time_from_timestamp(annotation_info[["timestamp"]])
      unreviewed <- (annotation_info[["review_count"]] == 0)
      review_times[unreviewed] <- ""
      res <- cbind(res, review_times)
      names(res)[[length(res)]] <- REV$ID$REVIEW_TIMESTAMP_COL
      res_col_names <- c(res_col_names, "Review time (UTC)")
    } else if (col == "data_time") {
      res <- cbind(res, REV_time_from_timestamp(annotation_info[["data_timestamp"]]))
      names(res)[[length(res)]] <- REV$ID$DATA_TIMESTAMP_COL
      res_col_names <- c(res_col_names, "Data time (UTC)")
    } else if (col == "data_update_count") {
      res <- cbind(res, annotation_info[["data_update_count"]])
      names(res)[[length(res)]] <- REV$ID$DATA_UPDATE_COUNT_COL
      res_col_names <- c(res_col_names, "Data update count")
    } else if (col == "review_count") {
      res <- cbind(res, annotation_info[["review_count"]])
      names(res)[[length(res)]] <- REV$ID$REVIEW_COUNT_COL
      res_col_names <- c(res_col_names, "Review count")
    } else if (col == "conflict") {
      res <- cbind(res, annotation_info[["conflict"]])
      names(res)[[length(res)]] <- REV$ID$CONFLICT_COL
      res_col_names <- c(res_col_names, "Review conflict")
    } else {
      browser()
    }
  }
 
  # add actual data
  res <- cbind(res, data)
  res_col_names <- c(res_col_names, col_names)
  
  return(list(data = res, col_names = res_col_names))
}

REV_UI <- function(ns, roles) {
  choices <- setNames(c("", roles), c("<select reviewer role>", make.names(roles)))

  res <- list()
  res[["ui"]] <- shiny::tagList(
    shiny::div(style = "text-align:right",
               shiny::actionButton(inputId = ns(REV$ID$CONNECT_STORAGE), label = "<Attach folder>"),
    ),
    shiny::selectInput(
      inputId = ns(REV$ID$ROLE), label = "Role:", choices = choices
    ),
    shiny::selectizeInput(
      ns(REV$ID$DEV_EXTRA_COLS_SELECT),
      label = htmltools::HTML("<i style='color:gray;'>[demo] Experimental review columns</i>"),
      choices = REV$DEV_EXTRA_COLS,
      multiple = TRUE,
      options = list(plugins = list("remove_button", "drag_drop"))
    )
  )
  res[["input_ids_to_exclude_from_bookmarking"]] <- c(ns(REV$ID$CONNECT_STORAGE), ns(REV$ID$ROLE), 
                                                      ns(REV$ID$DEV_EXTRA_COLS_SELECT))

  return(res)
}

REV_load_annotation_info <- function(folder, review, dataset_lists) {
  res <- list()
  
  review[["roles"]] <- make.names(review[["roles"]])
  
  for (dataset_lists_name in names(dataset_lists)) {
    sub_res <- list()
    dataset_list <- dataset_lists[[dataset_lists_name]]
    
    dataset_list_folder <- file.path(folder, dataset_lists_name)
    base::dir.create(dataset_list_folder, recursive = TRUE, showWarnings = FALSE)
    
    # review.codes (common to all datasets)
    fname <- file.path(dataset_list_folder, "review.codes")
    if (file.exists(fname)) {
      contents <- readBin(con = fname, raw(), n = file.size(fname), endian = "little")
      review_info <- RS_parse_review_codes(contents)
      if (!identical(review_info, review[["choices"]])) {
        browser() # TODO: Combine new review[["choices"]] with old `review.codes`
        browser() #       while preserving original associated integer codes
      }
    } else {
      contents <- RS_compute_review_codes_memory(review[["choices"]])
      writeBin(contents, fname)
    }
      
    for (dataset_review_name in names(review[["datasets"]])){
      dataset <- dataset_list[[dataset_review_name]]
    
      role_factor <- factor("", levels = c("", review[["roles"]]))
      status_factor <- factor("Pending", levels = REV$STATUS_LEVELS)
     
      row_count <- nrow(dataset)
     
      default_review <- factor(review[["choices"]][[1]], levels = review[["choices"]])
      dataset_review <- data.frame(review = rep(default_review, row_count),
                                   timestamp = numeric(row_count), 
                                   role = rep(role_factor, row_count), 
                                   status = rep(status_factor, row_count), 
                                   data_timestamp = numeric(row_count),
                                   data_update_count = integer(row_count),
                                   review_count = integer(row_count),
                                   conflict = logical(row_count))
      
      id_vars <- review[["datasets"]][[dataset_review_name]][["id_vars"]]
      untracked_vars <- review[["datasets"]][[dataset_review_name]][["untracked_vars"]]
      tracked_vars <- setdiff(names(dataset), c(id_vars, untracked_vars))
     
      base_timestamp <- NA_real_
      data_timestamps <- rep(NA_real_, row_count)
      update_count <- NULL
      # <domain>_000.base
      fname <- file.path(dataset_list_folder, paste0(dataset_review_name, "_000.base"))
      if (file.exists(fname)) {
        contents <- readBin(con = fname, raw(), n = file.size(fname), endian = "little")
        delta_fnames <- list.files(dataset_list_folder, 
                                   pattern = sprintf("^%s_[0-9]*.delta", dataset_review_name),
                                   full.names = TRUE)
        deltas <- local({
          res <- list()
          for (fname in delta_fnames){
            res[[length(res) + 1]] <- readBin(con = fname, raw(), n = file.size(fname), endian = "little")
          }
          return(res)
        })
        base_info <- RS_load(contents, deltas) # TODO? Call this RS_load_memory and write an RS_load() that works with fnames
        dataset_hash <- RS_hash_data_frame(dataset)
        if (!identical(dataset_hash, base_info[["contents_hash"]])) {
          new_delta <- RS_compute_delta_memory(state = base_info, dataset)
          deltas[[length(deltas) + 1]] <- new_delta
          base_info <- RS_load(contents, deltas)
          
          delta_number <- length(delta_fnames) + 1
          fname <- file.path(dataset_list_folder, sprintf("%s_%03d.delta", dataset_review_name, delta_number))
          writeBin(new_delta, fname)
          message(sprintf("Produced new delta %s", fname))
        }
      } else {
        contents <- RS_compute_base_memory(dataset_review_name, dataset, id_vars, tracked_vars)
        base_info <- RS_parse_base(contents)
        writeBin(contents, fname)
        base_timestamp <- base_info[["timestamp"]] # TODO: Consider providing timestamp to RS_compute_base_memory instead?
        data_timestamps <- base_info[["row_timestamps"]]
      }
      
      base_timestamp <- base_info[["timestamp"]]
      data_timestamps <- base_info[["row_timestamps"]]
      update_count <- base_info[["update_count"]]
      
      # This probably should live alongside RS_* functions
      # NOTE(miguel): I didn't consider the possibility of row reordering in the original design of the review file
      #               formats. As a consequence, I missed the need for this row map (that makes it possible to assign
      #               reviews from row indices to id_hashes). This vector could be appended to `delta` files, at the
      #               cost of four bytes per row. I think the superior approach would be to speed up data.frame row 
      #               hashing (by dropping down to C?), as the initial hashing would also benefit from it.
      #               That's why we recompute the hashes here:
      state_to_dataset_row_mapping <- local({ # TODO: Is this the right name?
        id_vars <- base_info[["id_vars"]]
        # FIXME: repeats #ahnail
        id_hashes <- apply(dataset[id_vars], 1, SH$hash_data_frame_row, simplify = TRUE) # coerces all types to be the same (character?)
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
      dataset_review[["data_update_count"]] <- update_count[state_to_dataset_row_mapping]
      
      # <domain>_<ROLE>.review
      for (role in review[["roles"]]){
        fname <- file.path(dataset_list_folder, paste0(dataset_review_name, "_", role, ".review"))
        if (file.exists(fname)) {
          f <- file(fname, "rb")
          contents <- readBin(f, raw(), n = file.size(fname))
          close(f)
        } else { 
          contents <- RS_compute_review_reviews_memory(role, dataset_review_name)
          writeBin(contents, fname)
        }
        
        # NOTE: each role keeps their own decisions...
        role_review <- RS_parse_review_reviews(contents, dataset_to_state_row_mapping = dataset_to_state_row_mapping, 
                                               expected_role = role, expected_domain = dataset_review_name)
        # NOTE: and we combine them to display the latest one, but we could...
        # TODO: ...make reviews by all roles available to the user? (could be done through separate columns)
        update_mask <- (role_review[["timestamp"]] > dataset_review[["timestamp"]])
        
        if (any(update_mask)) {
          review_indices <- role_review[update_mask, ][["review"]]
         
          conflict_mask <- local({ # compute review conflicts
            prev_review_mask <- (dataset_review[["review_count"]] > 0)
            potential_conflict_mask <- (update_mask & prev_review_mask)
            review_diff_mask <- (dataset_review[["review"]] != role_review[["review"]])
            
            return(potential_conflict_mask & review_diff_mask)
          })
          dataset_review[["conflict"]][conflict_mask] <- TRUE
          
          dataset_review[update_mask, ][["review"]] <- review[["choices"]][review_indices]
          dataset_review[update_mask, ][["timestamp"]] <- role_review[update_mask, ][["timestamp"]]
          dataset_review[update_mask, ][["role"]] <- role
          dataset_review[update_mask, ][["status"]] <- REV$STATUS_LEVELS[["UP_TO_DATE"]]
        }
        
        review_mask <- (role_review[["review"]] != 0)
        dataset_review[review_mask, ][["review_count"]] <- 
          dataset_review[review_mask, ][["review_count"]] + role_review[review_mask, ][["count"]]
      }
      
      outdated_review_mask <- (
        (dataset_review[["timestamp"]] < dataset_review[["data_timestamp"]]) &
          (dataset_review[["review_count"]] > 0)
      )
      if (any(outdated_review_mask)) {
        dataset_review[outdated_review_mask, ][["status"]] <- REV$STATUS_LEVELS[["OUTDATED"]]
      }
     
      # FIXME? Mapping attached as a parameter to avoid rewriting prototype-level code
      sub_res[[dataset_review_name]] <- dataset_review[c("review", "timestamp", "role", "status", "data_timestamp",
                                                         "data_update_count", "review_count", "conflict")]
      attr(sub_res[[dataset_review_name]], "state_to_dataset_row_mapping") <- state_to_dataset_row_mapping
    }
    res[[dataset_lists_name]] <- sub_res
  }
  
  return(res)
}
    
REV_logic_1 <- function(state, input, review, datasets) { # TODO: Rename
  # TODO: Flesh out the state machine. Right now there are only default selections for quick iteration
  state[["connected"]] <- shiny::reactiveVal(FALSE)
  state[["folder"]] <- "/tmp" # TODO: Revert to NULL
  state[["annotation_info"]] <- NULL

  shiny::observeEvent(input[[REV$ID$CONNECT_STORAGE]], {
    message("Connecting")
    shiny::updateActionButton(inputId = REV$ID$CONNECT_STORAGE, label = paste("Storage:", state[["folder"]]))
    # TODO: Let the user choose a folder
    
    state[["annotation_info"]] <- REV_load_annotation_info(state[["folder"]], review, datasets)
    
    state[["connected"]](TRUE)
  }, ignoreNULL = FALSE) # TODO: Remove
}

REV_logic_2 <- function(ns, state, input, review, datasets, selected_dataset_list_name, selected_dataset_name, data,
                        dt_proxy) { # TODO: Rename
  shiny::observeEvent(input[[REV$ID$REVIEW_SELECT]], {
    role <- input[[REV$ID$ROLE]]
   
    dataset_list_name <- selected_dataset_list_name() 
    dataset_name <- selected_dataset_name()
    
    new_data <- data()
    
    info <- input[[REV$ID$REVIEW_SELECT]]
    i_row <- info[["row"]]
    
    defiltered_i_row <- local({
      # `i_row` is relative to the filtered data sent to the client ...
      filter_mask <- attr(new_data, "filter_mask")
      res <- which(filter_mask)[[i_row]]
      return(res)
    })
    
    stored_i_row <- local({
      # ... and that `i_row` needs to be mapped into a base+deltas (stable) index
      row_map <- attr(state[["annotation_info"]][[dataset_list_name]][[dataset_name]], "state_to_dataset_row_mapping")
      res <- row_map[[defiltered_i_row]]
      return(res)
    })
    option <- as.integer(info[["option"]])
   
    timestamp <- SH$get_UTC_time_in_seconds()
    contents <- c(
      SH$integer_to_raw(stored_i_row),
      SH$integer_to_raw(option),
      SH$double_to_raw(timestamp)
    )
    
    fname <- paste0(dataset_name, "_", make.names(role), ".review")
    path <- file.path(state[["folder"]], dataset_list_name, fname)
    
    extra_col_names <- input[[REV$ID$DEV_EXTRA_COLS_SELECT]]
    
    # NOTE: Partially repeats #weilae 
    # NOTE: We could cache the modified table and avoid repeating this operation 
    #       if it turns out to be a performance bottleneck
    changes <- REV_include_review_info(
      annotation_info = state[["annotation_info"]][[dataset_list_name]][[dataset_name]],
      data = data(), col_names = list(), extra_col_names = extra_col_names
    )
    new_data <- changes[["data"]]
   
    old_review <- new_data[i_row, ][[REV$ID$REVIEW_COL]]
    old_role <- new_data[i_row, ][[REV$ID$ROLE_COL]]
   
    # Fixed columns 
    new_data[i_row, ][[REV$ID$REVIEW_COL]] <- review[["choices"]][[option]]
    new_data[i_row, ][[REV$ID$ROLE_COL]] <- role
   
    # Optional columns 
    # - review_status
    if (REV$ID$STATUS_COL %in% names(new_data)) {
      new_data[i_row, ][[REV$ID$STATUS_COL]] <- REV$STATUS_LEVELS[["UP_TO_DATE"]]
    }

    # - review_time
    if (REV$ID$REVIEW_TIMESTAMP_COL %in% names(new_data)) {
      new_data[i_row, ][[REV$ID$REVIEW_TIMESTAMP_COL]] <- REV_time_from_timestamp(timestamp)
    }
    
    # - data_time does not change when reviewed
    # - data_update_count does not change when reviewed
   
    # - review_count 
    if (REV$ID$REVIEW_COUNT_COL %in% names(new_data)) {
      new_data[i_row, ][[REV$ID$REVIEW_COUNT_COL]] <- new_data[i_row, ][[REV$ID$REVIEW_COUNT_COL]] + 1
    }
    
    # - conflict
    conflict <- local({
      # TODO: Wrong as it requires knowing the latest decision of each reviewer
      new_review <- new_data[i_row, ][[REV$ID$REVIEW_COL]]
      new_role <- new_data[i_row, ][[REV$ID$ROLE_COL]]
      res <- (old_review != new_review  && old_role != new_role)
      return(res)
    })
    
    if (REV$ID$CONFLICT_COL %in% names(new_data) && isTRUE(conflict)) {
      new_data[i_row, ][[REV$ID$CONFLICT_COL]] <- TRUE
    }
    
    # NOTE: Ensure we haven't forgotten some optional column
    implemented_extra_col_names <- c("review_status", "review_time", "data_time", "data_update_count", "review_count",
                                     "conflict")
    missing_col_implementation <- setdiff(extra_col_names, implemented_extra_col_names)
    if (length(missing_col_implementation)) {
      message(sprintf("Missing implementation for columns: %s", paste(missing_col_implementation, collapse = ", ")))
      browser() # TODO: Of you hit this breakpoint, implement the missing column and also patch `row_contents` below
    }
    
    # `REV_load_annotation_info()` would return this same (modified) state, but we do manual synchronization
    # to avoid potentially expensive data reloading
    row_contents <- state[["annotation_info"]][[dataset_list_name]][[dataset_name]][defiltered_i_row, ]
    row_contents[["review"]] <- review[["choices"]][[option]]
    row_contents[["timestamp"]] <- timestamp
    row_contents[["role"]] <- role
    row_contents[["status"]] <- REV$STATUS_LEVELS[["UP_TO_DATE"]]
    # > row_contents[["data_timestamp"]] # unchanged
    # > row_contents[["data_update_count"]] # unchanged
    row_contents[["review_count"]] <- row_contents[["review_count"]] + 1
    row_contents[["conflict"]] <- conflict
    
    state[["annotation_info"]][[dataset_list_name]][[dataset_name]][defiltered_i_row, ] <- row_contents
   
    # If we were doing pure client-side rendering of DT, maybe we could do a lighter upgrade with javascript:
    # > var table = $('#DataTables_Table_0').DataTable();
    # > var tmp = table.row(5).data();
    # > table.columns()[0].length;
    # > tmp[9] = '2';
    # > table.row(5).data(tmp).invalidate();
    DT::replaceData(dt_proxy, new_data, resetPaging = FALSE, clearSelection = "none")
    
    RS_append(path, contents)
  })
  
  return(NULL)
}
