REV <- pack_of_constants(
  ID = pack_of_constants(
    DROPDOWN = "review_dropdown",
    REVIEW_UI = "review_ui",
    REVIEW_COL = "__review__",
    ROLE_COL = "__role__",
    ISSUES_COL = "__issues__",
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
    REVIEW_COLS = c("Review", "Role", "Issues", "Latest Reviews")
  ),
  ISSUES_LEVELS = pack_of_constants(
    PENDING = "Pending",
    OUTDATED = "Outdated",
    CONFLICT = "Conflict",
    CONFLICT_ROLE = "Conflict with current role",
    OK = "OK"
  ),
  DEV_EXTRA_COLS = c(
    "Review time" = "review_time",
    "Data time" = "data_time"
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
  issues <- NA_character_
  latest_reviews <- annotation_info[["latest_reviews"]]
  
  # TODO: Introduce something to this effect
  # > shiny::validate(shiny::need(nrow(data) <= length(reviews), "Error: Inconsistency between review data and loaded datasets"))
 
  # include review-related columns
  res <- data.frame(reviews, roles) # FIXME: (maybe) Can't pass latest review as argument. List confuses data.frame
  res[["issues"]] <- issues
  res[["latest_reviews"]] <- latest_reviews
  names(res)[[1]] <- REV$ID$REVIEW_COL
  names(res)[[2]] <- REV$ID$ROLE_COL
  names(res)[[3]] <- REV$ID$ISSUES_COL
  names(res)[[4]] <- REV$ID$LATEST_REVIEW_COL
  res_col_names <- c(REV$LABEL$REVIEW_COLS)

  review_times <- annotation_info[["timestamp"]]
  unreviewed <- (annotation_info[["timestamp"]] <= attr(annotation_info, "base_timestamp"))
  review_times[unreviewed] <- NA_real_
  res <- cbind(res, review_times)
  names(res)[[length(res)]] <- REV$ID$REVIEW_TIMESTAMP_COL
  res_col_names <- c(res_col_names, "Review time (UTC)")

  res <- cbind(res, annotation_info[["data_timestamp"]])
  names(res)[[length(res)]] <- REV$ID$DATA_TIMESTAMP_COL
  res_col_names <- c(res_col_names, "Data time (UTC)")
 
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
        stop("Impossible to add new review choices")
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
     
      row_count <- nrow(dataset)
     
      default_review <- factor(review[["choices"]][[1]], levels = review[["choices"]])
      dataset_review <- data.frame(review = rep(default_review, row_count),
                                   timestamp = numeric(row_count), 
                                   role = rep(role_factor, row_count), 
                                   data_timestamp = numeric(row_count))
      
      id_vars <- review[["datasets"]][[dataset_review_name]][["id_vars"]]
      untracked_vars <- review[["datasets"]][[dataset_review_name]][["untracked_vars"]]
      tracked_vars <- setdiff(names(dataset), c(id_vars, untracked_vars))
     
      base_timestamp <- NA_real_
      data_timestamps <- rep(NA_real_, row_count)
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
      
      # <domain>_<ROLE>.review      
      all_latest_reviews <- local({
        role_list <- rep_len(list(), length.out = length(review[["roles"]]))
        names(role_list) <- review[["roles"]]
        role_timestamp_list <- list(reviews = role_list, data_timestamp = NULL)
        rep_len(list(role_timestamp_list), length.out = nrow(dataset_review))
      })

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
  }, ignoreNULL = FALSE, ignoreInit = TRUE) # TODO: Remove
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

    last_review_entry <- new_data[i_row, ][[REV$ID$LATEST_REVIEW_COL]][[1]]
    last_review_entry[["reviews"]][[role]][["role"]] <- role
    last_review_entry[["reviews"]][[role]][["review"]] <- review[["choices"]][[option]]
    last_review_entry[["reviews"]][[role]][["timestamp"]] <- timestamp

    # Fixed columns
    new_data[i_row, ][[REV$ID$REVIEW_COL]] <- review[["choices"]][[option]]
    new_data[i_row, ][[REV$ID$ROLE_COL]] <- role
    new_data[i_row, ][[REV$ID$LATEST_REVIEW_COL]][[1]] <- last_review_entry
    # TODO: Benchmark to decide if this is a bottleneck for bigger datasets
    new_data[[REV$ID$ISSUES_COL]] <- REV_compute_issues(new_data, role)
    new_data[[REV$ID$LATEST_REVIEW_COL]] <- REV_review_var_to_json(new_data[[REV$ID$LATEST_REVIEW_COL]])
    
    new_data[i_row, ][[REV$ID$REVIEW_TIMESTAMP_COL]] <- timestamp
    new_data[[REV$ID$REVIEW_TIMESTAMP_COL]] <- REV_time_from_timestamp(new_data[i_row, ][[REV$ID$REVIEW_TIMESTAMP_COL]])
    new_data[[REV$ID$DATA_TIMESTAMP_COL]] <- REV_time_from_timestamp(new_data[i_row, ][[REV$ID$DATA_TIMESTAMP_COL]])
    
    # - data_time does not change when reviewed
   
    # NOTE: Ensure we haven't forgotten some optional column
    implemented_extra_col_names <- c("review_issues", "review_time", "data_time")
    missing_col_implementation <- setdiff(extra_col_names, implemented_extra_col_names)
    if (length(missing_col_implementation)) {
      message(sprintf("Missing implementation for columns: %s", paste(missing_col_implementation, collapse = ", ")))
      browser() # TODO: If you hit this breakpoint, implement the missing column and also patch `row_contents` below
    }
    
    # `REV_load_annotation_info()` would return this same (modified) state, but we do manual synchronization
    # to avoid potentially expensive data reloading
    row_contents <- state[["annotation_info"]][[dataset_list_name]][[dataset_name]][defiltered_i_row, ]
    row_contents[["review"]] <- review[["choices"]][[option]]
    row_contents[["timestamp"]] <- timestamp
    row_contents[["role"]] <- role    
    row_contents[["latest_reviews"]][[1]] <- last_review_entry
    row_contents[["issues"]] <-  REV$ISSUES_LEVELS[["OK"]] # FIXME: A.10
    # > row_contents[["data_timestamp"]] # unchanged
    
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

REV_compute_issues <- function(dataset_review, role) {

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
  res[[REV$ID$ISSUES_COL]] <- factor(rep(REV$ISSUES_LEVELS$OK, length = nrow(res)), levels = REV$ISSUES_LEVELS)
  pending_mask <- dataset_review[[REV$ID$REVIEW_COL]] == levels(dataset_review[[REV$ID$REVIEW_COL]])[[1]] # First level is always default
  outdated_mask <- dataset_review[[REV$ID$DATA_TIMESTAMP_COL]] > dataset_review[[REV$ID$REVIEW_TIMESTAMP_COL]]

  possible_conflict_idx <- which(!pending_mask)
  conflict_with_latest_mask <- rep_len(FALSE, length(pending_mask))
  conflict_with_role_mask <- rep_len(FALSE, length(pending_mask))
  for (idx in possible_conflict_idx) {
    curr_reviews <- dataset_review[[REV$ID$LATEST_REVIEW_COL]][[idx]][["reviews"]]
    latest_review <- dataset_review[[REV$ID$REVIEW_COL]][[idx]]    
    for (role_nm in names(curr_reviews)) {      
      curr_entry <- curr_reviews[[role_nm]]      
      if (!is.null(curr_entry)) { # role_nm has reviewed this row
        conflict_with_latest_mask[[idx]] <- conflict_with_latest_mask[[idx]] || curr_entry[["review"]] != latest_review
        if (!is.na(role) && role_nm == role) {
          conflict_with_role_mask[[idx]] <- curr_entry[["review"]] != latest_review
        }
      }
    }
  }

  res[[REV$ID$ISSUES_COL]][pending_mask] <- REV$ISSUES_LEVELS$PENDING
  res[[REV$ID$ISSUES_COL]][outdated_mask] <- REV$ISSUES_LEVELS$OUTDATED
  res[[REV$ID$ISSUES_COL]][conflict_with_latest_mask & !outdated_mask] <- REV$ISSUES_LEVELS$CONFLICT
  res[[REV$ID$ISSUES_COL]][conflict_with_role_mask & !outdated_mask] <- REV$ISSUES_LEVELS$CONFLICT_ROLE
    
  return(res[[REV$ID$ISSUES_COL]])
}