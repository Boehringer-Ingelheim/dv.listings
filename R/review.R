REV <- pack_of_constants(
  ID = pack_of_constants(
    DROPDOWN = "review_dropdown",
    REVIEW_UI = "review_ui",
    REVIEW_COL = "__review__",
    ROLE_COL = "__role__",
    STATUS_COL = "__status__",
    REVIEW_SELECT = "rev_id",
    ROLE = "rev_role",
    CONNECT_STORAGE = "connect_storage"
  ),
  LABEL = pack_of_constants(
    DROPDOWN = "Annotation",
    REVIEW_COLS = c("Review", "Role", "Status")
  ),
  STATUS_LEVELS = c(
    PENDING = "Pending", 
    UP_TO_DATE = "Up to date", 
    OUTDATED = "Outdated"
  )
)

REV_add_review_columns <- function(ns, data, choices, selected, roles, status) { 
  data <- cbind(selected, roles, status, data) # prepend extra review columns
  names(data)[[1]] <- REV$ID$REVIEW_COL
  names(data)[[2]] <- REV$ID$ROLE_COL
  names(data)[[3]] <- REV$ID$STATUS_COL
  
  return(list(data = data, extra_column_names = REV$LABEL$REVIEW_COLS))
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
    )
  )
  res[["input_ids_to_exclude_from_bookmarking"]] <- c(ns(REV$ID$CONNECT_STORAGE), ns(REV$ID$ROLE))

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
                                   timestamp = rep(0., row_count), 
                                   role = rep(role_factor, row_count), status = rep(status_factor, row_count), 
                                   data_timestamp = rep(0., row_count))
      
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
        
        base_timestamp <- base_info[["timestamp"]]
        data_timestamps <- base_info[["row_timestamps"]]
      } else {
        contents <- RS_compute_base_memory(dataset_review_name, dataset, id_vars, tracked_vars)
        base_info <- RS_parse_base(contents)
        base_timestamp <- base_info[["timestamp"]] # TODO: Consider providing timestamp to RS_compute_base_memory instead?
        data_timestamps <- base_info[["row_timestamps"]]
        writeBin(contents, fname)
      }
      
      dataset_review[["data_timestamp"]] <- data_timestamps
      dataset_review[["timestamp"]] <- base_timestamp
      
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
        role_review <- RS_parse_review_reviews(contents, row_count = nrow(dataset), 
                                               expected_role = role, expected_domain = dataset_review_name)
        # NOTE: and we combine them to display the latest one, but we could...
        # TODO: make reviews by all roles available to the user? (could be done through separate columns)
        update_mask <- role_review[["timestamp"]] > dataset_review[["timestamp"]] 
        
        if (any(update_mask)) {
          review_indices <- role_review[update_mask, ][["review"]]
          dataset_review[update_mask, ][["review"]] <- review[["choices"]][review_indices]
          dataset_review[update_mask, ][["timestamp"]] <- role_review[update_mask, ][["timestamp"]]
          dataset_review[update_mask, ][["role"]] <- role
          dataset_review[update_mask, ][["status"]] <- REV$STATUS_LEVELS[["UP_TO_DATE"]]
        }
      }
      
      outdated_review_mask <- (dataset_review[["timestamp"]] < dataset_review[["data_timestamp"]])
      
      if (any(outdated_review_mask)) {
        dataset_review[outdated_review_mask, ][["status"]] <- REV$STATUS_LEVELS[["OUTDATED"]]
      }
      
      dataset_review[update_mask, ][["timestamp"]] < dataset_review[update_mask, ][["data_timestamp"]]
      
      sub_res[[dataset_review_name]] <-  dataset_review[c("review", "timestamp", "role", "status")]
    }
    res[[dataset_lists_name]] <- sub_res
  }
  
  return(res)
}
    
REV_logic_1 <- function(input, review, datasets) { # TODO: Rename
  # TODO: Flesh out the state machine. Right now there are only default selections for quick iteration
  state <- new.env(parent = emptyenv())
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
  
  return(state)
}

REV_logic_2 <- function(ns, state, input, review, datasets, selected_dataset_list_name, selected_dataset_name, data,
                        dt_proxy) { # TODO: Rename
  shiny::observeEvent(input[[REV$ID$REVIEW_SELECT]], {
    role <- input[[REV$ID$ROLE]]
   
    dataset_list_name <- selected_dataset_list_name() 
    dataset_name <- selected_dataset_name()
    
    info <- input[[REV$ID$REVIEW_SELECT]]
    i_row <- info[["row"]] # TODO: Once there are multiple versions of the dataset, this will require extra steps
    option <- as.integer(info[["option"]])
   
    timestamp <- SH$get_UTC_time_in_seconds()
    contents <- c(
      SH$integer_to_raw(i_row),
      SH$integer_to_raw(option),
      SH$double_to_raw(timestamp)
    )
    
    fname <- paste0(dataset_name, "_", make.names(role), ".review")
    path <- file.path(state[["folder"]], dataset_list_name, fname)
    
    new_data <- data()
    new_data[i_row, ][[REV$ID$REVIEW_COL]] <- review[["choices"]][[option]]
    new_data[i_row, ][[REV$ID$ROLE_COL]] <- role
    new_data[i_row, ][[REV$ID$STATUS_COL]] <- REV$STATUS_LEVELS[["UP_TO_DATE"]]

    # `REV_load_annotation_info()` would return this same (modified) state, but we do manual synchronization
    # to avoid potentially expensive data reloading
    state[["annotation_info"]][[dataset_list_name]][[dataset_name]][i_row, ] <- list(
      review = review[["choices"]][[option]],  timestamp = timestamp, role = role, 
      status = REV$STATUS_LEVELS[["UP_TO_DATE"]]
    )
   
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
