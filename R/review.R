REV <- pack_of_constants(
  ID = pack_of_constants(
    DROPDOWN = "review_dropdown",
    REVIEW_UI = "review_ui",
    REVIEW_COL = "__review__",
    REVIEW_SELECT = "rev_id",
    ROLE = "rev_role",
    CONNECT_STORAGE = "connect_storage"
  ),
  LABEL = pack_of_constants(
    DROPDOWN = "Annotation"
  )
)

# TODO: Compress with REV_add_review_column
REV_patch_select <- function(ns, row_index, choices, selected) {
  options <- paste(
    "<option value='0'>---</option>\n",
    paste(
      sprintf("<option value='%d'%%s>%s</option>", seq_len(length(choices)), choices),
      collapse = "\n"
    )
  )

  sprintf_args <- list(options) 
  for (i_pos in seq_len(length(choices))){
    sprintf_args[[length(sprintf_args) + 1]] <- ifelse(selected == i_pos, " selected", "")
  }

  options <- do.call(sprintf, sprintf_args)

  dropdown_column_template <- paste(
    r"---(<select style='width:100%%' onchange="Shiny.setInputValue('%s', {row:%d, option:this.value});">)---",
    "\n", options, "\n",
    "</select>"
  )

  res <- sprintf(dropdown_column_template, ns(REV$ID$REVIEW_SELECT), row_index)
  return(res)
}

# TODO: Compress with REV_patch_select
REV_add_review_column <- function(ns, data, choices, selected) { 
  data[[REV$ID$REVIEW_COL]] <- ""
 
  review_column_heading <- "Review"
  
  options <- paste(
    "<option value='0'>---</option>\n",
    paste(
      sprintf("<option value='%d'%%s>%s</option>", seq_len(length(choices)), choices),
      collapse = "\n"
    )
  )
 
  sprintf_args <- list(options) 
  for (i_pos in seq_len(length(choices))){
    sprintf_args[[length(sprintf_args) + 1]] <- ifelse(selected == i_pos, " selected", "")
  }
  
  options <- do.call(sprintf, sprintf_args)
  
  dropdown_column_template <- paste(
    r"---(<select style='width:100%%' onchange="Shiny.setInputValue('%s', {row:%d, option:this.value});">)---",
    "\n", options, "\n",
    "</select>"
  )
  
  data[[REV$ID$REVIEW_COL]] <- sprintf(dropdown_column_template, ns(REV$ID$REVIEW_SELECT), seq_len(nrow(data)))
  
  return(list(data = data, extra_column_names = review_column_heading))
}

REV_UI <- function(ns, roles) {
  # TODO: Exclude these inputs from bookmarking
  
  choices <- setNames(c("", roles), c("---", make.names(roles)))
  
  res <- shiny::tagList(
    shiny::div(style = "text-align:right",
               shiny::actionButton(inputId = ns(REV$ID$CONNECT_STORAGE), label = "<Attach folder>"),
    ),
    shiny::selectInput(
      inputId = ns(REV$ID$ROLE), label = "Role:", choices = choices,
      selected = roles[[1]] # TODO: Remove
    )
  )
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
    
    for (dataset_review_name in names(review[["datasets"]])){
      dataset <- dataset_list[[dataset_review_name]]
      
      row_count <- nrow(dataset)
      dataset_review <- data.frame(review = rep(0L, row_count), timestamp = rep(0., row_count))
      
      id_vars <- review[["datasets"]][[dataset_review_name]][["id_vars"]]
      tracked_vars <- review[["datasets"]][[dataset_review_name]][["tracked_vars"]]
      
      # <domain>_000.base
      fname <- file.path(dataset_list_folder, paste0(dataset_review_name, "_000.base"))
      if (file.exists(fname)) {
        # TODO: Read header, compute full dataset hash, check for change
        # TODO: If dataset has changed, produce delta
      } else {
        contents <- RS_compute_base_memory(dataset_review_name, dataset, id_vars, tracked_vars)
        writeBin(contents, fname)
      }
      
      # review.codes
      fname <- file.path(dataset_list_folder, "review.codes")
      if (file.exists(fname)) {
        # TODO: Read and check?
      } else {
        contents <- RS_compute_review_codes_memory(review[["choices"]])
        writeBin(contents, fname)
      }
      
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
        # TODO: make them all available to the user
        update_mask <- role_review[["timestamp"]] > dataset_review[["timestamp"]] 
        dataset_review[update_mask, ] <- role_review[update_mask, ]
      }
      sub_res[[dataset_review_name]] <- dataset_review[["review"]]
    }
    res[[dataset_lists_name]] <- sub_res
  }
  
  return(res)
}
    
REV_logic_1 <- function(input, review, datasets) { # TODO: Rename
  # TODO: Flesh out the state machine. Right now there are only default selections for quick iteration
  state <- shiny::reactiveValues(
    connected = FALSE,
    folder = "/tmp", # TODO: Revert to NULL
    annotation_info = NULL
  )
  
  shiny::observeEvent(input[[REV$ID$CONNECT_STORAGE]], {
    message("Connecting")
    shiny::updateActionButton(inputId = REV$ID$CONNECT_STORAGE, label = paste("Storage:", state[["folder"]]))
    # TODO: Let the user choose a folder
    
    state[["annotation_info"]] <- REV_load_annotation_info(state[["folder"]], review, datasets)
    
    state[["connected"]] <- TRUE
  }, ignoreNULL = FALSE) # TODO: Remove
  
  return(state)
}

REV_logic_2 <- function(ns, state, input, review, datasets, selected_dataset_list_name, selected_dataset_name, data,
                        dt_proxy) { # TODO: Rename
  shiny::observeEvent(input[[REV$ID$REVIEW_SELECT]], {
    role <- make.names(input[[REV$ID$ROLE]])
   
    dataset_list_name <- selected_dataset_list_name() 
    dataset_name <- selected_dataset_name()
    
    info <- input[[REV$ID$REVIEW_SELECT]]
    i_row <- info[["row"]] # TODO: Once there are multiple versions of the dataset, this will require extra steps
    option <- as.integer(info[["option"]])
    
    contents <- c(
      SH$integer_to_raw(i_row),
      SH$integer_to_raw(option),
      SH$double_to_raw(SH$get_UTC_time_in_seconds())
    )
    
    fname <- paste0(dataset_name, "_", role, ".review")
    path <- file.path(state[["folder"]], dataset_list_name, fname)
    
    new_data <- data()
    new_data[i_row, ][[REV$ID$REVIEW_COL]] <- REV_patch_select(ns, i_row, review[["choices"]], option)
   
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
