REV <- pack_of_constants(
  ID = pack_of_constants(
    DROPDOWN = "review_dropdown",
    REVIEW_COL = "__review__",
    REVIEW_SELECT = "rev_id"
  ),
  LABEL = pack_of_constants(
    DROPDOWN = "Annotations"
  )
)

REV_UI <- function(ns){
  res <- shinyWidgets::dropdownButton(
    inputId = ns(REV$ID$DROPDOWN), label = REV$LABEL$DROPDOWN, circle = FALSE,
    shiny::actionButton('asd', 'press me')
  )
  return(res)
}


REV_add_review_column <- function(ns, data, col_names) { 
  data[[REV$ID$REVIEW_COL]] <- ""
  review_column_heading <- DTH$set_column_heading_hover_info(
    "Review", "Placeholder help text"
  )
  
  dropdown_column_template <- r"---(
  <select style='width:100%%' onchange="Shiny.setInputValue('%s', {row:%d, option:this.value});">
  <option value='1'>One</option>
  <option value='2'>Two</option>
  <option value='3'>Three</option>
  </select>
  )---"
  
  data[[REV$ID$REVIEW_COL]] <- sprintf(dropdown_column_template, ns(REV$ID$REVIEW_SELECT), seq_len(nrow(data)))
  
  return(list(data = data, col_names = c(col_names, review_column_heading)))
}

REV_logic <- function(input){
  shiny::observeEvent(input[[REV$ID$REVIEW_SELECT]], {
    browser() # Figure out which record to alter and do it
  })
}

if(FALSE){
  # TODO: Reference 
    ## Cell edit ----
    # TODO: Remove 
    
    compute_stable_row_id <- function(row) {
      # TODO: Flesh out
      return(row[[subjid_var]])
    }
    
    DT_hardcoded_cell_edit_suffix <- "_cell_edit"
    cell_edit_id <- paste0(TBL$TABLE_ID, DT_hardcoded_cell_edit_suffix)
    shiny::observeEvent(input[[cell_edit_id]], {
      dataset_name <- input[[TBL$DATASET_ID]]
      shiny::req(dataset_name)
      edit_info <- input[[cell_edit_id]]
      column <- edit_info[["col"]]
      col_count <- length(r_selected_columns_in_dataset()[[dataset_name]])
      shiny::req(column == col_count + 1) # row number counts as an editable column

      row <- listings_data()[edit_info$row, ]
      row_id <- compute_stable_row_id(row) #nolint
    })
    
  
}


    