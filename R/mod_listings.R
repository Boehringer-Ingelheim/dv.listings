# CONSTANTS ----
TBL <- pack_of_constants( # nolint
  DATASET_ID = "dataset",
  DATASET_LABEL = "Select listing",
  COLUMNS_ID = "col_sel",
  COLUMNS_LABEL = "Select variables",
  DRPDBUTTON_ID = "dropdown_btn",
  DRPDBUTTON_WIDTH = "300px",
  DRPDBUTTON_LABEL = "Click to see inputs",
  TABLE_ID = "listing",
  NO_COL_MSG = "Please select at least one column.",
  EXPORT_ID = "export",
  RESET_FILT_BUTTON_ID = "reset_filt_btn",
  RESET_FILT_BUTTON_LABEL = "Reset all filters",
  SELECT_ALL_COLS_BUTTON_ID = "select_all_cols_btn",
  SELECT_ALL_COLS_BUTTON_LABEL = "Select all variables",
  REMOVE_ALL_COLS_BUTTON_ID = "remove_all_cols_btn",
  REMOVE_ALL_COLS_BUTTON_LABEL = "Remove all variables",
  RESET_COLS_DEFAULT_BUTTON_ID = "reset_cols_btn",
  RESET_COLS_DEFAULT_BUTTON_LABEL = "Reset to default variables",
  RESET_ROWS_ORDER_BUTTON_ID = "reset_row_order_btn",
  RESET_ROWS_ORDER_BUTTON_LABEL = "Reset Row Order",
  SEARCH_BOX_ID = "search_box",
  SEARCH_BOX_LABEL = "Search: ",
  SEL_SUB_ID = "selected_subject_id",
  REVIEW_DROPDOWN_ID = "review_dropdown_id",
  REVIEW_UI_ID = "review_ui_id",
  REVIEW_DROPDOWN_LABEL = "Review",
  FSA_CLIENT = "client"
)

#' A module that displays datasets as listings
#'
#' @description This module displays a given dataset as listing. It allows switching between datasets if it receives
#'   more than one.
#'
#' @template id_param
#'
#' @export
#' @family data_listings
listings_UI <- function(module_id) { # nolint
  
  # Check validity of arguments
  checkmate::assert_string(module_id, min.chars = 1)
  ns <- shiny::NS(module_id)
  
  highlight_review_cols <- shiny::singleton(shiny::tags$head(shiny::tags[["style"]](shiny::HTML(
    "td.dv_listings_review_column {background-color: #f2eeee !important;}"
  ))))
  
  shiny::tagList(
    add_dv_listings_dependency(),
    highlight_review_cols,    
    shiny::div(
      style = "display: flex; gap: 10px; align-items: baseline",
      shinyWidgets::dropdownButton(
        inputId = ns(TBL$DRPDBUTTON_ID),
        shiny::selectizeInput(ns(TBL$DATASET_ID), label = TBL$DATASET_LABEL, choices = NULL),
        shiny::tags[["style"]](shiny::HTML(paste0(
          "#",
          ns(TBL$COLUMNS_ID),
          " + div.selectize-control div.selectize-input.items {max-height:250px; overflow-y:auto;}"
        ))),
        shiny::selectizeInput(
          ns(TBL$COLUMNS_ID),
          label = TBL$COLUMNS_LABEL,
          choices = NULL,
          multiple = TRUE,
          options = list(plugins = list("remove_button", "drag_drop"))
        ),
        shiny::actionButton(
          ns(TBL$SELECT_ALL_COLS_BUTTON_ID), 
          TBL$SELECT_ALL_COLS_BUTTON_LABEL, 
          icon = shiny::icon("check-double")
        ),
        shiny::actionButton(
          ns(TBL$REMOVE_ALL_COLS_BUTTON_ID),
          TBL$REMOVE_ALL_COLS_BUTTON_LABEL, 
          icon = shiny::icon("xmark")
        ),
        shiny::actionButton(
          ns(TBL$RESET_COLS_DEFAULT_BUTTON_ID),
          TBL$RESET_COLS_DEFAULT_BUTTON_LABEL, 
          icon = shiny::icon("rotate-left")
        ), 
        circle = FALSE,
        icon = shiny::icon("cog"),
        width = TBL$DRPDBUTTON_WIDTH,
        label = TBL$DRPDBUTTON_LABEL,
        tooltip = shinyWidgets::tooltipOptions(title = TBL$DRPDBUTTON_LABEL)
      ),
      mod_export_listings_UI(module_id = ns(TBL$EXPORT_ID)),
      shiny::actionButton(
        ns(TBL$RESET_FILT_BUTTON_ID),
        TBL$RESET_FILT_BUTTON_LABEL,
        icon = shiny::icon("filter-circle-xmark")
      ),
      shiny::tags[["button"]](
        id = ns(TBL$RESET_ROWS_ORDER_BUTTON_ID),
        class = "btn btn-default action-button",
        TBL$RESET_ROWS_ORDER_BUTTON_LABEL
      ),
      shiny::tags[["script"]](shiny::HTML(sprintf("
  $(document).on('click', '#%s', function() {
    let table = $('#%s table.dataTable').DataTable();
    table.order([]); // reset sorting    
    table.draw();
  });
", ns(TBL$RESET_ROWS_ORDER_BUTTON_ID), ns(TBL$TABLE_ID)))),
      shiny::uiOutput(ns(TBL$REVIEW_UI_ID)),
      shiny::div(style = "flex-grow: 1;"),
      shiny::span(
        shiny::tags[["label"]](
          "for" = ns(TBL$SEARCH_BOX_ID),
          TBL$SEARCH_BOX_LABEL
        ),
        shiny::tags[["input"]](
          id = ns(TBL$SEARCH_BOX_ID),
          type = "text"
        ),
        
        shiny::tags[["script"]](
          shiny::HTML(
            sprintf("
              $(document).on('input', '#%s', function() {
              let table = $('#%s table.dataTable').DataTable();
              table.search(this.value).draw();
              });",
              ns(TBL$SEARCH_BOX_ID), ns(TBL$TABLE_ID)
            )
          )
        ),
        
        # Even with our search box we need to enable searching in the datatable (see: algfne) otherwise searching is
        # not possible even using JS. Therefore we need to hide the table searching box.        
        shiny::tags[["style"]](          
          shiny::HTML(
            sprintf("#%s .dataTables_filter {display:none}", ns(TBL$TABLE_ID))
          )
        )
      ),
    ),
    DT::dataTableOutput(ns(TBL$TABLE_ID), height = "87vh"),
    shiny::tags[["script"]](shiny::HTML(sprintf("
    $('#%s').on('init.dt', function(e, settings) {    
      const table_container_id = '%s';
      const table = document.querySelector('#' + table_container_id + ' table.dataTable');
      if (!table) return;
  
      // Make column filters scroll horizontally along the rest of the table rows
      const fixed_headers = table.querySelectorAll('thead tr')[0]?.querySelectorAll('th.dtfc-fixed-left');
      const filters = table.querySelectorAll('thead tr')[1]?.querySelectorAll('td');
  
      if (!fixed_headers || !filters) return;
      for(let idx = 0; idx < fixed_headers.length; ++idx){
        const th = fixed_headers[idx];
        const td = filters[idx];
        if (!td) return;
  
        const computed_style = window.getComputedStyle(th);
        const left = computed_style.left;
        const zIndex = computed_style.zIndex;
  
        /* Character filters have an element with z-index 25 therefore we choose 26. This is an empirical finding as
           I found no reference in the documentation, therefore this magic number may change. */
        magic_z_index = 26; 
  
        td.classList.add('dtfc-fixed-left');
        td.style.position = 'sticky';
        td.style.left = left;
        td.style.zIndex = magic_z_index;      
        td.style.background = 'white';
      }
      
      // Extend width of factor search boxes to fit options (adapted from https://stackoverflow.com/a/76771419)
      let tds = table.querySelectorAll('td[data-type=\"factor\"]');
      for(let i = 0; i < tds.length; i += 1){
        let td = tds[i];
        let dropdown_content = td.querySelector('div.selectize-dropdown-content');
        dropdown_content.setAttribute('style','min-width:100%%; width:max-content; background-color:white; border:1px solid rgba(0, 0, 0, 0.15); border-radius:4px; box-shadow:0 6px 12px rgba(0, 0, 0, 0.175);');
      }
  });", ns(TBL$TABLE_ID), ns(TBL$TABLE_ID))))
  )
}

#' @describeIn listings_UI
#'
#' @param dataset_list `[shiny::reactive(list(data.frame)]`
#'
#' A reactive list of data.framish dataset(s) that will be shown as listing.
#' @param default_vars `[list(characters(1+)) | NULL]`
#'
#' A list of character vectors which contain the variable names to be displayed as default per
#'   dataset. Named according to the \code{dataset_names}. If `NULL`, the first six variables are displayed for each
#'   dataset.
#' @param dataset_metadata `[list(character(1), character(1+))]` A list with the following two elements:
#' \code{dataset_metadata$name()} containing a reactive string specifying the name of the selected
#' dataset and \code{dataset_metadata$date_range()} containing a reactive character vector with two entries
#' specifying the earliest and latest modification date in the dataset.
#' Usually obtained from module manager.
#'
#' @param pagination `[logical(1) | NULL]` Either a boolean indicating if pagination should be activated, or
#' NULL for which pagination will be activated for large datasets (nrows > 1000) automatically.
#'
#' @param intended_use_label `[character(1) | NULL]` Either a string indicating the intended use for export, or
#' NULL. The provided label will be displayed prior to the download and will also be included in the exported file.
#'
#' @param subjid_var `[character(1)]`
#'
#' Column corresponding to subject ID. Default value is 'USUBJID'
#'
#' @param receiver_id `[character(1) | NULL]`
#'
#' Character string defining the ID of the module to which to send a subject ID. The
#' module must exist in the module list. The default is NULL which disables communication.
#'
#' @param on_sbj_click `[function()]`
#'
#' Function to invoke when a subject ID is clicked in a listing
#' 
#' @param review `[list()]`
#'
#' Configuration of the experimental data review feature. 
#' Only one instance of the listings module can use this feature on any given app.
#' For more details, please refer to `vignette("data_review")`.
#'
#' @export
listings_server <- function(module_id,
                            dataset_list,
                            default_vars = NULL,
                            dataset_metadata,
                            pagination = NULL,
                            intended_use_label = NULL,
                            subjid_var = "USUBJID",
                            on_sbj_click = NULL,
                            review = NULL) {
  checkmate::assert(
    checkmate::check_character(module_id, min.chars = 1),
    checkmate::check_multi_class(dataset_list, c("reactive", "shinymeta_reactive")),
    checkmate::check_list(default_vars, null.ok = TRUE),
    checkmate::check_list(dataset_metadata, names = "named", types = c("reactive", "shinymeta_reactive")),
    checkmate::check_character(names(dataset_metadata), unique = TRUE),
    checkmate::check_subset(names(dataset_metadata), choices = c("name", "date_range")),
    checkmate::check_logical(pagination, null.ok = TRUE),
    checkmate::check_string(intended_use_label, null.ok = TRUE),
    checkmate::check_function(on_sbj_click, null.ok = TRUE),
    checkmate::check_list(review, null.ok = TRUE),
    combine = "and"
  )
  if (!is.null(default_vars)) {
    checkmate::assert_names(names(default_vars), type = "unique")
  }
  
  testing <- isTRUE(getOption("shiny.testmode"))

  # Initiate module server
  shiny::moduleServer(module_id, function(input, output, session) {

    ns <- session[["ns"]]
    
    v_dataset_list <- shiny::reactive({      
      checkmate::assert_list(dataset_list(), types = "data.frame", null.ok = TRUE, names = "named")      
      dataset_list()
    })
    
    # Set choices as a reactive value item
    rvs <- shiny::reactiveValues(dataset_choices = NA, variable_choices = NA)
    
    # Listing selection (start)
    shiny::observeEvent(v_dataset_list(), {
      # Fill default in case bookmark or default columns do not have all the listings in the dataset
      r_selected_columns_in_dataset(fill_default_vars(r_selected_columns_in_dataset(), v_dataset_list()))
      
      selected <- if (is.null(bmk_dataset)) {
        if (input[[TBL$DATASET_ID]] == "" || !input[[TBL$DATASET_ID]] %in% names(v_dataset_list())) names(v_dataset_list())[1] else input[[TBL$DATASET_ID]]
      } else {
        bmk_dataset
      }
      bmk_dataset <<- NULL
      
      rvs$dataset_choices <- generate_choices(v_dataset_list())
      shiny::exportTestValues(dataset_choices = rvs$dataset_choices) # Export values for shinytest2  tests

      shiny::updateSelectizeInput(inputId = TBL$DATASET_ID, choices = rvs$dataset_choices, selected = selected)
    })
    
    listings_data <- shiny::reactive({
      v_dataset_list()[[shiny::req(input[[TBL$DATASET_ID]])]]
    })
    
    # Listing selection (end)
    
    # Column selection (start)
    
    # When switching datasets I want to go back to the same state I was before
    r_selected_columns_in_dataset <- shiny::reactiveVal(default_vars)
    
    # Load options in the column menu
    shiny::observeEvent(
      # React to changes in the listings identity or full dataset change
      list(input[[TBL$DATASET_ID]], dataset_metadata[["name"]]()),
      { 
        # Notify of columns not present in the dataset
        p_selected_cols <- intersect(r_selected_columns_in_dataset()[[input[[TBL$DATASET_ID]]]], names(listings_data()))
        np_selected_cols <- setdiff(r_selected_columns_in_dataset()[[input[[TBL$DATASET_ID]]]], p_selected_cols)
        if (length(np_selected_cols) > 0) {
          shiny::showNotification(
            paste("Removing columns not present in dataset", paste(np_selected_cols, collapse = ";")),
            type = "warning"
          )
        }
        
        rvs$variable_choices <- generate_choices(listings_data())
        
        shiny::updateSelectizeInput(
          inputId = TBL$COLUMNS_ID,
          choices = rvs$variable_choices,
          selected = r_selected_columns_in_dataset()[[input[[TBL$DATASET_ID]]]]
        )
      }
    )
    
    shiny::observeEvent(input[[TBL$SELECT_ALL_COLS_BUTTON_ID]], {
      shiny::updateSelectizeInput(
        inputId  = TBL$COLUMNS_ID,
        choices  = rvs$variable_choices,
        selected = rvs$variable_choices
      )
    })
    
    shiny::observeEvent(input[[TBL$REMOVE_ALL_COLS_BUTTON_ID]], {
      shiny::updateSelectizeInput(
        inputId  = TBL$COLUMNS_ID,
        choices  = rvs$variable_choices,
        selected = NULL
      )
    })
    
    shiny::observeEvent(input[[TBL$RESET_COLS_DEFAULT_BUTTON_ID]], {
      r_selected_columns_in_dataset(
        fill_default_vars(default_vars, v_dataset_list())
      )
      shiny::updateSelectizeInput(
        inputId  = TBL$COLUMNS_ID,
        choices  = rvs$variable_choices,
        selected = r_selected_columns_in_dataset()[[input[[TBL$DATASET_ID]]]]
      )
    })
    
    
    shiny::observeEvent(input[[TBL$COLUMNS_ID]], {
      selected_columns_in_dataset <- r_selected_columns_in_dataset()
      selected_columns_in_dataset[[input[[TBL$DATASET_ID]]]] <- input[[
        TBL$COLUMNS_ID
      ]]
      r_selected_columns_in_dataset(selected_columns_in_dataset)
    })
    
    # Column selection (end)
    
    # Bookmarking (start)
    
    bmk_dataset <- NULL
    
    # for storing cache for bookmarking
    shiny::onBookmark(function(state) {
      state$values$selected_columns_in_dataset <- r_selected_columns_in_dataset()
      state$values$data_sel <- input[[TBL$DATASET_ID]]
    })
    
    shiny::onRestore(function(state) {
      if (length(state$input) > 0) { # makes sure that the default_vars are displayed at app launch with SSO
        bmk_dataset <<- state$values$data_sel
        r_selected_columns_in_dataset(state$values$selected_columns_in_dataset)
      }
    })
    
    shiny::setBookmarkExclude(c(
      "table_cell_clicked",
      "table_rows_current",
      "table_rows_all",
      "table_search",
      "table_search_columns",
      "table_rows_selected",
      "table_cells_selected",
      "table_columns_selected",
      "table_state",
      "select_all_cols_btn",
      "remove_all_cols_btn",
      "reset_cols_btn",
      "download_data",
      "reset_filt_btn",
      "download_data",
      "dropdown_btn",
      "clear_filters",
      REV_UI(ns = identity, roles = character(0))[["input_ids_to_exclude_from_bookmarking"]]
    ))

    # Bookmarking (end)
    
    mod_export_listings_server(
      module_id = TBL$EXPORT_ID,
      dataset_metadata = dataset_metadata,
      dataset_list = v_dataset_list,
      data = shiny::reactive(set_data(listings_data(), r_selected_columns_in_dataset()[[input[[TBL$DATASET_ID]]]])),
      data_selection_name = shiny::reactive(input[[TBL$DATASET_ID]]),
      current_rows = shiny::reactive(input[[paste0(TBL$TABLE_ID, "_rows_all")]]),
      intended_use_label = intended_use_label
    )
    
    # Proxy reference to dataTable
    dt_proxy <- DT::dataTableProxy(TBL$TABLE_ID)
    shiny::observeEvent(input[[TBL$RESET_FILT_BUTTON_ID]], DT::clearSearch(dt_proxy))
   
    enable_review <- !is.null(review)
    show_review_columns <- function() FALSE
    REV_state <- new.env(parent = emptyenv())
    if (enable_review) {

      fs_callbacks <- list(
        attach = shiny::reactiveVal(NULL),
        list = shiny::reactiveVal(NULL),
        read = shiny::reactiveVal(NULL),
        write = shiny::reactiveVal(NULL),
        append = shiny::reactiveVal(NULL),
        read_folder = shiny::reactiveVal(NULL),
        execute_IO_plan = shiny::reactiveVal(NULL)
      )
     
      fs_client <- NULL 
      if (is.null(review[["store_path"]])) {
        fs_client <- fsa_init(input, TBL$FSA_CLIENT, fs_callbacks)
      } else {
        fs_client <- fs_init(fs_callbacks, review[["store_path"]])
      }
      
      # Overly restrictive sanitization of role strings, as they will be used for file names:
      # TODO: Consider adapting https://github.com/r-lib/fs/blob/main/R/sanitize.R instead to allow alternative charsets
      review[["roles"]] <- gsub("[^a-zA-Z0-9 _.-]", "", review[["roles"]]) # Accepts alpha+num+space+'.'+'_'+'-'

      output[[TBL$REVIEW_UI_ID]] <- shiny::renderUI({

        review_ui <- shinyWidgets::dropdownButton(
          inputId = ns(TBL$REVIEW_DROPDOWN_ID), label = shiny::textOutput(ns("review_label"), inline = TRUE), circle = FALSE,
          REV_UI(ns = ns, roles = review[["roles"]])[["ui"]]
        )

        review_ui

      })

      review_button_label <- shiny::reactive({
        role <- input[[REV$ID$ROLE]]
        label <- TBL$REVIEW_DROPDOWN_LABEL
        if (checkmate::test_string(role, min.chars = 1, na.ok = FALSE, null.ok = FALSE)) {
          label <- paste(TBL$REVIEW_DROPDOWN_LABEL, "as:", role)
        }
        label
      })

      output[["review_label"]] <- shiny::renderText(review_button_label())

      shiny::outputOptions(output, TBL$REVIEW_UI_ID, suspendWhenHidden = FALSE)

      REV_main_logic(REV_state, input, review, review[["data"]], fs_client, fs_callbacks)
      show_review_columns <- REV_state[["contents_ready"]]
    }

    js_generate_review_column_contents <- shiny::reactive({
      js_render_call <- c("dv_listings.render_identity")
      render_status_js_call <- c("dv_listings.render_identity")

      role <- NA_character_

      current_role <- input[[REV$ID$ROLE]]
      if (length(current_role) == 1 && current_role %in% review[["roles"]]) {

        # If role is found make interactive          
        collapsed_choices <- paste(paste0("'", review[["choices"]], "'"), collapse = ", ")
        js_render_call <- sprintf("dv_listings.render_selection('%s', '%s', [%s])", ns(REV$ID$REVIEW_SELECT), current_role, collapsed_choices)
        role <- input[[REV$ID$ROLE]]
        render_status_js_call <- sprintf("dv_listings.render_status('%s', '%s', [%s])", ns(REV$ID$REVIEW_SELECT), current_role, collapsed_choices)
      }

      res <- list(
        role = role,
        js_render_call = js_render_call,
        render_status_js_call = render_status_js_call
      )

      return(res)
    }) |> trigger_only_on_change()
    
    output_table_data <- shiny::reactive({
      shiny::validate(                        # TODO: Explain why these are necessary
        shiny::need(
          !is.null(input[[TBL$COLUMNS_ID]]), 
          TBL$NO_COL_MSG
        )
      )
      
      selected_cols <- r_selected_columns_in_dataset()[[input[[TBL$DATASET_ID]]]]
      
      dataset <- listings_data()[selected_cols]
      
      # drop factor levels to ensure column filter of DT don't show non-existing levels
      shiny::req(!is.null(dataset))
      labels <- get_labels(dataset)
      data <- droplevels(dataset)
      data <- set_labels(data, labels)
      
      set_up <- set_up_datatable(dataset = data, pagination = pagination)
      
      if (testing) {
        col_names <- set_up[["col_names"]]
        shiny::exportTestValues(output_table = data, column_names = col_names)
      }

      if (!is.null(on_sbj_click) && subjid_var %in% names(data)) {
        # Style subject column as link
        data[[subjid_var]] <- sprintf("<a title=\"Click for subject details\" href=\"\" onclick=\"Shiny.setInputValue('%s', '%s', {priority:'event'}); return false;\">%s</a>", 
                                      session[["ns"]](TBL$SEL_SUB_ID), data[[subjid_var]], data[[subjid_var]])
      }
     
      # NOTE: Pass the reconstructed `filter_mask` as an attribute of `data` itself to ensure they're synchronized
      if (enable_review) {
        attr(data, "filter_mask") <- local({
          selected_dataset_list_name <- review[["selected_dataset"]]()
          selected_dataset_name <- input[[TBL$DATASET_ID]]
          
          all_data <- review[["data"]][[selected_dataset_list_name]][[selected_dataset_name]]
          id_vars <- review[["datasets"]][[selected_dataset_name]][["id_vars"]]
          group_separator <- "\01e"
          all_ids <- do.call(paste, c(all_data[id_vars], sep = group_separator))
          filtered_ids <- do.call(paste, c(listings_data()[id_vars], sep = group_separator))
          return(all_ids %in% filtered_ids)
        })
      }
      
      return(
        list(
          data = data, 
          row_names = set_up[["row_names"]], 
          col_names = set_up[["col_names"]],
          paging = set_up[["paging"]],
          dataset_name = input[[TBL$DATASET_ID]]
        )
      )
    })
  
    if (enable_review) {
      REV_respond_to_user_review(
        ns = ns, state = REV_state, input = input, review = review,
        selected_dataset_list_name = review[["selected_dataset"]],
        selected_dataset_name = shiny::reactive(input[[TBL$DATASET_ID]]),
        data = shiny::reactive(output_table_data()[["data"]]),
        dt_proxy = dt_proxy,
        fs_execute_IO_plan = fs_client[["execute_IO_plan"]]
      )
    }
    
    output[[TBL$TABLE_ID]] <- DT::renderDataTable({
      shiny::validate(shiny::need(!is.null(input[[TBL$COLUMNS_ID]]), TBL$NO_COL_MSG))

      table_data <- output_table_data()
      
      column_defs <- list(list(className = "dt-center", targets = "_all"))
      selected_dataset_name <- shiny::isolate(input[[TBL$DATASET_ID]])
      
      selected_dataset_label <- attr(shiny::isolate(dataset_list())[[selected_dataset_name]], "label")
      if (length(selected_dataset_label) == 0 || nchar(selected_dataset_label) == 0) 
        selected_dataset_label <- selected_dataset_name
      
      init_complete_payloads <- sprintf(
        "$('#' + settings.sTableId + '_wrapper').find('.top-title').append('<h4>%s</h4>');", selected_dataset_label 
      )
      
      review_col_count <- 0L
      if (show_review_columns() && selected_dataset_name %in% names(review$datasets)) {
        js_render_call <- js_generate_review_column_contents()[["js_render_call"]]
        render_status_js_call <- js_generate_review_column_contents()[["render_status_js_call"]]
        role <- js_generate_review_column_contents()[["role"]]

        # patch table data
        selected_dataset_list_name <- shiny::isolate(review[["selected_dataset"]]())

        # NOTE: Partially repeats #weilae 
        annotation_info <- REV_state[["annotation_info"]][[selected_dataset_list_name]][[selected_dataset_name]]
        changes <- REV_include_review_info(
          annotation_info = annotation_info,
          data = table_data[["data"]],
          col_names = table_data[["col_names"]]
        )

        changes[["data"]][[REV$ID$STATUS_COL]] <- REV_compute_status(changes[["data"]], role)
        changes[["data"]][[REV$ID$LATEST_REVIEW_COL]] <- REV_review_var_to_json(changes[["data"]][[REV$ID$LATEST_REVIEW_COL]])        
        
        review_col_count <- ncol(changes[["data"]]) - ncol(table_data[["data"]])
        table_data[["data"]] <- changes[["data"]]
        table_data[["col_names"]] <- changes[["col_names"]]
        
        table_data <- REV_include_highlight_info(
          table_data, annotation_info, 
          tracked_vars = review[["datasets"]][[selected_dataset_name]][["tracked_vars"]]
        )
        
        # patch table style
        review_column_indices <- seq_len(review_col_count)
        highlight_column_indices <- which(endsWith(table_data[["col_names"]], REV$ID$HIGHLIGHT_SUFFIX))
        
        column_defs <- append(
          column_defs,
          list(
            list(className = "dv_listings_review_column", targets = review_column_indices),
            list(render = htmlwidgets::JS(js_render_call), data = 1, 
                 targets = head(review_column_indices, 1)),
            list(render = htmlwidgets::JS(render_status_js_call), data = 3,
                 targets = review_column_indices[[3]]),
            list(visible = FALSE,
                 targets = c(review_column_indices[[4]], highlight_column_indices))
          )
        )

        # TODO: find a place for this if
        if (checkmate::test_string(input[[REV$ID$ROLE]], min.chars = 1)) {
          bulk_render <- sprintf(
            "dv_listings.render_bulk_menu(settings.sTableId + \"_wrapper\", [%s], '%s');",
            paste(paste0("'", review[["choices"]], "'"), collapse = ", "), ns(REV$ID$REVIEW_SELECT)
          )
          init_complete_payloads <- c(init_complete_payloads, bulk_render)
        }
      }
      
      init_complete_js <- paste0("function(settings, json) {", paste(init_complete_payloads, collapse = "\n"), "}")

      res <- DT::datatable(
        data = table_data[["data"]],
        colnames = table_data[["col_names"]],
        rownames = table_data[["row_names"]],
        escape = FALSE,
        filter = "top",
        extensions = c("FixedColumns"),
        fillContainer = TRUE,
        options = list(
          searching = TRUE, # (see: algfne)
          paging = table_data[["paging"]],
          scrollX = TRUE,
          ordering = TRUE,
          columnDefs = column_defs,
          # TODO: Update to use new recommended API: https://datatables.net/reference/option/layout
          dom = "<'top'<'top-title'>>rtilp", # Buttons, filtering, processing display element, table, information summary, length, pagination
          fixedColumns = list(left = review_col_count),
          initComplete = htmlwidgets::JS(init_complete_js),
          drawCallback = htmlwidgets::JS("
            function (settings) {  
            const table_wrapper = settings.nTableWrapper;
              const search_inputs = table_wrapper.querySelectorAll('thead input[type=\"search\"]');
              for(let i = 0; i < search_inputs.length; i++){
                search_inputs[i].removeAttribute('disabled');
              }              
              
              dv_listings.refresh_bulk_select_all_checkbox(settings.sTableId + \"_wrapper\");

              /*
              when upgrading to bs5 some filters were duplicated and left as orphans in the table body.
              The options fillContainer and scrollX seems to be the culprits. Although I could not find specific issues
              on this there seems to be many threads on the interactions of these two options with other datatable
              features. The included solution is to remove these elements by hand on each draw.
              */
              const scroll_body = table_wrapper.querySelector('.dataTables_scrollBody');
              if (scroll_body) {
                const letfover_sliders = scroll_body.querySelectorAll('.noUi-target');
                for(let i=0;i<letfover_sliders.length;++i){
                  letfover_sliders[i].parentNode.remove();
                }                
              }
            }
          ") # Keep filtering enabled even for columns that have a unique value
        ),
        selection = "none"
      )
      
      if (show_review_columns()) {
        tracked_vars <- sort(review[["datasets"]][[selected_dataset_name]][["tracked_vars"]])
        present_vars <- names(table_data[["data"]])
        sorted_present_tracked_vars <- sort(intersect(tracked_vars, present_vars))
        
        if (length(sorted_present_tracked_vars)) {
          res <- DT::formatStyle(
            table = res,
            columns = sorted_present_tracked_vars,
            valueColumns = paste0("__", sorted_present_tracked_vars, REV$ID$HIGHLIGHT_SUFFIX),
            target = "cell",
            backgroundColor = DT::styleEqual(c(FALSE, TRUE), c("#00000000", "#f0ad4ecc"))
          )
        }
      }
      
      return(res)
    })
    
    shiny::exportTestValues(
      selected_columns_in_dataset = r_selected_columns_in_dataset()
    )
    
    # Tell other modules we are a listing that offers the review function
    mod_return_value <- list(enabled_review = !is.null(review))

    ## Jump to subject ----
    if (!is.null(on_sbj_click)) {
      shiny::observe({
        shiny::req(!is.null(input[[TBL$SEL_SUB_ID]]))
        on_sbj_click()
      })
      mod_return_value[["subj_id"]] <- shiny::reactive(input[[TBL$SEL_SUB_ID]])
    }
    
    return(mod_return_value)
  })
}


#' Data listings module for DaVinci's module manager
#'
#' @description
#'
#' This module will present the dataset as listing using the DT package.
#' @param dataset_names `[character(1+)]`
#'
#' Name(s) of the dataset(s) that will be displayed.
#'
#' @inheritParams listings_server
#'
#' @template module_id-arg
#'
#' @export
#'
#' @family data_listings
#'
#' @examplesIf interactive()
#' library(dv.listings)
#'
#' # 1. Create a data list with example data
#' data_list <- list(
#'   adsl  = pharmaverseadam::adsl,
#'   adae  = pharmaverseadam::adae,
#'   adtte = pharmaverseadam::adtte_onco
#' )
#'
#' # 2. Preprocessing
#' # Convert data to appropriate types
#' data_list$adsl <- convert_data(data_list$adsl)
#' data_list$adae <- convert_data(data_list$adae)
#' data_list$adtte <- convert_data(data_list$adtte)
#'
#' # Assign meaningful labels to data domain names
#' attributes(data_list$adsl)$label <- "Subject Level"
#' attributes(data_list$adae)$label <- "Adverse Events"
#' attributes(data_list$adtte)$label <- "Time-to-Event"
#'
#' # Specify default variables
#' default_vars <- list(
#'   adsl = c("STUDYID", "USUBJID", "SITEID", "ARM"),
#'   adae = c("STUDYID", "ASTDY", "AENDT", "AESER")
#' )
#'
#' # 3. Module list
#' module_list <- list(
#'   "Exemplary listings" = mod_listings(
#'     module_id = "mod1",
#'     dataset_names = c("adsl", "adae", "adtte"),
#'     default_vars = default_vars
#'   )
#' )
#'
#' # 4. Launch the app
#' dv.manager::run_app(
#'   data = list("MyData" = data_list),
#'   module_list = module_list,
#'   filter_data = "adsl"
#' )
mod_listings <- function(
    module_id,
    dataset_names,
    default_vars = NULL,
    pagination = NULL,
    intended_use_label = "Use only for internal review and monitoring during the conduct of clinical trials.",
    subjid_var = "USUBJID",
    receiver_id = NULL,
    review = NULL) {
  # Check validity of parameters
  checkmate::assert_character(dataset_names)
  
  mod <- list(
    ui = function(module_id) {
      listings_UI(module_id = module_id)
    },
    server = function(afmm) {
      dataset_list <- shiny::reactive(afmm$filtered_dataset()[dataset_names])
      
      on_sbj_click_fun <- NULL
      if (!is.null(receiver_id)) {
        on_sbj_click_fun <- function() afmm[["utils"]][["switch2mod"]](receiver_id)
      }
      
      if (is.list(review)) {
        # These afmm fields are only required for the review functionality, so we bundle them in the `review` list
        review[["data"]] <- afmm[["data"]]
        review[["selected_dataset"]] <- afmm[["dataset_metadata"]][["name"]]
        
        # Prevent and warn against multiple `dv.listings` instances with active review functionality.
        #
        # This block of code takes advantage of the way `dv.manager` incrementally populates afmm[["module_output"]].
        # At this point in (non-reactive) time, `dv.manager` has run the server functions of only the modules that
        # precede this one in the `module_list` declaration of the DaVinci app. As long as one of them has declared 
        # that it offers the review interface, we will refuse to start our own.
        for (mod_output in afmm[["module_output"]]()){
          if (is.list(mod_output) && isTRUE(mod_output[["enabled_review"]])) {
            this_tab_name <- afmm[["module_names"]][[module_id]]
            
            shiny::showNotification({
              paste(
                "This app is configured to review listings in more than one tab. However,",
                "only one instance of the `dv.listings` module can offer review functionality on any given app.<br>",
                sprintf(
                  'We have <b>disabled the review interface on the tab labeled "%s" (with module ID "%s")</b>',
                  this_tab_name, module_id
                ), "to sidestep this issue. Sorry for the inconvenience."
              ) |> htmltools::HTML()
            }, duration = NULL, type = "error")
            
            review <- NULL
            break
          }
        }
      }
      
      listings_server(
        dataset_list = dataset_list,
        default_vars = default_vars,
        dataset_metadata = afmm$dataset_metadata,
        pagination = pagination,
        module_id = module_id,
        intended_use_label = intended_use_label,
        subjid_var = subjid_var,
        on_sbj_click = on_sbj_click_fun,
        review = review
      )
    },
    module_id = module_id
  )
  return(mod)
}

# Listings module interface description ----
# TODO: Fill in for dressing room and automatic generation of docs
mod_listings_API_docs <- list(
  "Listings",
  module_id = "",
  dataset_names = list(""),
  default_vars = list(""),
  pagination = list(""),
  intended_use_label = list(""),
  subjid_var = list(""), 
  receiver_id = list(""),
  review = list(
    "Review-related fields",
    datasets = list(""),
    choices = list(""),
    roles = list(""),
    store_path = list("")
  )
)

mod_listings_API_spec <- TC$group(
  module_id = TC$mod_ID(),
  dataset_names = TC$dataset_name() |> TC$flag("one_or_more"),
  default_vars = TC$group() |> TC$flag("manual_check"),                         # manually tested by check_mod_listings
  pagination = TC$logical() |> TC$flag("manual_check", "optional"),             # manually tested by check_mod_listings
  intended_use_label = TC$character() |> TC$flag("manual_check", "optional"),   # manually tested by check_mod_listings
  subjid_var = TC$character() |> TC$flag("manual_check"),                       # manually tested by check_mod_listings
  receiver_id = TC$character() |> TC$flag("manual_check"),                      # manually tested by check_mod_listings
  review = TC$group(
    datasets = TC$group(),
    choices = TC$character() |> TC$flag("one_or_more"),
    roles = TC$character() |> TC$flag("one_or_more"),
    store_path = TC$character() |> TC$flag("optional")
  ) |> TC$flag("manual_check", "optional")
) |> TC$attach_docs(mod_listings_API_docs)

dataset_info_listings <- function(dataset_names, ...) {
  return(list(all = unique(dataset_names), subject_level = character(0)))
}

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


check_mod_listings <- function(afmm, datasets, module_id, dataset_names, 
                               default_vars, pagination, intended_use_label,
                               subjid_var, receiver_id, review) {
  warn <- CM$container()
  err <- CM$container()
  
  ok <- check_mod_listings_auto(
    afmm, datasets,
    module_id, dataset_names, default_vars, pagination, intended_use_label,
    subjid_var, receiver_id, review, warn, err
  )
  
  # default_vars 
  if (ok[["dataset_names"]] && !is.null(default_vars)) {
    if (CM$assert(
      container = err,
      cond = (checkmate::test_list(default_vars, types = "character", names = "unique") &&
              checkmate::test_subset(names(default_vars), dataset_names)),
      msg = "`default_vars` should be a named list, whose names are unique references to elements of `dataset_names`."
    )) {
      for (name in names(default_vars)){
        available_cols <- names(datasets[[name]])
        
        CM$assert(
          container = err,
          cond = checkmate::test_subset(default_vars[[name]], available_cols),
          msg = sprintf("`default_vars[['%s']]` should be a subset of these columns: %s.", name,
                        paste(available_cols, collapse = ", "))
        )
      }
    }
  }
  
  # pagination
  CM$assert(
    container = err,
    cond = checkmate::test_logical(pagination, null.ok = TRUE, len = 1),
    msg = "`pagination` should be either logical(1) or NULL."
  )
  
  # intended_use_label
  CM$assert(
    container = err,
    cond = checkmate::test_string(intended_use_label, null.ok = TRUE),
    msg = "`intended_use_label` should be either character(1) or NULL."
  )
  
  # subjid_var
  CM$assert(
    container = err,
    cond = checkmate::test_string(subjid_var, null.ok = TRUE),
    msg = "`subjid_var` should be either character(1) or NULL."
  )
  
  CM$assert(
    container = err,
    cond = checkmate::test_string(receiver_id, null.ok = TRUE),
    msg = "`receiver_id` should be either character(1) or NULL."
  ) && CM$assert(
    container = err,
    cond = is.null(receiver_id) || receiver_id %in% names(afmm$module_names),
    msg = sprintf('`receiver_id` refers to "%s", which is not among the available module IDs: %s',
                  receiver_id, paste(names(afmm$module_names), collapse = ", ")
    )
  )

  check_review_parameter(datasets, dataset_names, review, err)
  
  res <- list(warnings = warn[["messages"]], errors = err[["messages"]])
  return(res)
}

mod_listings <- CM$module(
  mod_listings, check_mod_listings, dataset_info_listings
)
