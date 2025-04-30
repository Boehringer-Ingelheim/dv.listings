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
  SEL_SUB_ID = "selected_subject_id",
  REVIEW_DROPDOWN_ID = "review_dropdown_id",
  REVIEW_UI_ID = "review_ui_id",
  REVIEW_DROPDOWN_LABEL = "Annotations"
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
    highlight_review_cols, 
    shiny::fluidRow(
      shiny::column(
        5,
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
        )
      ),
      shiny::column(
        5, 
        shinyWidgets::dropdownButton(
          inputId = ns(TBL$REVIEW_DROPDOWN_ID), label = TBL$REVIEW_DROPDOWN_LABEL, circle = FALSE,
          shiny::uiOutput(ns(TBL$REVIEW_UI_ID))
        )
      ),
      shiny::column(2, mod_export_listings_UI(module_id = ns(TBL$EXPORT_ID)))
    ),
    shiny::br(),
    shiny::actionButton(
      ns(TBL$RESET_FILT_BUTTON_ID),
      TBL$RESET_FILT_BUTTON_LABEL,
      icon = shiny::icon("filter-circle-xmark")
    ),
    shiny::br(),
    DT::dataTableOutput(ns(TBL$TABLE_ID), height = "80vh"),
    shiny::tags[["script"]](shiny::HTML(sprintf("
    $('#%s').on('.dt', function(e) {
  console.log('DataTables event triggered:', e.type);
});

    $('#%s').on('init.dt', function(e, settings) {    
    const table_container_id = '%s';
    const table = document.querySelector('#' + table_container_id + ' table.dataTable');
    if (!table) return;
    console.log('table found');


    const headers = table.querySelectorAll('thead tr')[0]?.querySelectorAll('th.dtfc-fixed-left');
    const filters = table.querySelectorAll('thead tr')[1]?.querySelectorAll('td');
    if (!headers || !filters) return;
    console.log('headers found');
    headers.forEach((th, i) => {
      const td = filters[i];
      if (!td) return;

      const computed_style = window.getComputedStyle(th);
      const left = computed_style.left;
      const zIndex = computed_style.zIndex;

      td.classList.add('dtfc-fixed-left');
      td.style.position = 'sticky';
      td.style.left = left;
      td.style.zIndex = zIndex;      
    });
  });", ns(TBL$TABLE_ID), ns(TBL$TABLE_ID), ns(TBL$TABLE_ID))))
    
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
#' @param subjid_var `[character(1) | NULL]`
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
        if (input[[TBL$DATASET_ID]] == "") names(v_dataset_list())[1] else input[[TBL$DATASET_ID]]
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
      # NOTE(miguel): Added here for easier merge with other branches
      # TODO(miguel): Move elsewhere after merging 
      REV_UI(ns = ns, roles = review[["roles"]])[["input_ids_to_exclude_from_bookmarking"]]
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
    show_review_column <- function() FALSE
    REV_state <- list()
    if (enable_review) {
      rev_ui_info <- REV_UI(ns = ns, roles = review[["roles"]])
      output[[TBL$REVIEW_UI_ID]] <- shiny::renderUI(rev_ui_info[["ui"]])
      shiny::outputOptions(output, TBL$REVIEW_UI_ID, suspendWhenHidden = FALSE)
      REV_state <- REV_logic_1(input, review, review[["data"]])
      show_review_column <- shiny::reactive(REV_state[["connected"]])
    }
    
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
      labels <- get_labels(dataset)
      data <- droplevels(dataset)
      data <- set_labels(data, labels)
      
      set_up <- set_up_datatable(dataset = data, pagination = pagination)
      
      if (testing) {
        col_names <- set_up[["col_names"]]
        shiny::exportTestValues(output_table = data, column_names = col_names)
      }

      if (show_review_column()) {
        selected_dataset_list_name <- review[["selected_dataset"]]()
        selected_dataset_name <- input[[TBL$DATASET_ID]]
        annotation_info <- REV_state[["annotation_info"]]
        reviews <- annotation_info[[selected_dataset_list_name]][[selected_dataset_name]][["review"]]
        roles <- annotation_info[[selected_dataset_list_name]][[selected_dataset_name]][["role"]]
        status <- annotation_info[[selected_dataset_list_name]][[selected_dataset_name]][["status"]]
        
        # TODO: Column fixing of subject and review columns; also of possibly dedicated jump-to column (visit first the DT docs page)
        # https://stackoverflow.com/questions/51623584/fixing-a-column-in-shiny-datatable-while-scrolling-right-does-not-work
        changes <- REV_add_review_columns(ns, data, review[["choices"]], reviews, roles, status)
        data <- changes[["data"]]
        set_up[["col_names"]] <- c(changes[["extra_column_names"]], set_up[["col_names"]])
      }
      
      if (!is.null(on_sbj_click)) {
        # FIXME? Assumes the subject column is present
        # Style subject column as link
        data[[subjid_var]] <- sprintf("<a title=\"Click for subject details\" href=\"\" onclick=\"Shiny.setInputValue('%s', '%s', {priority:'event'}); return false;\">%s</a>", 
                                      session[["ns"]](TBL$SEL_SUB_ID), data[[subjid_var]], data[[subjid_var]])
      }
      
      return(
        list(
          data = data, 
          row_names = set_up[["row_names"]], 
          col_names = set_up[["col_names"]],
          paging = set_up[["paging"]]
        )
      )
    })
  
    if (enable_review) {
      REV_logic_2(
        ns = ns, state = REV_state, input = input, review = review, datasets = review[["data"]],
        selected_dataset_list_name = review[["selected_dataset"]],
        selected_dataset_name = shiny::reactive(input[[TBL$DATASET_ID]]),
        data = shiny::reactive(output_table_data()[["data"]]), dt_proxy = dt_proxy
      )
    }
    
    output[[TBL$TABLE_ID]] <- DT::renderDataTable({
      shiny::validate(shiny::need(!is.null(input[[TBL$COLUMNS_ID]]), TBL$NO_COL_MSG))
      
      js_restore_original_order <- c(
        "function(e, dt, node, config) {",
        "  dt.iterator('table', function(s) {",
        "    s.aaSorting.length = 0;",
        "    s.aiDisplay.sort(function(a,b) {",
        "       return a-b;",
        "    });",
        "    s.aiDisplayMaster.sort(function(a,b) {",
        "       return a-b;",
        "    });",
        "  }).draw();",
        "}"
      )

      js_generate_review_column_contents <- c(
        "function(data, type, row, meta){",
        "  return data;",
        "}"
      )

      current_role <- input[[REV$ID$ROLE]]
      if (length(current_role) == 1 && current_role %in% review[["roles"]]) {
        js_generate_review_column_contents <- c(
          "function(data, type, row, meta){",
          "  if(type === 'display'){",
          "    let result = '';",
          "    let options = [%s];" |> sprintf(paste(paste0("'", review[["choices"]], "'"), collapse = ", ")),
          "    result += `<select style=\"width:100%%\" onchange=\"Shiny.setInputValue(\'%s\', {row:${row[0]}, option:this.value});\">`;" |> sprintf(ns(REV$ID$REVIEW_SELECT)),
          "    for (let i = 0; i < options.length; i+=1) {",
          "      result += `<option value=${i+1}${options[i]==data?' selected':''}>${options[i]}</option>`;",
          "    }",
          "    result += '</select>';",
          "    return result;",
          "  } else {",
          "    return data;",
          "  }",
          "}"
        )
      }
      
      table_data <- output_table_data()
      
      review_column_indices <- integer()
      if (enable_review) review_column_indices <- seq_along(REV$LABEL$REVIEW_COLS)
      
      fixed_columns_left <- length(review_column_indices) + 1
      
      DT::datatable(
        data = table_data[["data"]],
        colnames = table_data[["col_names"]],
        rownames = table_data[["row_names"]],
        escape = FALSE,
        filter = "top", # FIXME: Interacts badly with fixedColumns
        # Here's something that works: https://datatables.net/extensions/fixedcolumns/examples/styling/col_filter.html
        extensions = c("Buttons", "FixedColumns"),
        fillContainer = TRUE,
        options = list(
          searching = TRUE,
          paging = table_data[["paging"]],
          scrollX = TRUE,
          ordering = TRUE,
          columnDefs = list(
            list(className = "dt-center", targets = "_all")
            , list(className = "dv_listings_review_column", targets = review_column_indices) 
            , list(render = htmlwidgets::JS(js_generate_review_column_contents), data = 1, targets = head(review_column_indices, 1))
          ),
          # FIXME: Update to use https://datatables.net/reference/option/layout
          dom = "Bfrtilp", # Buttons, filtering, processing display element, table, information summary, length, pagination
          buttons = list(
            list(
              extend = "collection",
              text = "Reset rows order",
              action = htmlwidgets::JS(js_restore_original_order)
            )
          ),
          fixedColumns = list(left = 4)
        ),
        selection = "none"
      )
    })
    
    shiny::exportTestValues(
      selected_columns_in_dataset = r_selected_columns_in_dataset()
    )
    
    ## Jump to subject ----
    mod_return_value <- NULL
    if (!is.null(on_sbj_click)) {
      shiny::observe({
        shiny::req(!is.null(input[[TBL$SEL_SUB_ID]]))
        on_sbj_click()
      })
      mod_return_value <- list(subj_id = shiny::reactive(input[[TBL$SEL_SUB_ID]]))
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
  review = list(""), 
  receiver_id = list("")
)

mod_listings_API_spec <- TC$group(
  module_id = TC$mod_ID(),
  dataset_names = TC$dataset_name() |> TC$flag("one_or_more"),
  default_vars = TC$group() |> TC$flag("ignore"),         # manually tested by check_mod_listings
  pagination = TC$group() |> TC$flag("ignore"),           # manually tested by check_mod_listings
  intended_use_label = TC$group() |> TC$flag("ignore"),   # manually tested by check_mod_listings
  subjid_var = TC$group() |> TC$flag("ignore"),           # manually tested by check_mod_listings
  review = TC$group() |> TC$flag("ignore"),               # functionality is a WIP, so not defining for now
  receiver_id = TC$group() |> TC$flag("ignore")           # manually tested by check_mod_listings
) |> TC$attach_docs(mod_listings_API_docs)

dataset_info_listings <- function(dataset_names, ...) {
  return(list(all = unique(dataset_names), subject_level = character(0)))
}

check_mod_listings <- function(afmm, datasets, module_id, dataset_names, 
                               default_vars, pagination, intended_use_label,
                               subjid_var, receiver_id, review) {
  warn <- CM$container()
  err <- CM$container()
  
  ok <- check_mod_listings_auto(
    afmm, datasets,
    module_id, dataset_names, default_vars, pagination, intended_use_label,
    subjid_var, receiver_id, warn, err
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
  
  # review
  # TODO:
  
  # receiver_id
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
  
  res <- list(warnings = warn[["messages"]], errors = err[["messages"]])
  return(res)
}

mod_listings <- CM$module(
  mod_listings, check_mod_listings, dataset_info_listings
)
