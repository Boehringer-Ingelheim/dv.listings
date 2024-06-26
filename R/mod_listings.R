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
  EXPORT_ID = "export"
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

  shiny::tagList(
    shiny::fluidRow(
      shiny::column(2, shinyWidgets::dropdownButton(
        inputId = ns(TBL$DRPDBUTTON_ID),
        shiny::selectizeInput(ns(TBL$DATASET_ID), label = TBL$DATASET_LABEL, choices = NULL),
        shiny::selectizeInput(
          ns(TBL$COLUMNS_ID),
          label = TBL$COLUMNS_LABEL,
          choices = NULL,
          multiple = TRUE,
          options = list(plugins = list("remove_button", "drag_drop"))
        ),
        circle = FALSE,
        icon = shiny::icon("cog"),
        width = TBL$DRPDBUTTON_WIDTH,
        label = TBL$DRPDBUTTON_LABEL,
        tooltip = shinyWidgets::tooltipOptions(title = TBL$DRPDBUTTON_LABEL)
      )),
      shiny::column(2, mod_export_listings_UI(module_id = ns(TBL$EXPORT_ID)), offset = 8)
    ),
    shiny::br(),
    DT::dataTableOutput(ns(TBL$TABLE_ID), height = "80vh")
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
#' @export
listings_server <- function(module_id,
                            dataset_list,
                            default_vars = NULL,
                            dataset_metadata,
                            pagination = NULL,
                            intended_use_label = NULL) {
  checkmate::assert(
    checkmate::check_character(module_id, min.chars = 1),
    checkmate::check_multi_class(dataset_list, c("reactive", "shinymeta_reactive")),
    checkmate::check_list(default_vars, null.ok = TRUE),
    checkmate::check_list(dataset_metadata, names = "named", types = c("reactive", "shinymeta_reactive")),
    checkmate::check_character(names(dataset_metadata), unique = TRUE),
    checkmate::check_subset(names(dataset_metadata), choices = c("name", "date_range")),
    checkmate::check_logical(pagination, null.ok = TRUE),
    checkmate::check_string(intended_use_label, null.ok = TRUE),
    combine = "and"
  )
  if (!is.null(default_vars)) {
    checkmate::assert_names(names(default_vars), type = "unique")
  }
  # Initiate module server
  shiny::moduleServer(module_id, function(input, output, session) {
    v_dataset_list <- shiny::reactive({
      checkmate::assert_list(dataset_list(), types = "data.frame", null.ok = TRUE, names = "named")
      dataset_list()
    })

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

      choices <- generate_choices(v_dataset_list())
      shiny::exportTestValues(dataset_choices = choices) # Export values for shinytest2  tests

      shiny::updateSelectizeInput(inputId = TBL$DATASET_ID, choices = choices, selected = selected)
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

        choices <- generate_choices(listings_data())

        shiny::updateSelectizeInput(
          inputId = TBL$COLUMNS_ID,
          choices = choices,
          selected = r_selected_columns_in_dataset()[[input[[TBL$DATASET_ID]]]]
        )
      }
    )

    shiny::observeEvent(input[[TBL$COLUMNS_ID]], {
      selected_columns_in_dataset <- r_selected_columns_in_dataset()
      selected_columns_in_dataset[[input[[TBL$DATASET_ID]]]] <- input[[TBL$COLUMNS_ID]]
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
      "table_state"
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

    output[[TBL$TABLE_ID]] <- DT::renderDataTable({
      shiny::validate(shiny::need(!is.null(input[[TBL$COLUMNS_ID]]), TBL$NO_COL_MSG))

      # JS to restore original sort
      js <- c(
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

      selected_cols <- r_selected_columns_in_dataset()[[input[[TBL$DATASET_ID]]]]

      dataset <- listings_data()[selected_cols]

      # drop factor levels to ensure column filter of DT don't show non existing levels
      labels <- get_labels(dataset)
      data <- droplevels(dataset)
      data <- set_labels(data, labels)

      set_up <- set_up_datatable(dataset = data, selected_cols = selected_cols, pagination = pagination)

      # Export values for shinytest2 tests
      shiny::exportTestValues(output_table = data, column_names = set_up$col_names)

      DT::datatable(
        data,
        colnames = set_up$col_names,
        rownames = set_up$row_names,
        filter = "top",
        extensions = "Buttons",
        fillContainer = TRUE,
        options = list(
          searching = TRUE,
          paging = set_up$paging,
          scrollX = TRUE,
          ordering = TRUE,
          columnDefs = list(list(className = "dt-center", targets = "_all")),
          dom = "Bfrtilp",
          buttons = list(list(extend = "collection", text = "Reset Rows Order", action = htmlwidgets::JS(js)))
        )
      )
    })


    shiny::exportTestValues(
      selected_columns_in_dataset = r_selected_columns_in_dataset()
    )
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
#' Cannot be used together with the parameter \code{dataset_disp}.
#'
#' @inheritParams listings_server
#'
#' @param dataset_disp `[dv.manager::mm_dispatch()]`
#'
#' This is only for advanced usage. An mm_dispatch object.
#' Can not be used together with the parameter \code{dataset_names}.
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
    dataset_disp) {
  # Check validity of parameters
  if (!missing(dataset_names)) {
    checkmate::assert_character(dataset_names)
  }
  if (!missing(dataset_disp)) {
    checkmate::assert_list(dataset_disp, types = "character")
  }

  # skip assertions/checks for module_id and default_vars since they will be checked directly in listings_server()
  if (!missing(dataset_disp)) {
    checkmate::assert(
      checkmate::check_class(dataset_disp, "mm_dispatcher"),
      checkmate::check_names(names(dataset_disp), identical.to = c("from", "selection")),
      combine = "and"
    )
  }

  if (!missing(dataset_names) && !missing(dataset_disp)) {
    stop("You specified both parameters dataset_names and dataset_disp, but only one can be used at the same time.")
  } else if (missing(dataset_names) && missing(dataset_disp)) {
    stop("Neither dataset_names nor dataset_disp is specified, please specify one of them.")
  }
  # check if dataset_disp should be used
  if (missing(dataset_disp)) {
    use_disp <- FALSE
  } else {
    use_disp <- TRUE
  }

  mod <- list(
    ui = function(module_id) {
      listings_UI(module_id = module_id)
    },
    server = function(afmm) {
      dataset_list <- if (use_disp) {
        dv.manager::mm_resolve_dispatcher(dataset_disp, afmm)
      } else {
        shiny::reactive({
          afmm$filtered_dataset()[dataset_names]
        })
      }

      listings_server(
        dataset_list = dataset_list,
        default_vars = default_vars,
        dataset_metadata = afmm$dataset_metadata,
        pagination = pagination,
        module_id = module_id,
        intended_use_label = intended_use_label
      )
    },
    module_id = module_id
  )
  return(mod)
}
