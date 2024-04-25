#' Mock app without layout
#'
#' \code{mock_listings_app} launches a mock app for the dv.listings shiny module. Only for development purposes.
#'
#' @param mode Character value that indicates whether to include one or multiple datasets. Could either be "single" or
#'   "multi".
#'
#' @keywords internal
#' @family mock apps
#'
mock_listings_app <- function(mode = "single") {
  stopifnot(mode %in% c("single", "multi"))

  mock_listings_UI <- function(id) { # nolint
    ns <- ifelse(is.character(id), shiny::NS(id), shiny::NS(NULL))
    shiny::fluidPage(listings_UI(ns("listings")))
  }

  mock_listings_server <- function(input, output, session) {
    if (mode == "single") {
      data <- list(mtcars = datasets::mtcars)
      data$mtcars["index"] <- rownames(data$mtcars)
      attributes(data$mtcars$mpg)$label <- "Miles/(US) gallon"
      def_col <- list(mtcars = c("index", "mpg", "cyl"))
    } else {
      mtcars <- datasets::mtcars
      iris <- datasets::iris
      attributes(mtcars$mpg)$label <- "Miles/(US) gallon"
      attributes(mtcars)$label <- "Motor Trend Car Road Tests"
      data <- list("mtcars" = mtcars, "iris" = iris)
      def_col <- list(mtcars = names(mtcars[1:3]), iris = names(iris))
    }

    listings_server(
      "listings",
      dataset_list = shiny::reactive({
        data
      }), default_vars = def_col,
      dataset_metadata = list(
        name = shiny::reactive("test_name"),
        date_range = shiny::reactive({
          c("2022-01-01", "2022-12-03")
        })
      )
    )
  }

  shiny::shinyApp(
    mock_listings_UI,
    mock_listings_server
  )
}


#' Mock app integrated in the module manager
#'
#' \code{mock_listings_mm} launches a mock app for the dv.listings shiny module by means of
#' the module manager (dv.manager).
#'
#' @keywords internal
#' @family mock apps
#'
mock_listings_mm <- function() {
  data <- list()
  data[["adsl"]] <- convert_data(pharmaverseadam::adsl)
  data[["adae"]] <- convert_data(pharmaverseadam::adae)
  data[["adtte"]] <- convert_data(pharmaverseadam::adtte_onco)
  data[["test"]] <- data.frame(
    USUBJID = c("01-701-1015", "01-701-1023"),
    test_date = as.POSIXct(c("2023-03-09", "2023-03-10"))
  )


  # Add dataset with less than six columns to check behavior for default number of columns
  data[["small"]] <- convert_data(pharmaverseadam::adsl[1:4])

  # Add data list without any labels for comparison purposes
  data_no_lab <- data

  # Labels for dataset names of first data list
  attributes(data$adsl)$label <- "Subject Level"
  attributes(data$adae)$label <- "Adverse Events"
  attributes(data$adtte)$label <- "Time-to-Event"
  attributes(data$small)$label <- "Few columns"

  # Define default columns
  default_vars_multi <- list(adsl = names(data$adsl)[1:8], adae = names(data$adae)[1:8])
  default_vars_single <- NULL

  # No Label within column header
  attributes(data$adsl[["STUDYID"]])$label <- NULL

  # Define and launch mock app
  module_list <- list(
    "Multiple Listings" = dv.listings::mod_listings(
      dataset_disp = dv.manager::mm_dispatch("filtered_dataset", c("adsl", "adae", "adtte", "small", "test")),
      module_id = "multi",
      default_vars = default_vars_multi,
      pagination = TRUE
    ),
    "Single Listing" = dv.listings::mod_listings(
      dataset_names = "adsl",
      module_id = "single",
      default_vars = default_vars_single,
      intended_use_label = NULL
    )
  )

  dv.manager::run_app(
    data = list("demo" = data, "demo no labels" = data_no_lab),
    module_list = module_list,
    filter_data = "adsl",
    enableBookmarking = "url"
  )
}
