mock_simple_listing_app <- function() {
  shinyApp(
    mock_simple_listing_UI,
    mock_simple_listing_server
  )
}

mock_simple_listing_UI <- function(id) { # nolint
  if (is.character(id)) {
    ns <- NS(id)
  } else {
    ns <- NS(NULL)
  }
  tagList(
    simple_listing_UI(ns("listing"))
  )
}

#'
mock_simple_listing_server <- function(input, output, session) {
  simple_listing_server("listing", dataset = reactive(datasets::mtcars))
}

#' Mock function for simple listing
#' 
mock_simple_listing_mm <- function() {
  stopifnot(
    requireNamespace("dv.manager", quietly = TRUE),
    requireNamespace("dplyr", quietly = TRUE),
    requireNamespace("tibble", quietly = TRUE)
  )

  create_dummy <- function(dataset) {
    list(
      mpg = dplyr::select(
        tibble::as_tibble(dataset, rownames = "car"),
        dplyr::all_of(c("car", "mpg"))
      ),
      carb = dplyr::select(
        tibble::as_tibble(dataset, rownames = "car"),
        dplyr::all_of(c("car", "carb"))
      )
    )
  }

  datasets <- list(
    test1 = create_dummy(datasets::mtcars[1:4, ]),
    test2 = create_dummy(datasets::mtcars[5:10, ])
  )

  dv.manager::run_app(
    data = datasets,
    module_list = list(
      "Listing" = mod_simple_listing(
        dv.manager::mm_dispatch("filtered_dataset", "mpg"),
        "mod_listing"
      )
    ),
    filter_data = "mpg",
    filter_key = "car"
  )
}
