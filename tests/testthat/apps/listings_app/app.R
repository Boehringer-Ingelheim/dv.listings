use_load_all <- isTRUE(as.logical(Sys.getenv("TEST_LOCAL"))) | isTRUE(as.logical(Sys.getenv("TEST_PUSH")))
if (use_load_all) {
  devtools::load_all("../../../../", quiet = TRUE)
} else {
  library(dv.listings)
}

# Load dummy data for testing purpose
source("../../dummy-data.R")

# Test app
test_ui <- function(id) { # nolint
  ns <- ifelse(is.character(id), shiny::NS(id), shiny::NS(NULL))
  shiny::fluidPage(listings_UI(ns("listings")))
}

test_server <- function(input, output, session) {
  dummy1 <- simple_dummy
  dummy2 <- simple_dummy[1:10, 2:8]
  attributes(dummy1$var1)$label <- "My 1st label"
  attributes(dummy1$var2)$label <- "My 2nd label"
  data <- list("dummy1" = dummy1, "dummy2" = dummy2)
  def_vars <- list("dummy1" = names(dummy1[1:3]), "dummy2" = names(dummy2))

  listings_server(
    "listings",
    dataset_list = shiny::reactive(data),
    default_vars = def_vars,
    dataset_metadata = list(
      name = shiny::reactive("test_name"),
      date_range = shiny::reactive(c("2022-01-01", "2022-12-03"))
    ),
    intended_use_label = NULL
  )
}

shiny::shinyApp(test_ui, test_server)
