#' A module that displays a single dataset in a table form
#'
#' @description This module shows a given dataset in a table form.
#'
#' @template id_param
#'
#' @importFrom shiny NS tagList
#' @import shiny
#'
#' @name simple_listing
NULL

#' @describeIn simple_listing UI
#'
simple_listing_UI <- function(module_id) { # nolint
  ns <- NS(module_id)
  tagList(
    DT::dataTableOutput(ns("table"))
  )
}

#' @describeIn simple_listing server
#'
#' @param dataset a data.framish dataset that will be shown as a table
simple_listing_server <- function(module_id, dataset) {
  module <- function(input, output, session) {
    output$table <- DT::renderDataTable({
      dataset()
    })
  }
  moduleServer(module_id, module)
}

utils::globalVariables("filtered_datasets")

#' @describeIn simple_listing module
#'
#' @param dataset_disp An mm_dispatch object.
#' @template module_id-arg
#'
#' @family data_table
mod_simple_listing <- function(dataset_disp, module_id) {
  mod <- list(
    ui = simple_listing_UI,
    server = function(afmm) {
      simple_listing_server(
        dataset = dv.manager::mm_resolve_dispatcher(dataset_disp, afmm, flatten = TRUE),
        module_id = module_id
      )
    },
    module_id = module_id
  )
  return(mod)
}
