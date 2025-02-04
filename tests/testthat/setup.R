# validation (S)
vdoc <- local({
  #                      ##########
  # package_name is used # INSIDE # the sourced file below
  #                      ##########
  package_name <- read.dcf("../../DESCRIPTION")[, "Package"]
  utils_file_path <- system.file("validation", "utils-validation.R", package = package_name, mustWork = TRUE)
  source(utils_file_path, local = TRUE)[["value"]]
})
specs <- vdoc[["specs"]]
#  validation (F)

# YT#VH19ec235e56cdd18f129215603abf0ca6#VH00000000000000000000000000000000#

#' Test harness for communication with `dv.papo`.
#'
#' @param mod Parameterized instance of the module to test. Should produce valid output and not trigger a `shiny::req`.
#' @param data Data matching the previous parameterization.
#' @param trigger_input_id Fully namespaced input ID that, when set to a subject ID value,
#'                         should make the module send `dv.papo` a message.
test_communication_with_papo <- function(mod, data, trigger_input_id) {
  datasets <- shiny::reactive(data)

  afmm <- list(
    data = list(DS = data),
    unfiltered_dataset = datasets,
    filtered_dataset = datasets,
    module_output = function() list(),
    module_names = list(papo = "Papo"),
    utils = list(switch2mod = function(id) NULL),
    dataset_metadata = list(name = shiny::reactive("dummy_dataset_name"))
  )

  app_ui <- function() {
    shiny::fluidPage(mod[["ui"]](mod[["module_id"]]))
  }

  app_server <- function(input, output, session) {
    ret_value <- mod[["server"]](afmm)

    ret_value_update_count <- shiny::reactiveVal(0)
    shiny::observeEvent(ret_value[["subj_id"]](), ret_value_update_count(ret_value_update_count() + 1))

    shiny::exportTestValues(
      ret_value = try(ret_value[["subj_id"]]()), # try because of https://github.com/rstudio/shiny/issues/3768
      update_count = ret_value_update_count()
    )
    return(ret_value)
  }

  app <- shiny::shinyApp(ui = app_ui, server = app_server)

  testthat::test_that("module adheres to send_subject_id_to_papo protocol", {
    app <- shinytest2::AppDriver$new(app, name = "test_send_subject_id_to_papo_protocol")

    app$wait_for_idle()

    # Module starts and sends no message
    exports <- app$get_values()[["export"]]
    testthat::expect_equal(exports[["update_count"]], 0)

    trigger_subject_selection <- function(subject_id) {
      set_input_params <- append(
        as.list(setNames(subject_id, trigger_input_id)),
        list(allow_no_input_binding_ = TRUE, priority_ = "event")
      )
      do.call(app$set_inputs, set_input_params)
    }

    # Module sends exactly one message per trigger event, even if subject does not change
    subject_ids <- c("A", "A", "B")
    for (i in seq_along(subject_ids)) {
      trigger_subject_selection(subject_ids[[i]])
      app$wait_for_idle()

      exports <- app$get_values()[["export"]]
      # Module outputs selection once
      testthat::expect_equal(exports[["ret_value"]], subject_ids[[i]])
      testthat::expect_equal(exports[["update_count"]], i)
    }

    app$stop()
  })
}
