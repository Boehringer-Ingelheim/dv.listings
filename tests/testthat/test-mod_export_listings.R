# Load dummy data for testing purpose
source("dummy-data.R")

# Rewrite server function due to module_id argument which is expected to be named id by shiny::testServer
server_func <- function(
    id,
    dataset_metadata,
    dataset_list,
    data,
    data_selection_name,
    current_rows,
    intended_use_label) {
  mod_export_listings_server(
    module_id = id,
    dataset_metadata = dataset_metadata,
    dataset_list = dataset_list,
    data = data,
    data_selection_name = data_selection_name,
    current_rows = current_rows,
    intended_use_label = intended_use_label
  )
}

test_that("mod_export_listings_UI fails when argument type mismatches", {
  expect_error(mod_export_listings_UI(""))
  expect_error(mod_export_listings_UI(3))
})

test_that("mod_export_listings_UI returns a shiny tagList with three elements", {
  ui <- mod_export_listings_UI("test")

  checkmate::expect_list(ui, len = 3)
  checkmate::expect_class(ui, "shiny.tag.list")
})

test_that("mod_export_listings_server fails when argument type mismatches", {
  # specify valid and invalid server arguments
  id_valid <- "test"
  id_invalid <- list(
    3, # wrong type
    "" # less than one character
  )

  dataset_metadata_valid <- list(
    name = shiny::reactive("test"),
    date_range = shiny::reactive(c("01-01-2000", "01-01-2000"))
  )
  dataset_metadata_invalid <- list(
    c("wrong", "type"), # wrong type
    list(shiny::reactive("test"), shiny::reactive(c("01-01-2000", "01-01-2000"))), # unnamed
    list(name = shiny::reactive("test"), name = shiny::reactive("test2")), # not unique
    list(test1 = shiny::reactive("test"), test2 = shiny::reactive(c("01-01-2000", "01-01-2000"))) # wrong names
  )

  dataset_list_valid <- shiny::reactive({
    list(dm = dm_dummy, ae = ae_dummy)
  })
  dataset_list_invalid <- list(
    c("no_reactive"),
    shiny::reactive({
      c("wrong", "type")
    }), # wrong type
    shiny::reactive({
      list(dm = "test", ae = 3)
    }), # wrong element type
    shiny::reactive({
      list(dm_dummy, ae_dummy)
    }) # unnamed
  )

  data_valid <- shiny::reactive({
    list(data = dm_dummy, col_names = colnames(ae_dummy))
  })
  data_invalid <- list(
    c("wrong", "type") # wrong type
  )

  data_selection_name_valid <- shiny::reactive("dm")
  data_selection_name_invalid <- list(
    shiny::reactive({
      3
    }) # wrong type
  )

  current_rows_valid <- shiny::reactive(seq_len(dim(dm_dummy)[2]))
  current_rows_invalid <- list(
    shiny::reactive({
      c("wrong", "type")
    }) # wrong type
  )

  # execute invalid test cases
  purrr::walk(id_invalid, ~ expect_error(
    shiny::testServer(
      app = server_func,
      expr = {
        session$flushReact()
        expect_true(TRUE)
      },
      args = list(
        id = .x,
        dataset_metadata = dataset_metadata_valid,
        dataset_list = dataset_list_valid,
        data = data_valid,
        data_selection_name = data_selection_name_valid,
        current_rows = current_rows_valid
      )
    )
  ))
  purrr::walk(dataset_metadata_invalid, ~ expect_error(
    shiny::testServer(
      app = server_func,
      expr = {
        session$flushReact()
        expect_true(TRUE)
      },
      args = list(
        id = id_valid,
        dataset_metadata = .x,
        dataset_list = dataset_list_valid,
        data = data_valid,
        data_selection_name = data_selection_name_valid,
        current_rows = current_rows_valid
      )
    )
  ))
  purrr::walk(dataset_list_invalid, ~ expect_error(
    shiny::testServer(
      app = server_func,
      expr = {
        v_dataset_list() # call reactive to trigger error message
        session$flushReact()
        expect_true(TRUE)
      },
      args = list(
        id = id_valid,
        dataset_metadata = dataset_metadata_valid,
        dataset_list = .x,
        data = data_valid,
        data_selection_name = data_selection_name_valid,
        current_rows = current_rows_valid
      )
    )
  ))
  purrr::walk(data_invalid, ~ expect_error(
    shiny::testServer(
      app = server_func,
      expr = {
        session$flushReact()
        expect_true(TRUE)
      },
      args = list(
        id = id_valid,
        dataset_metadata = dataset_metadata_valid,
        dataset_list = dataset_list_valid,
        data = .x,
        data_selection_name = data_selection_name_valid,
        current_rows = current_rows_valid
      )
    )
  ))
  purrr::walk(data_selection_name_invalid, ~ expect_error(
    shiny::testServer(
      app = server_func,
      expr = {
        session$flushReact()
        expect_true(TRUE)
      },
      args = list(
        id = id_valid,
        dataset_metadata = dataset_metadata_valid,
        dataset_list = dataset_list_valid,
        data = data_valid,
        data_selection_name = .x,
        current_rows = current_rows_valid
      )
    )
  ))
  purrr::walk(current_rows_invalid, ~ expect_error(
    shiny::testServer(
      app = server_func,
      expr = {
        session$flushReact()
        expect_true(TRUE)
      },
      args = list(
        id = id_valid,
        dataset_metadata = dataset_metadata_valid,
        dataset_list = dataset_list_valid,
        data = data_valid,
        data_selection_name = data_selection_name_valid,
        current_rows = .x
      )
    )
  ))

  # verify that valid arguments launch the server as intended
  expect_success(
    shiny::testServer(
      app = server_func,
      expr = {
        v_dataset_list() # call reactive to trigger error message
        session$flushReact()
        expect_true(TRUE)
      },
      args = list(
        id = id_valid,
        dataset_metadata = dataset_metadata_valid,
        dataset_list = dataset_list_valid,
        data = data_valid,
        data_selection_name = data_selection_name_valid,
        current_rows = current_rows_valid,
        intended_use_label = NULL
      )
    )
  )
})

test_that("mod_export_listings_server updates file type choices when switching between single and all datasets" |> # nolint
  vdoc[["add_spec"]](specs$export), {
  # server arguments
  id <- "test"
  dataset_metadata <- list(
    name = shiny::reactive("test"),
    date_range = shiny::reactive(c("01-01-2000", "01-01-2000"))
  )
  dataset_list <- shiny::reactive({
    list(dm = dm_dummy, ae = ae_dummy)
  })
  data <- shiny::reactive({
    list(data = dm_dummy, col_names = colnames(dm_dummy))
  })
  data_selection_name <- shiny::reactive("dm")
  current_rows <- shiny::reactive(seq_len(dim(dm_dummy)[2]))

  # perform tests
  shiny::testServer(
    app = server_func,
    expr = {
      # initial expectation
      # note: id is hard coded because pack of constants cannot be used within session$setInputs()
      session$setInputs(which_data = "single")
      actual_choices <- type_choices()
      expected_choices <- c("Excel" = ".xlsx", "PDF" = ".pdf")
      expect_equal(actual_choices, expected_choices)

      # after selection switch
      session$setInputs(which_data = "all")
      actual_choices <- type_choices()
      expected_choices <- c("Excel" = ".xlsx")
      expect_equal(actual_choices, expected_choices)
    },
    args = list(
      id = id,
      dataset_metadata = dataset_metadata,
      dataset_list = dataset_list,
      data = data,
      data_selection_name = data_selection_name,
      current_rows = current_rows,
      intended_use_label = NULL
    )
  )
})

test_that("mod_export_listings_server hides (shows) warning if checkbox is (not) ticked", {
  # server arguments
  id <- "test"
  dataset_metadata <- list(
    name = shiny::reactive("test"),
    date_range = shiny::reactive(c("01-01-2000", "01-01-2000"))
  )
  dataset_list <- shiny::reactive({
    list(dm = dm_dummy, ae = ae_dummy)
  })
  data <- shiny::reactive({
    list(data = dm_dummy, col_names = colnames(dm_dummy))
  })
  data_selection_name <- shiny::reactive("dm")
  current_rows <- shiny::reactive(seq_len(dim(dm_dummy)[2]))

  # perform tests
  shiny::testServer(
    app = server_func,
    expr = {
      # after selection switch
      session$setInputs(name = "name", check = TRUE) # name must be set manually to avoid errors
      actual_label <- checkbox_label()
      actual_label <- gsub("<[^>]+>", "", actual_label) # remove HTML tags
      actual_label <- gsub("[\r\n] *", "", actual_label) # remove newline tags and multiple spacing
      expected_label <- paste(EXP$DATAPROTECT_LABEL, intended_use_label)
      expect_equal(actual_label, expected_label)

      # after further selection switch
      session$setInputs(check = FALSE) # name must be set manually to avoid errors
      actual_label <- checkbox_label()
      actual_label <- gsub("<[^>]+>", "", actual_label) # remove HTML tags
      actual_label <- gsub("[\r\n] *", "", actual_label) # remove newline tags and multiple spacing
      expected_label <- paste0(EXP$DATAPROTECT_LABEL, " ", intended_use_label, EXP$DATAPROTECT_WARN)
      expect_equal(actual_label, expected_label)
    },
    args = list(
      id = id,
      dataset_metadata = dataset_metadata,
      dataset_list = dataset_list,
      data = data,
      data_selection_name = data_selection_name,
      current_rows = current_rows,
      intended_use_label = ""
    )
  )
})

test_that("mod_export_listings_server en-/disables download button if prerequesites are (not) met", {
  # server arguments
  id <- "test"
  dataset_metadata <- list(
    name = shiny::reactive("test"),
    date_range = shiny::reactive(c("01-01-2000", "01-01-2000"))
  )
  dataset_list <- shiny::reactive({
    list(dm = dm_dummy, ae = ae_dummy)
  })
  data <- shiny::reactive({
    list(data = dm_dummy, col_names = colnames(dm_dummy))
  })
  data_selection_name <- shiny::reactive("dm")
  current_rows <- shiny::reactive(seq_len(dim(dm_dummy)[2]))

  # perform tests
  shiny::testServer(
    app = server_func,
    expr = {
      # ticking the checkbox and inserting file name should enable download button
      session$setInputs(name = "name", check = TRUE)
      expect_equal(download_enable(), TRUE)

      # removing file name should disable download button
      session$setInputs(name = "")
      expect_equal(download_enable(), FALSE)

      # reentering file name should enable download button
      session$setInputs(name = "test")
      expect_equal(download_enable(), TRUE)
    },
    args = list(
      id = id,
      dataset_metadata = dataset_metadata,
      dataset_list = dataset_list,
      data = data,
      data_selection_name = data_selection_name,
      current_rows = current_rows,
      intended_use_label = NULL
    )
  )
})

test_that("mod_export_listings_server closes the export pop-up window via modal button or via easy close functionality", { # nolint
  skip("Modal dialogue is provided by shiny package and tested there.")
})

test_that("mod_export_listings_server places exported files in the local download folder under the prespecified file name", { # nolint
  skip("Cannot be tested automatically on a local machine.")
})

app_dir <- "./apps/mm_app"

test_that("mock_listings_mm exports all pages when downloading the currently displayed table in case of pagination turned on" |> # nolint
  vdoc[["add_spec"]](specs$export_active_listing), { # nolint
  # Initialize test app
  app <- shinytest2::AppDriver$new(
    app_dir = app_dir, name = "test_restore_row_order"
  )

  # Click buttons
  app$wait_for_idle()
  app$click("multi-export-download_data")
  app$wait_for_idle()

  # Check if length(current_rows()) > 10 (more than one page)
  testthat::expect_identical(
    length(app$get_value(export = "multi-export-current_rows")),
    as.integer(100)
  )

  app$stop()
})
