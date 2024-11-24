test_that("listings_UI() fails when argument type mismatches", {
  expect_error(listings_UI(""))
  expect_error(listings_UI(3))
})


server_func <- function(id, dataset_list, default_vars, dataset_metadata, pagination = NULL, intended_use_label) {
  listings_server(
    module_id = id, dataset_list = dataset_list, default_vars = default_vars,
    dataset_metadata = dataset_metadata, pagination = pagination,
    intended_use_label = intended_use_label
  )
}

test_that("listings_server() fails when argument type mismatches", {
  # Prepare test arguments
  id_valid <- "test"
  id_num <- 3
  id_zero <- ""

  data_valid <- shiny::reactive({
    list(dm = dm_dummy, ae = ae_dummy)
  })
  data_no_df <- shiny::reactive(list(string = "Some text", num = 1))
  data_no_list <- shiny::reactive(ae_dummy)
  data_null <- shiny::reactive(NULL)
  data_unnamed <- shiny::reactive(list(dm_dummy, ae_dummy))

  cols_valid <- list(dm = "USUBJID", ae = c("AETERM", "AESEV"))
  cols_null <- NULL
  cols_no_list <- "USUBJID"
  cols_unnamed <- list("USUBJID", c("AETERM", "AESEV"))

  metadata_valid <- list(
    name = shiny::reactive("test"),
    date_range = shiny::reactive(c("01-01-2000", "31-12-2000"))
  )
  metadata_wrong_type <- c("wrong", "type")
  metadata_unnamed <- list(shiny::reactive("test"), shiny::reactive(c("01-01-2000", "31-12-2000")))
  metadata_not_unique <- list(name = shiny::reactive("test"), name = shiny::reactive("test2"))
  metadata_wrong_names <- list(
    test1 = shiny::reactive("test"),
    test2 = shiny::reactive(c("01-01-2000", "31-12-2000"))
  )

  pagination_valid <- NULL
  pagination_num <- 1
  pagination_char <- "wrong"

  # Cases that expect an error
  test_id <- list(id_num, id_zero) # list to preserve types
  test_dataset_list <- list(data_no_df, data_no_list, data_unnamed)
  test_cols <- list(cols_no_list, cols_unnamed)
  test_metadata <- list(metadata_wrong_type, metadata_unnamed, metadata_not_unique, metadata_wrong_names)
  test_pagination <- list(pagination_num, pagination_char)

  # Execute test cases
  purrr::walk(test_id, ~ expect_error(
    shiny::testServer(server_func, args = list(
      id = .x, dataset_list = data_valid,
      default_vars = cols_valid, dataset_metadata = metadata_valid,
      pagination = pagination_valid
    ), {
      session$flushReact()
      expect_true(TRUE) # to silence error that is caused from missing expectations
    })
  )) # test module_id parameter
  purrr::walk(test_dataset_list, ~ expect_error( # expect a warning since checkmate call is in a reactive call
    shiny::testServer(server_func, args = list(
      id = id_valid, dataset_list = .x,
      default_vars = cols_valid, dataset_metadata = metadata_valid,
      pagination = pagination_valid
    ), {
      v_dataset_list() # call reactive to trigger error message
      session$flushReact()
      expect_true(TRUE) # to silence error that is caused from missing expectations
    })
  ))
  purrr::walk(test_cols, ~ expect_error(
    shiny::testServer(server_func, args = list(
      id = id_valid, dataset_list = data_valid,
      default_vars = .x, dataset_metadata = metadata_valid,
      pagination = pagination_valid
    ), {
      session$setInputs(dataset = "dm") # needed to avoid warning from missing input
      session$flushReact()
      expect_true(TRUE) # to silence error that is caused from missing expectations
    })
  ))

  purrr::walk(test_metadata, ~ expect_error(
    shiny::testServer(server_func, args = list(
      id = id_valid, dataset_list = data_valid,
      default_vars = cols_valid, dataset_metadata = .x,
      pagination = pagination_valid
    ), {
      session$flushReact()
      expect_true(TRUE) # to silence error that is caused from missing expectations
    })
  )) # test dataset_metadata parameter

  purrr::walk(test_pagination, ~ expect_error(
    shiny::testServer(server_func, args = list(
      id = id_valid, dataset_list = data_valid,
      default_vars = cols_valid, dataset_metadata = metadata_valid,
      pagination = .x
    ), {
      session$flushReact()
      expect_true(TRUE) # to silence error that is caused from missing expectations
    })
  )) # test pagination parameter

  # Verify that valid arguments launch the server as intended
  expect_success(
    shiny::testServer(
      server_func,
      args = list(
        id = id_valid,
        dataset_list = data_valid,
        default_vars = cols_valid,
        dataset_metadata = metadata_valid,
        intended_use_label = NULL
      ),
      {
        session$setInputs(dataset = "dm") # needed to avoid warning from missing input
        session$flushReact()
        expect_true(TRUE) # to silence error that is caused from missing expectations
      }
    )
  )
})

test_that("listings_server() saves default_vars in the selected_columns_in_dataset at app launch" %>%
  vdoc[["add_spec"]](specs$default_vars), {
  # Prepare test parameters
  dataset_list <- list(dm = dm_dummy, ae = ae_dummy)
  default_vars <- list(dm = c("USUBJID", "AGE", "SEX"), ae = c("USUBJID", "AETERM", "AESEV"))
  dataset_metadata <- list(
    name = shiny::reactive("trial_xy"),
    date_range = shiny::reactive(c("01-01-2000", "31-12-2000"))
  )

  # Perform tests
  shiny::testServer(
    server_func,
    args = list(
      dataset_list = shiny::reactive({
        dataset_list
      }),
      default_vars = default_vars,
      dataset_metadata = dataset_metadata,
      intended_use_label = NULL
    ),
    {
      expect_equal(r_selected_columns_in_dataset(), default_vars)
    }
  )
})

test_that("listings_server() adds default variables, if not specified by the app creator" %>%
  vdoc[["add_spec"]](specs$default_vars), {
  # Prepare test parameters
  dataset_list <- list(dm = dm_dummy, ae = ae_dummy)
  default_vars <- list(dm = c("USUBJID", "AGE", "SEX"))
  expected_cols <- default_vars %>%
    purrr::list_modify(ae = names(dataset_list$ae)[1:6])
  dataset_metadata <- list(
    name = shiny::reactive("trial_xy"),
    date_range = shiny::reactive(c("01-01-2000", "31-12-2000"))
  )

  # Perform tests
  shiny::testServer(
    server_func,
    args = list(
      dataset_list = shiny::reactive({
        dataset_list
      }),
      default_vars = default_vars,
      dataset_metadata = dataset_metadata,
      intended_use_label = NULL
    ),
    {
      session$setInputs(dataset = "ae")
      expect_equal(r_selected_columns_in_dataset(), expected_cols)
    }
  )
})

app_dir <- "./apps/bookmarking_app"

# Initialize test app
app <- shinytest2::AppDriver$new(
  app_dir = app_dir, name = "bookmarking_app"
)

app_dir <- app$get_url()

test_that("listings_server() stores the selected_columns_in_dataset and the currently selected dataset for bookmarking" %>% # nolint
  vdoc[["add_spec"]](specs$retain_last_selection), { # nolint
  # Initialize test app
  app <- shinytest2::AppDriver$new(
    app_dir = app_dir, name = "test_on_bookmark_TAB-SO-763"
  )

  app_dir <- app$get_url()

  app$set_inputs(`listings-dataset` = "dummy2")
  app$wait_for_idle() # make sure inputs are updated before bookmarking
  app$set_inputs(`listings-col_sel` = c("var2", "var4"))
  app$wait_for_idle()
  app$set_inputs(!!"._bookmark_" := "click")

  tst <- app$get_value(export = "state")
  bmk_url <- app$get_value(export = "url")
  query_string_list <- parseQueryString(bmk_url, nested = TRUE)

  expected_elements <- c("dummy1", "var1", "var2", "var3", "dummy2", "var2", "var4")
  exist <- purrr::map2_lgl(
    expected_elements,
    query_string_list$`listings-selected_columns_in_dataset`,
    grepl
  )

  testthat::expect_true(all(exist))
  testthat::expect_true(grepl("dummy2", query_string_list$`listings-data_sel`))
})

test_that("listings_server() restores the selected_columns_in_dataset and the currently selected dataset for bookmarking" %>% # nolint
  vdoc[["add_spec"]](specs$retain_last_selection), { # nolint
  # Initialize test app
  app <- shinytest2::AppDriver$new(
    app_dir = app_dir, name = "test_restore_bookmark_TAB-SO-777"
  )

  # Capture the background app's URL and add appropriate query parameters
  bk_url <- paste0(app$get_url(), "?_inputs_&listings-dropdown_btn_state=false&listings-col_sel=%5B%22var2%22%2C%22var4%22%5D&listings-dataset=%22dummy2%22&listings-export-download_data=0&listings-dropdown_btn=2&_values_&listings-selected_columns_in_dataset=%7B%22dummy1%22%3A%5B%22var1%22%2C%22var2%22%2C%22var3%22%5D%2C%22dummy2%22%3A%5B%22var2%22%2C%22var4%22%5D%7D&listings-data_sel=%22dummy2%22") # nolint
  # Open the bookmark URL in a new AppDriver object
  app <- shinytest2::AppDriver$new(app_dir = bk_url, name = "test_restore_bookmark")

  actual <- app$get_values(export = c("listings-selected_columns_in_dataset", "listings-data_sel"))

  expected <- list(export = list(
    `listings-selected_columns_in_dataset` = list(
      dummy1 = c("var1", "var2", "var3"),
      dummy2 = c("var2", "var4")
    )
  ))

  testthat::expect_equal(actual, expected)
})

test_that("listings_server() allows bookmarking for dataset and column selections" %>%
  vdoc[["add_spec"]](specs$bookmarking), {
  # Initialize test app
  app <- shinytest2::AppDriver$new(
    app_dir = app_dir, name = "test_on_bookmark_TAB-SO-777"
  )

  # Capture the background app's URL and add appropriate query parameters
  bk_url <- paste0(app$get_url(), "?_inputs_&listings-dropdown_btn_state=false&listings-col_sel=%5B%22var2%22%2C%22var4%22%5D&listings-dataset=%22dummy2%22&listings-export-download_data=0&listings-dropdown_btn=1&_values_&listings-selected_columns_in_dataset=%7B%22dummy1%22%3A%5B%22var1%22%2C%22var2%22%2C%22var3%22%5D%2C%22dummy2%22%3A%5B%22var2%22%2C%22var4%22%5D%7D&listings-data_sel=%22dummy2%22") # nolint

  # Open the bookmark URL in a new AppDriver object
  app <- shinytest2::AppDriver$new(app_dir = bk_url, name = "test_on_bookmark")

  actual <- app$get_values(input = c("listings-dataset", "listings-col_sel"))

  expected <- list(input = list(
    `listings-col_sel` = c("var2", "var4"),
    `listings-dataset` = "dummy2"
  ))

  testthat::expect_equal(actual, expected)

  app$stop()
})

test_that("listings_server() displays default columns after SSO redirect", {
  skip("Cannot integrate SSO in unit test, i.e. this test has to be performed manually.")
})

# start table test app
app <- shinytest2::AppDriver$new(
  app_dir = "./apps/listings_app", name = "listings_app"
)
app_dir <- app$get_url()

test_that("mod_listings() fails when argument types mismatch", {
  # Prepare parameters to test
  disp_no_list <- "Not a list" # Parameter not a list at all
  disp_no_char <- list(from = 3, selection = "adae") # Parameter not a list of characters

  disp_no_names <- list("filtered_dataset", "adsl") # Parameter not named at all
  class(disp_no_names) <- "mm_dispatcher" # correct

  disp_wrong_names <- list(a = "filtered_dataset", b = "adsl") # Parameter not named correctly
  class(disp_wrong_names) <- "mm_dispatcher" # correct

  disp_wrong_class <- list(from = "unfiltered_dataset", selection = "adae") # Correct list structure but ...
  class(disp_wrong_class) <- "character" # ... wrong class

  test_cases <- c(disp_no_list, disp_no_char, disp_no_names, disp_wrong_names, disp_wrong_class)

  # Perform tests
  purrr::walk(test_cases, ~ expect_error(mod_listings(dataset_disp = .x, module_id = "test_id")))


  dataset_names_no_chr <- 1
  dataset_names_list <- list("adsl", "adae")

  names_test_cases <- list(dataset_names_no_chr, dataset_names_list)

  # Perform tests
  purrr::walk(names_test_cases, ~ expect_error(mod_listings(dataset_names = .x, module_id = "test_id")))
})

test_that("mod_listings() fails when both or none of dataset_names and dataset_disp are specified", {
  dataset_disp <- dv.manager::mm_dispatch("filtered_dataset", c("adsl"))
  dataset_names <- c("adsl")

  # throw error because both are specified
  expect_error(mod_listings(module_id = "test_id", dataset_names = dataset_names, dataset_disp = dataset_disp))

  # throw error because both are not specified
  expect_error(mod_listings(module_id = "test_id", dataset_names = NULL, dataset_disp = NULL))
})

test_that("mod_listings() returns a list containing all information for dv.manager", {
  # Valid parameters
  disp <- dv.manager::mm_dispatch("filtered_dataset", "adsl")
  id <- "test_id"

  # Return value
  outcome <- mod_listings(dataset_disp = disp, module_id = id)

  # Perform tests
  checkmate::expect_list(outcome, len = 3, names = "named") # Must be a list
  checkmate::expect_names(names(outcome), permutation.of = c("ui", "server", "module_id")) # Must have those names
  checkmate::expect_function(outcome$ui) # ui entry must be a function
  checkmate::expect_function(outcome$server) # server entry must be a function
  expect_equal(outcome$module_id, id) # must not modify the id
})



test_that("mod_listings() displays a data table, dataset selector and corresponding column selector at app launch" %>%
  vdoc[["add_spec"]](
    c(
      specs$display_listing,
      specs$listing_selection,
      specs$column_selection
    )
  ), {
  # Initialize test app
  app <- shinytest2::AppDriver$new(
    app_dir = app_dir, name = "test_initial_state"
  )

  dataset_sel <- app$get_value(input = "listings-dataset")
  column_sel <- app$get_value(input = "listings-col_sel")
  table_out <- app$get_value(output = "listings-listing")

  # Verify that required elements exist
  testthat::expect_true(!is.null(dataset_sel) && !is.null(column_sel) && !is.null(table_out))
})

test_that("mod_listings() restores row order of the whole table when restoring a sorted variable" %>%
  vdoc[["add_spec"]](
    c(
      specs$restore_row_order,
      specs$sorting_columns
    )
  ), {
  # Initialize test app
  app <- shinytest2::AppDriver$new(
    app_dir = app_dir, name = "test_restore_row_order"
  )

  # Needed buttons to click
  sort_selector <- '.dt-center.sorting[aria-label="var1 [My 1st label]: activate to sort column ascending"]'
  reset_selector <- "div.dt-buttons>button"

  # Perform steps within test app
  app$wait_for_idle()
  initial_rows <- app$get_value(input = "listings-listing_rows_all")
  app$click(selector = sort_selector)
  app$wait_for_idle()
  sorted_rows <- app$get_value(input = "listings-listing_rows_all")
  app$click(selector = reset_selector)
  app$wait_for_idle()
  reset_rows <- app$get_value(input = "listings-listing_rows_all")

  # Perform test that row order changed and then gets restored
  testthat::expect_false(all(initial_rows == sorted_rows))
  testthat::expect_identical(initial_rows, reset_rows)
})

app_dir <- "./apps/mm_app" # applies for all tests within this describe()

app <- shinytest2::AppDriver$new(
  app_dir = app_dir, name = "test_launch_mm"
)
app_dir <- app$get_url()

test_that("mock_listings_mm() launches successfully the module via dv.manager", {
  # Initialize test app
  app <- shinytest2::AppDriver$new(
    app_dir = app_dir, name = "test_launch_mm"
  )
  app$wait_for_idle()

  value_list <- app$get_values(
    input = c("multi-dataset", "multi-col_sel"),
    export = "multi-output_table"
  )
  # check availability of:
  # values for dataset and column selection
  # output dataframe

  # Only the existence of values are checked not the actual values
  col_sel <- length(value_list[["input"]][["multi-col_sel"]]) > 0
  data_sel <- length(value_list[["input"]][["multi-dataset"]]) == 1

  out_table <- (nrow(value_list[["export"]][["multi-output_table"]]) > 0 &&
    ncol(value_list[["export"]][["multi-output_table"]]) > 0) # nolint

  # Verify that module can be launched via module manager by expecting values for selectors and a output dataframe
  testthat::expect_true((col_sel && data_sel && out_table))
}) # integration

test_that("mock_table_mm() displays the column names with the corresponding labels" %>%
  vdoc[["add_spec"]](specs$column_label), {
  # Initialize test app
  app <- shinytest2::AppDriver$new(
    app_dir = app_dir, name = "test_col_labels_TAB-SO-760"
  )

  expected <- c(
    "STUDYID [No label]", "DOMAIN [Domain Abbreviation]",
    "USUBJID [Unique Subject Identifier]", "SUBJID [Subject Identifier for the Study]",
    "RFSTDTC [Subject Reference Start Date/Time]", "RFENDTC [Subject Reference End Date/Time]",
    "RFXSTDTC [Date/Time of First Study Treatment]", "RFXENDTC [Date/Time of Last Study Treatment]"
  )

  actual <- app$get_value(export = "multi-column_names")

  # Verify that dataset choices are displayed properly with their labels
  testthat::expect_equal(actual, expected)
})

test_that("mock_table_mm() updates dropdown choices on dataset change in dv.manager" %>%
  vdoc[["add_spec"]](specs$listings_label), {
  # Initialize test app
  app <- shinytest2::AppDriver$new(
    app_dir = app_dir, name = "test_update_labels"
  )
  app$wait_for_idle()
  
  app$set_inputs(selector = "demo", wait_ = FALSE)
  app$wait_for_idle()
  
  expected <- c(
    "adsl [Subject Level]" = "adsl",
    "adae [Adverse Events]" = "adae",
    "small [Few columns]" = "small"
  )
  actual <- app$get_value(export = "multi-dataset_choices")
  
  actual <- app$wait_for_value(
    export = "multi-dataset_choices", ignore = list(NULL), timeout = 5e3
  )
  testthat::expect_equal(actual, expected = expected)

  app$set_inputs(selector = "demo no labels") # Switch overall dataset (via module manager)
  app$click("multi-dropdown_btn")
  app$set_inputs(`multi-dropdown_btn_state` = TRUE)

  expected <- c(
    "adsl [No label]" = "adsl",
    "adae [No label]" = "adae",
    "small [No label]" = "small"
  )
  actual <- app$wait_for_value(
    export = "multi-dataset_choices", ignore = list(actual), timeout = 10e3
  )
  app$stop()
  testthat::expect_equal(actual, expected = expected)
}) # integration

test_that("mock_table_mm() displays no table when global filter returns an empty data.frame", {
  # Initialize test app
  app <- shinytest2::AppDriver$new(
    app_dir = app_dir, name = "test_empty_df",
    timeout = 6000, load_timeout = 30000
  )

  # Choose global filter settings that lead to an empty data.frame
  # NOTE: It is not possible to put those two set_input() lines into only one call since we need to wait until the
  # first input is available! (Second call wouldn't be able to find it otherwise.)
  app$set_inputs(`global_filter-vars` = "RACE")
  app$wait_for_idle()
  app$set_inputs(`global_filter-RACE` = character(0))
  app$wait_for_idle(duration = 3000)

  # Verify that a table with zero rows is shown

  dataset <- app$get_value(export = "multi-output_table")
  actual <- nrow(dataset)

  testthat::expect_equal(actual, expected = 0)
}) # integration

test_that("mock_table_mm() displays selected columns after activating global filter", {
  # Initialize test app
  app <- shinytest2::AppDriver$new(
    app_dir = app_dir, name = "test_global_filter_selected_cols"
  )

  # Set selected columns
  selected_cols <- c("STUDYID", "USUBJID")
  app$set_inputs(`multi-col_sel` = selected_cols)
  app$wait_for_idle()

  # Activate global filter
  app$set_inputs(`global_filter-vars` = "RACE")
  app$wait_for_idle()

  actual <- app$get_value(input = "multi-col_sel")

  testthat::expect_equal(actual, expected = selected_cols)
}) # integration

test_that("mock_table_mm() displays selected dataset after activating global filter", {
  # Initialize test app
  app <- shinytest2::AppDriver$new(
    app_dir = app_dir, name = "test_global_filter_selected_dataset"
  )

  selected <- "adae"
  # Switch dataset
  app$set_inputs(`multi-dataset` = selected)
  app$wait_for_idle()

  # Activate global filter
  app$set_inputs(`global_filter-vars` = "RACE")
  app$wait_for_idle()

  actual <- app$get_value(input = "multi-dataset")

  # Kill test app
  app$stop()

  testthat::expect_equal(actual, expected = selected)
}) # integration


test_that("mock_table_mm() displays selected dataset after activating global filter", {
  # Initialize test app
  app <- shinytest2::AppDriver$new(
    app_dir = app_dir, name = "test_global_filter_selected_dataset"
  )

  selected <- "adae"
  # Switch dataset
  app$set_inputs(`multi-dataset` = selected)
  app$wait_for_idle()

  # Activate global filter
  app$set_inputs(`global_filter-vars` = "RACE")
  app$wait_for_idle()

  actual <- app$get_value(input = "multi-dataset")

  # Kill test app
  app$stop()

  testthat::expect_equal(actual, expected = selected)
}) # integration

app_dir <- "./apps/listings_app" # applies for all tests within this describe()
app <- shinytest2::AppDriver$new(app_dir = app_dir, name = "test_listings_app")
app_dir <- app$get_url()

test_that("Check select all columns works correctly", {
  app <- shinytest2::AppDriver$new(
    app_dir = app_dir, name = "test_select_all_columns"
  )

  # SET INITIAL DATASET
  app$click("listings-dropdown_btn")
  app$set_inputs(`listings-dropdown_btn_state` = TRUE, wait_ = FALSE)
  app$set_inputs(`listings-dataset` = "dummy1", wait_ = FALSE) # set to simple_dummy data

  # CHECK ALL COLS SELECTED
  app$click("listings-select_all_cols_btn")
  actual <- app$get_value(input = "listings-col_sel")
  app$stop()

  expected <- names(simple_dummy)
  testthat::expect_equal(actual, expected)
}) # integration

test_that("Check unselect all columns works correctly", {
  app <- shinytest2::AppDriver$new(
    app_dir = app_dir, name = "test_unselect_all_columns"
  )

  # SET INITIAL DATASET
  app$click("listings-dropdown_btn")
  app$set_inputs(`listings-dropdown_btn_state` = TRUE, wait_ = FALSE)
  app$set_inputs(`listings-dataset` = "dummy1", wait_ = FALSE) # set to simple_dummy data

  # CHECK ALL COLS UNSELECTED
  app$click("listings-remove_all_cols_btn")
  actual <- app$get_value(input = "listings-col_sel")
  app$stop()
  testthat::expect_null(actual)
}) # integration

test_that("Check reset all columns works correctly", {
  app <- shinytest2::AppDriver$new(
    app_dir = app_dir, name = "test_reset_columns"
  )

  # SET INITIAL DATASET
  app$click("listings-dropdown_btn")
  app$set_inputs(`listings-dropdown_btn_state` = TRUE, wait_ = FALSE)
  app$set_inputs(`listings-dataset` = "dummy1", wait_ = FALSE) # set to simple_dummy data

  # CHECK COLS RESET TO DEFAULT VARS
  app$click("listings-reset_cols_btn")
  actual <- app$get_value(input = "listings-col_sel")
  app$stop()
  expected <- names(simple_dummy)[1:3]

  testthat::expect_equal(actual, expected)
}) # integration

test_that("Check reset filters works correctly", {
  app <- shinytest2::AppDriver$new(
    app_dir = app_dir, name = "test_reset_filters"
  )
  
  # SET INITIAL DATASET
  app$click("listings-dropdown_btn")
  app$set_inputs(`listings-dropdown_btn_state` = TRUE, wait_ = FALSE)
  app$set_inputs(`listings-dataset` = "dummy1", wait_ = FALSE) # set to simple_dummy data
  expected <- app$wait_for_value(
    output = "listings-listing", ignore = list(NULL), timeout = 800
  )
  
  # SET COL FILTERS
  app$set_inputs(
    `listings-listing_search_columns` = c("4 ... 31", "23 ... 48", "2 ... 30"), 
    allow_no_input_binding_ = TRUE
  )
  
  # PRESS RESET FILT BUTTON
  app$click("listings-reset_filt_btn")
  actual <- app$get_value(output = "listings-listing")
  
  testthat::expect_equal(actual, expected)
}) # integration
