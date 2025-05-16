# Load dummy data for testing purpose
source("dummy-data.R")

test_that("fill_default_vars() throws an error when dataset argument type mismatches", {
  # Correct default_vars argument
  default_vars <- NULL

  # Wrong dataset arguments
  type_mismatch <- "Not a list." # wrong argument type
  element_mismatch <- list(a = letters[1:6], b = 1:10) # wrong list element types
  unnamed <- list(simple_dummy, data.frame(test = 1:10)) # missing names
  duplicated_names <- list(data1 = simple_dummy, data1 = data.frame(test = 1:10)) # duplicated names
  inexistent <- NULL # no data

  # Perform tests
  purrr::walk(
    list(type_mismatch, element_mismatch, unnamed, duplicated_names, inexistent),
    ~ expect_error(fill_default_vars(default_vars, .x))
  )
})

test_that("fill_default_vars() throws an error when default_vars argument type mismatches", {
  dummy_names <- names(simple_dummy)

  # Wrong default_vars argument

  type_mismatch <- "Not a list." # wrong argument type
  element_mismatch <- list(dummy1 = 1:6, dummy2 = 1:10) # wrong list element types
  unnamed <- list(dummy_names[1:4], dummy_names[1:8]) # missing names
  duplicated_names <- list(dummy1 = dummy_names[1:4], dummy1 = dummy_names[1:8]) # duplicated names
  duplicated_values <- list( # duplicated vector entries
    dummy1 = c(dummy_names[1:4], dummy_names[1:2]), dummy2 = dummy_names[1:8]
  )
  wrong_values <- list(dummy1 = c(dummy_names[1:4], "wrong"), dummy2 = dummy_names[1:8]) # wrong vector entries

  # Correct dataset argument
  dataset <- list(dummy1 = simple_dummy, dummy2 = simple_dummy)

  # Perform tests
  purrr::walk(list(
    type_mismatch, element_mismatch, unnamed, duplicated_names, duplicated_values, wrong_values
  ), ~ expect_error(fill_default_vars(.x, dataset)))
})

test_that("fill_default_vars() returns default_vars without transformation", {
  # Prepare dataset and default_vars
  dataset <- list(dummy1 = simple_dummy, dummy2 = simple_dummy)
  default_vars <- list(dummy1 = names(simple_dummy)[1:4], dummy2 = names(simple_dummy)[1:8])

  # Run function
  default_vars_res <- fill_default_vars(default_vars, dataset)
  default_vars_exp <- default_vars

  # Perform test
  expect_identical(default_vars_res, default_vars_exp) # check values (as well as types, names and lengths)
})

test_that("fill_default_vars() fills all entries of default_vars witth 6 column names in case of not prespecified", {
  # Prepare dataset and default_vars
  dataset <- list(dummy1 = simple_dummy, dummy2 = simple_dummy)
  default_vars <- NULL

  # Run function
  default_vars_res <- fill_default_vars(default_vars, dataset)
  default_vars_exp <- list(dummy1 = names(simple_dummy)[1:6], dummy2 = names(simple_dummy)[1:6])

  # Perform test
  expect_identical(default_vars_res, default_vars_exp) # check values (as well as types, names and lengths)
})

test_that("fill_default_vars() fills single missing entry of default_vars with 6 column names", {
  # Prepare dataset and default_vars
  dataset <- list(dummy1 = simple_dummy, dummy2 = simple_dummy, dummy3 = simple_dummy)
  default_vars <- list(dummy1 = names(simple_dummy)[1:4], dummy3 = names(simple_dummy)[1:8])

  # Run function
  default_vars_res <- fill_default_vars(default_vars, dataset)
  default_vars_exp <- list(
    dummy1 = names(simple_dummy)[1:4], dummy2 = names(simple_dummy)[1:6], dummy3 = names(simple_dummy)[1:8]
  )

  # Perform test
  expect_identical(default_vars_res, default_vars_exp) # check values (as well as types, names and lengths)
})

test_that("fill_default_vars() fills default_vars only with available columns in case of small datasets", {
  # Prepare dataset and default_vars
  dataset <- list(dummy1 = simple_dummy, dummy2 = simple_dummy[, 1:4], dummy3 = simple_dummy[, 1:2])
  default_vars <- NULL

  # Run function
  default_vars_res <- fill_default_vars(default_vars, dataset)
  default_vars_exp <- list(
    dummy1 = names(simple_dummy)[1:6], dummy2 = names(simple_dummy)[1:4], dummy3 = names(simple_dummy)[1:2]
  )

  # Perform test
  expect_identical(default_vars_res, default_vars_exp) # check values (as well as types, names and lengths)
})



test_that("get_labels() throws an error when argument type mismatches", {
  # Initialize test cases
  type_mismatch <- "Not a data.frame or list" # wrong argument type
  element_mismatch <- list(a = letters[1:6], b = 1:10) # wrong list element types
  unnamed <- list(simple_dummy, data.frame(test = 1:10)) # missing names

  # Perform tests
  purrr::walk(list(type_mismatch, element_mismatch, unnamed), ~ expect_error(get_labels(.x)))
})

test_that("get_labels() returns column labels of a data.frame in a character vector, named according to the column names", { # nolint
  # Prepare a data.frame with labels
  df <- simple_dummy[, 1:3]
  labels <- c("Label 1", "Label 2", "Label 3")
  purrr::walk2(names(df), labels, function(x, y) attributes(df[[x]])$label <<- y)

  # Perform test
  col_labs <- get_labels(df)
  expect_equal(col_labs, labels, ignore_attr = TRUE) # check only values
  expect_named(col_labs, names(df)) # check names
  expect_type(col_labs, "character")
  expect_vector(col_labs)
})

test_that("get_labels() returns labels for each element of a list of data.frames in a character vector, named according to the data.frame names", { # nolint
  # Prepare a list of data.frames with labels
  df_list <- list(dummy1 = simple_dummy[, 1:3], dummy2 = simple_dummy)
  labels <- c("First label", "Second label")
  purrr::walk2(names(df_list), labels, function(x, y) attributes(df_list[[x]])$label <<- y)

  # Perform test
  df_labs <- get_labels(df_list)
  expect_equal(df_labs, labels, ignore_attr = TRUE) # check only values
  expect_named(df_labs, names(df_list)) # check names
  expect_type(df_labs, "character")
  expect_vector(df_labs)
})

test_that("get_labels() returns NULL when the argument is empty or NULL", {
  # Initialize test cases
  empty_df <- data.frame()
  empty_list <- list()
  null_arg <- NULL

  # Perform tests
  purrr::walk(list(empty_df, empty_list, null_arg), ~ expect_null(get_labels(.x)))
})

test_that("get_labels() substitutes missing labels with a 'No label' entry in the returned vector", {
  # We can directly use simple_dummy as it has no column labels
  missing_labs <- get_labels(simple_dummy)
  expected_labs <- rep("No label", times = ncol(simple_dummy))

  # Perform test
  expect_equal(missing_labs, expected_labs, ignore_attr = TRUE)
})



df1 <- data.frame(
  mpg = c(21, 21, 22.8, 21.4),
  cyl = c(6, 6, 4, 6),
  disp = c(160, 160, 108, 258),
  type = c("Mazda RX4", "Mazda RX4 Wag", "Datsun 710", "Hornet 4 Drive")
)
df2 <- data.frame(
  leng = c(5.1, 4.9),
  wid = c(3.5, 3.0),
  spec = c("setosa", "versicolor")
)

attributes(df1)$label <- "Test data 1"
attributes(df1$mpg)$label <- "Col label 1"
attributes(df1$type)$label <- "Col label 2"

test_that("generate_choices() generates meaningful choices for datasets and columns to be used in the corresponding dropdown menues" %>% # nolint
  vdoc[["add_spec"]](
    c(
      specs$listings_label,
      specs$column_label
    )
  ), {
  df_list <- list(first = df1, second = df2)

  expected_df <- c("first [Test data 1]" = "first", "second [No label]" = "second")
  actual_df <- generate_choices(df_list)
  expect_equal(expected_df, actual_df)

  expected_col <- c(
    "mpg [Col label 1]" = "mpg",
    "cyl [No label]" = "cyl",
    "disp [No label]" = "disp",
    "type [Col label 2]" = "type"
  )
  actual_col <- generate_choices(df1)
  expect_equal(expected_col, actual_col)
})


test_that("set_data() throws an error when at leat one argument type mismatches", {
  # Initialize test cases ...
  ## ... for base_data parameter
  not_df <- matrix(
    c(1, 2, 3, 11, 12, 13),
    nrow = 2,
    ncol = 3,
    dimnames = list(c("row1", "row2"), c("C.1", "C.2", "C.3"))
  )
  df <- simple_dummy

  ## ... for selector parameter
  not_char <- c(1, 2)
  char <- c("C.1", "C.2")

  # Perform tests
  expect_error(set_data(not_df, char))
  expect_error(set_data(df, not_char))
  expect_error(set_data(not_df, not_char))
})

test_that("set_data() returns NULL when selector is empty/NULL", {
  expect_null(set_data(simple_dummy, NULL))
})

test_that("set_data() returns a named list containing a data.frame, a character vector, and a character vector", {
  # Initialize test cases
  df <- simple_dummy
  selection <- names(df)[1:2]
  rows <- as.character(seq_len(nrow(df)))
  outcome <- set_data(df, selection)
  expected_names <- c("data", "col_names", "row_names")

  # Perform tests
  expect_type(outcome, "list")
  expect_length(outcome, 3)
  checkmate::expect_data_frame(outcome[[1]]) # first list entry must be a data.frame
  checkmate::expect_character(outcome[[2]]) # second list entry must be a character vector
  checkmate::expect_character(outcome[[3]]) # third list entry must be a character vector
  expect_named(outcome, expected_names)
})

test_that("set_data() returns a character of length zero as row_names if data has no rows", {
  # Initialize test cases
  df <- simple_dummy[0, ]
  selection <- names(df)[1:3]
  outcome <- set_data(df, selection)

  # Perform tests
  testthat::expect_true(length(outcome[["row_names"]]) == 0)
})

test_that("set_data() reduces the columns of base_data to those specified in selector", {
  # Initialize test cases
  df <- simple_dummy
  selection <- names(df)[3:5]
  outcome <- set_data(df, selection)

  # Perform tests
  checkmate::expect_data_frame(outcome[["data"]], ncols = length(selection))
  expect_named(outcome[["data"]], selection)
})

test_that("set_data() orders the columns of the returned data.frame according to the order in selector", {
  # Initialize test cases
  df <- simple_dummy
  selection <- names(df)[5:3]
  outcome <- set_data(df, selection)

  # Perform test
  expect_named(outcome[["data"]], selection)
})

test_that("set_data() creates descriptive column names consisting of the column names of base_data and their labels appended in square brackets", { # nolint
  # Initialize test cases
  df <- simple_dummy[1:3]
  labels <- c("First label", "Second label", "Third label")
  purrr::walk2(names(df), labels, function(x, y) attributes(df[[x]])$label <<- y)
  expected <- c("var1 [First label]", "var2 [Second label]")
  outcome <- set_data(df, selector = names(df)[1:2])

  # Perform test
  expect_equal(outcome[["col_names"]], expected)
})



test_that("set_data() throws an error when argument type mismatches", {
  # Initialize test case
  df <- as.list(simple_dummy)

  # Perform test
  expect_error(convert_data(df))
})

test_that("set_data() returns the data.frame with converted column data types and correct labels", {
  # Initialize test case
  df <- data.frame(A = c("a", "b", "c"), B = c("1", "2", "3"))
  attributes(df$A)$label <- "Character column"
  attributes(df$B)$label <- "Numeric column"

  # Perform test
  conv_df <- convert_data(df)

  expect_equal(attributes(conv_df$A)$label, attributes(df$A)$label)
  expect_equal(attributes(conv_df$B)$label, attributes(df$B)$label)

  expect_equal(class(conv_df$A), "factor")
  expect_equal(class(conv_df$B), "integer")
})



test_that("set_up_datatable() returns correct column names, row names, and paging", {
  df <- data.frame(A = c("a", "b", "c"), B = c("1", "2", "3"), C = c("a", "b", "c"))

  attributes(df$A)$label <- "Label A"
  attributes(df$C)$label <- "Label C"

  selected_cols <- c("A", "B", "C")
  pagination <- NULL

  actual <- set_up_datatable(df, selected_cols, pagination)

  expected <- list(
    col_names = c("A [Label A]", "B [No label]", "C [Label C]"),
    row_names = c("1", "2", "3"),
    paging = FALSE
  )
  expect_equal(actual, expected)
})

test_that("set_up_datatable() automatically activates pagination for large datasets", {
  df <- data.frame(
    A = sample(c("a", "b", "c"), 1001, replace = TRUE),
    B = sample(c("1", "2", "3"), 1001, replace = TRUE),
    C = sample(c("a", "b", "c"), 1001, replace = TRUE)
  )

  selected_cols <- c("A", "B", "C")
  pagination <- NULL

  actual <- set_up_datatable(df, selected_cols, pagination)

  expect_true(actual$paging)
})

test_that("set_up_datatable() automatically deactivates pagination for small datasets", {
  df <- data.frame(
    A = sample(c("a", "b", "c"), 100, replace = TRUE),
    B = sample(c("1", "2", "3"), 100, replace = TRUE),
    C = sample(c("a", "b", "c"), 100, replace = TRUE)
  )

  selected_cols <- c("A", "B", "C")
  pagination <- NULL

  actual <- set_up_datatable(df, selected_cols, pagination)

  expect_false(actual$paging)
})
