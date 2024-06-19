# Load dummy data for testing purpose
source("dummy-data.R")

test_that("export_modal_content() throws an error when argument types mismatch", {
  # arguments
  ns_valid <- function(id) {}
  ns_invalid <- list(NULL, 42, c("wrong", "type"))
  file_name_valid <- "name"
  file_name_invalid <- list(NULL, 42, c("wrong", "type"))
  cond_valid <- "true"
  cond_invalid <- list(NULL, 42, c("wrong", "type"))
  colnames_valid <- c("valid", "column", "names")
  colnames_invalid <- list(42)

  # perform tests
  purrr::walk(ns_invalid, ~ expect_error(export_modal_content(.x, file_name_valid, cond_valid, colnames_valid)))
  purrr::walk(file_name_invalid, ~ expect_error(export_modal_content(ns_valid, .x, cond_valid, colnames_valid)))
  purrr::walk(cond_invalid, ~ expect_error(export_modal_content(ns_valid, file_name_valid, .x, colnames_valid)))
  purrr::walk(colnames_invalid, ~ expect_error(export_modal_content(ns_valid, file_name_valid, cond_valid, .x)))
  expect_error(export_modal_content(ns_valid, file_name_valid, cond_valid, colnames_valid), NA) # expect no error
})

test_that("export_modal_content() returns a shiny tagList with five element", {
  result <- export_modal_content(function(id) {}, "name", "true", c("valid", "column", "names"))

  checkmate::expect_list(result, len = 5)
  checkmate::expect_class(result, "shiny.tag.list")
})

test_that("export_modal_content() shows/hides additional panels depending on condition", {
  # show additional panels
  result <- export_modal_content(function(id) {}, "name", "true", c("column", "names"))
  expect_equal(result[[4]]$attribs$`data-display-if`, "true")

  # hide additional panels
  result <- export_modal_content(function(id) {}, "name", "false", c("column", "names"))
  expect_equal(result[[4]]$attribs$`data-display-if`, "false")
})


test_that("shorten_entries() throws an error when argument types mismatch", {
  # arguments
  vec_valid <- c("first_entry", "second_entry", "third_entry")
  vec_invalid <- list(42)
  len_max_valid <- as.integer(5)
  len_max_invalid <- list(
    8.4, # type_mismatch
    as.integer(2), # too_small
    as.integer(c(1, 2)) # too_long
  )

  # perform tests
  purrr::walk(vec_invalid, ~ expect_error(shorten_entries(.x, len_max_valid)))
  purrr::walk(len_max_invalid, ~ expect_error(shorten_entries(vec_valid, .x)))
  expect_error(shorten_entries(vec_valid, len_max_valid), NA) # expect no error
})

test_that("shorten_entries() returns the original vector if strings are already short enough", {
  # arguments
  vec <- c("first_entry", "second_entry", "third_entry")
  len_max <- as.integer(42)

  # perform test
  expect_identical(shorten_entries(vec, len_max), vec)
})

test_that("shorten_entries() cuts strings to not exceed a specific length", {
  # arguments
  vec <- c("first_entry", "second_entry", "third_entry")
  len_max <- as.integer(5)

  # perform test
  expect_identical(nchar(shorten_entries(vec, len_max)), rep(len_max, length(vec)))
})

test_that("shorten_entries() returns the correct string after cutting", {
  # arguments
  vec <- c("first_entry", "second_entry", "third_entry")
  len_max <- as.integer(5)

  # perform test
  expect_identical(shorten_entries(vec, len_max), c("fi...", "se...", "th..."))
})


test_that("split_label() throws an error when argument types mismatch", {
  # arguments
  label_valid <- "This is a label."
  label_invalid <- list(42, c("multiple labels", "in one", "vector"), NULL)
  min_width_valid <- as.integer(5)
  min_width_invalid <- list(as.integer(0), 5.8, c(as.integer(4), as.integer(7)), "Wrong type")
  max_width_valid <- as.integer(50)
  max_width_invalid <- list(as.integer(3), 10.5, c(as.integer(10), as.integer(12)), "Wrong type")
  label_width_valid <- as.integer(6)
  label_width_invalid <- list(as.integer(0), 5.8, c(as.integer(4), as.integer(7)), "Wrong type")

  # perform tests
  purrr::walk(label_invalid, ~ expect_error(split_label(.x, min_width_valid, max_width_valid, label_width_valid)))
  purrr::walk(min_width_invalid, ~ expect_error(split_label(label_valid, .x, max_width_valid, label_width_valid)))
  purrr::walk(max_width_invalid, ~ expect_error(split_label(label_valid, min_width_valid, .x, label_width_valid)))
  purrr::walk(label_width_invalid, ~ expect_error(split_label(label_valid, min_width_valid, max_width_valid, .x)))
  expect_error(split_label(label_valid, min_width_valid, max_width_valid, label_width_valid), NA) # expect no error
})

test_that("split_label() splits a simple label correctly", {
  # arguments
  label <- "This is a simple label."
  min_width <- as.integer(5)
  max_width <- as.integer(10)
  label_width <- as.integer(5)

  # expected
  label_vec <- c("This", "is a", "simple", "label.")

  # perform test
  expect_identical(
    split_label(label, min_width, max_width, label_width),
    list(label_vec = label_vec, col_width = max(nchar(label_vec)))
  )
})

test_that("split_label() deals with labels that do not fit in the foreseen lines", {
  # arguments
  label <- "This is a looooong label which does not fit in the foreseen lines."
  min_width <- as.integer(1)
  max_width <- as.integer(6)
  label_width <- as.integer(6)

  # expected
  label_vec <- c("This", "is a", "loo...", "label", "which", "doe...")

  # perform test
  expect_identical(
    split_label(label, min_width, max_width, label_width),
    list(label_vec = label_vec, col_width = max(nchar(label_vec)))
  )
})

test_that("split_label() directly returns the unchanged label and min_width in case one entry is longer than the label", { # nolint
  # arguments
  label <- "Short label"
  min_width <- as.integer(50)
  max_width <- as.integer(100)
  label_width <- as.integer(6)

  # expected
  result <- list(label_vec = label, col_width = min_width)

  # perform test
  expect_identical(split_label(label, min_width, max_width, label_width), result)
})


test_that("calculate_col_width() throws an error when argument types mismatch", {
  # arguments
  df_valid <- dm_dummy

  df_unnamed <- data.frame(test = cbind(1:10, 11:20))
  colnames(df_unnamed) <- NULL
  df_duplicated_names <- data.frame(test = cbind(1:10, 11:20))
  colnames(df_duplicated_names) <- c("name1", "name1")

  df_invalid <- list(
    "Not a data frame.", # type_mismatch
    df_unnamed, # unnamed
    df_duplicated_names, # duplicated names
    data.frame() # no_dimensions
  )

  ref_valid <- paste0(names(df_valid)[1], " [", get_labels(df_valid[1]), "]")
  ref_invalid <- list(
    cbind(1:5, 6:10), # type_mismatch
    c("wrong", "entries") # wrong_entries
  )

  # perform tests
  purrr::walk(df_invalid, ~ expect_error(calculate_col_width(.x, ref_valid)))
  purrr::walk(ref_invalid, ~ expect_error(calculate_col_width(df_valid, .x)))
  expect_error(calculate_col_width(df_valid, ref_valid), NA) # expect no error
})

test_that("calculate_col_width() returns the correct results", {
  # df argument
  df <- data.frame(cbind(
    c("entry", "entry"),
    c("entry", "entry"),
    c("entry", "entry 14 chars")
  ))
  colnames(df) <- c("name_13_chars", "name2", "name3")
  attributes(df[["name_13_chars"]])$label <- "label 1"
  attributes(df[["name2"]])$label <- "label 10 characters"
  attributes(df[["name3"]])$label <- "label 3"

  # ref argument
  ref_ind <- c(1, 2)
  ref <- paste0(names(df)[ref_ind], " [", get_labels(df)[ref_ind], "]")

  # result
  col_width_res <- calculate_col_width(df, ref)

  # expected
  table_width <- PDF_EXP$N_COL_CHARS - nchar(nrow(df))
  label_vecs <- purrr::map2(get_labels(df), c(13, 5, 14), ~ {
    split_label(.x, as.integer(.y), as.integer(table_width), as.integer(PDF_EXP$LABEL_WIDTH))$label_vec
  })
  col_width <- purrr::map2(get_labels(df), c(13, 5, 14), ~ {
    split_label(.x, as.integer(.y), as.integer(table_width), as.integer(PDF_EXP$LABEL_WIDTH))$col_width
  })
  width_max <- unlist(col_width)
  ref_width <- sum(width_max[ref_ind]) + PDF_EXP$COL_TRANSITION * (length(ref_ind) - 1)
  col_width_exp <- list(
    ref = names(df)[ref_ind],
    label_vecs = label_vecs,
    width_max = width_max,
    ref_width = ref_width,
    width = unlist(col_width[!(paste0(names(df), " [", get_labels(df), "]") %in% ref)]),
    table_width = table_width,
    check_ref_cols = FALSE
  )

  # perform test
  expect_identical(col_width_res, col_width_exp)
})

test_that("calculate_col_width() detects that all columns are specified as reference columns", {
  # arguments
  df <- data.frame(cbind(
    c("entry", "entry"),
    c("entry", "entry")
  ))
  colnames(df) <- c("name1", "name2")
  ref <- paste0(colnames(df), " [", get_labels(df), "]")

  # perform test
  expect_identical(calculate_col_width(df, ref)$check_ref_cols, TRUE)
})

test_that("calculate_col_width() detects that reference columns take up too much space", {
  # arguments
  df <- dm_dummy
  ref <- paste0(colnames(df)[1:15], " [", get_labels(df)[1:15], "]")

  # perform test
  expect_identical(calculate_col_width(df, ref)$check_ref_cols, TRUE)
})



empty_rownames <- purrr::imap(rep("", PDF_EXP$LABEL_WIDTH), ~ paste(rep(.x, .y), collapse = " "))

test_that("pdf_preprocessing() throws an error when argument types mismatch", {
  # arguments
  df_valid <- dm_dummy
  unnamed <- data.frame(test = cbind(1:10, 11:20))
  colnames(unnamed) <- NULL
  duplicated_names <- data.frame(test = cbind(1:10, 11:20))
  colnames(duplicated_names) <- c("name1", "name1")
  df_invalid <- list(
    "Not a data frame.", # type_mismatch
    unnamed, # unnamed
    duplicated_names, # duplicated names
    data.frame() # no dimensions
  )

  ref_valid <- paste0(names(df_valid)[1], " [", get_labels(df_valid[1]), "]")
  ref_invalid <- list(
    cbind(1:5, 6:10), # type_mismatch
    c("wrong", "entries") # wrong_entries
  )

  # perform tests
  purrr::walk(df_invalid, ~ expect_error(pdf_preprocessing(.x, ref_valid)))
  purrr::walk(ref_invalid, ~ expect_error(pdf_preprocessing(df_valid, .x)))
  expect_error(pdf_preprocessing(df_valid, ref_valid), NA) # expect no error
})

test_that("pdf_preprocessing() returns original df (without splitting) inclusively labels and its column widths", {
  # df argument
  df <- data.frame(cbind(
    c("entry", "entry"),
    c("entry", "entry")
  ))
  colnames(df) <- c("name1", "name2")
  labels <- c("label 1", "label 2")
  rownames <- row.names(df)
  df_res <- rbind(labels, df)
  row.names(df_res) <- c("", rownames)
  attributes(df[["name1"]])$label <- labels[1]
  attributes(df[["name2"]])$label <- labels[2]

  # result
  pdf_preprocessing_res <- pdf_preprocessing(df, NULL)

  # expected
  pdf_preprocessing_exp <- list(data.frame(rbind(
    c("label", "label"),
    c("1", "2"),
    matrix("", nrow = PDF_EXP$LABEL_WIDTH - 2, ncol = 2),
    c("entry", "entry"),
    c("entry", "entry")
  )))
  colnames(pdf_preprocessing_exp[[1]]) <- c("name1", "name2")
  rownames(pdf_preprocessing_exp[[1]]) <- c(empty_rownames, 1, 2)

  # perform test
  expect_identical(pdf_preprocessing_res, pdf_preprocessing_exp)
})

test_that("pdf_preprocessing() deals with data frames containing only one column", {
  # df argument
  df <- data.frame(simple_dummy[1:5, 1])
  colnames(df) <- c("name1")

  # result
  pdf_preprocessing_res <- pdf_preprocessing(df, NULL)

  # expected
  pdf_preprocessing_exp <- data.frame(c("No", "label", "", "", "", "", simple_dummy[1:5, 1]))
  colnames(pdf_preprocessing_exp) <- colnames(df)
  rownames(pdf_preprocessing_exp) <- c(empty_rownames, 1:5)
  pdf_preprocessing_exp <- list(pdf_preprocessing_exp)

  # perform test
  expect_identical(pdf_preprocessing_res, pdf_preprocessing_exp)
})

test_that("pdf_preprocessing() deals with data frames containing only one row", {
  # df argument
  df <- data.frame(simple_dummy[1, 1:3])
  colnames(df) <- c("name1", "name2", "name3")

  # result
  pdf_preprocessing_res <- pdf_preprocessing(df, "name2 [No label]")

  # expected
  pdf_preprocessing_exp <- data.frame(rbind(
    c("No", "No", "No"),
    c("label", "label", "label"),
    matrix("", nrow = PDF_EXP$LABEL_WIDTH - 2, ncol = 3),
    as.matrix(simple_dummy[1, 1:3])
  ))
  colnames(pdf_preprocessing_exp) <- colnames(df)
  rownames(pdf_preprocessing_exp) <- c(empty_rownames, seq_len(nrow(df)))
  pdf_preprocessing_exp <- pdf_preprocessing_exp[, c(2, 1, 3)]
  pdf_preprocessing_exp <- list(pdf_preprocessing_exp)

  # perform test
  expect_identical(pdf_preprocessing_res, pdf_preprocessing_exp)
})

test_that("pdf_preprocessing() changes column order due to reference column specification" %>%
  vdoc[["add_spec"]](specs$export_pdf), {
  # df argument
  len <- 4
  df <- simple_dummy[1:2, 1:len]

  # ref argument
  ref_ind <- c(4, 2)
  ref <- paste0(names(df)[ref_ind], " [", get_labels(df)[ref_ind], "]")

  # result
  pdf_preprocessing_res <- pdf_preprocessing(df, ref)

  # expected
  df_res <- data.frame(rbind(
    rep("No", len),
    rep("label", len),
    matrix("", nrow = PDF_EXP$LABEL_WIDTH - 2, ncol = len),
    as.matrix(df)
  ))
  rownames(df_res) <- c(empty_rownames, 1, 2)
  pdf_preprocessing_exp <- list(cbind(df_res[, ref_ind], df_res[, !(1:len %in% ref_ind)]))


  # perform test
  expect_identical(pdf_preprocessing_res, pdf_preprocessing_exp)
})

test_that("pdf_preprocessing() splits df into disjoint sub dataframes that form together the original df when ignoring labels", { # nolint
  # df argument
  df <- dm_dummy[1:35, 1:10]

  # result
  pdf_preprocessing_res <- pdf_preprocessing(df, NULL)
  pdf_preprocessing_res <- purrr::map(pdf_preprocessing_res, ~ {
    return(.x[7:dim(.x)[1], ])
  })
  pdf_preprocessing_res <- rbind(
    cbind(pdf_preprocessing_res[[1]], pdf_preprocessing_res[[2]]),
    cbind(pdf_preprocessing_res[[3]], pdf_preprocessing_res[[4]])
  )

  # perform test
  expect_identical(apply(pdf_preprocessing_res, 2, as.character), apply(df, 2, as.character))
})

test_that("pdf_preprocessing() shortens entries that do not fit on one page", {
  # df argument
  df <- data.frame(cbind(
    c("entry", "entry", "entry"),
    c("entry", "entry", "entry")
  ))
  colnames(df) <- c("name1", "name2")
  attributes(df[["name1"]])$label <- "label 1"
  attributes(df[["name2"]])$label <- "label 2"
  long_entry <- paste0(c("this is a", rep("very", 25), "long entry"), collapse = " ")
  df[2, 1] <- long_entry

  # result
  pdf_preprocessing_res <- pdf_preprocessing(df, NULL)[[1]][8, 1]

  # expected
  pdf_preprocessing_exp <- paste0(substr(long_entry, 1, (PDF_EXP$N_COL_CHARS - nchar(nrow(df)) - 3)), "...")

  # perform test
  expect_identical(pdf_preprocessing_res, pdf_preprocessing_exp)
})



# function to set "No label" labels
set_labels <- function(df) {
  as.data.frame(mapply(function(col, label) {
    attr(col, "label") <- label
    return(col)
  }, df, rep("No label", ncol(df)), SIMPLIFY = FALSE))
}

test_that("prep_export_data() throws an error when argument types mismatch", {
  # arguments
  data_selection_valid <- "single"
  data_selection_invalid <- list(NULL, 42, c("wrong", "type"), "wrong string")
  current_data_valid <- simple_dummy
  current_data_invalid <- list("no dataframe", NULL)
  data_selection_name_valid <- "dummy_data"
  data_selection_name_invalid <- list(42, c("wrong", "type"))
  dataset_list_valid <- list("dummy_data" = simple_dummy)
  dataset_list_invalid <- list(c("wrong", "type"), list(1, 2, "no dataframe"))

  # perform tests
  purrr::walk(data_selection_invalid, ~ expect_error(prep_export_data(
    .x,
    current_data_valid,
    data_selection_name_valid,
    dataset_list_valid
  )))
  purrr::walk(current_data_invalid, ~ expect_error(prep_export_data(
    data_selection_valid,
    .x,
    data_selection_name_valid,
    dataset_list_valid
  )))
  purrr::walk(data_selection_name_invalid, ~ expect_error(prep_export_data(
    data_selection_valid,
    current_data_valid,
    .x,
    dataset_list_valid
  )))
  purrr::walk(dataset_list_invalid, ~ expect_error(prep_export_data(
    data_selection_valid,
    current_data_valid,
    data_selection_name_valid,
    .x
  )))
  expect_error(prep_export_data(
    data_selection_valid,
    current_data_valid,
    data_selection_name_valid,
    dataset_list_valid
  ), NA) # expect no error
})

test_that("prep_export_data() performs the correct transformation in the single dataset case", {
  # arguments
  data_selection_valid <- "single"
  dataset_list_valid <- list(data1 = data.frame(col1 = c(1, 2), col2 = c(3, 4)))
  attributes(dataset_list_valid$data1)$label <- "My Label"
  current_data_valid <- dataset_list_valid[[1]]
  data_selection_name_valid <- names(dataset_list_valid)[1]

  # result
  res <- prep_export_data(data_selection_valid, current_data_valid, data_selection_name_valid, dataset_list_valid)

  # expected
  exp <- list("data1 (My Label)" = set_labels(data.frame(col1 = c("1", "2"), col2 = c("3", "4"))))

  # perform tests
  expect_identical(res, exp)
})

test_that("prep_export_data() performs the correct transformation in the multiple dataset case" %>%
  vdoc[["add_spec"]](specs$export_excel), {
  # arguments
  data_selection_valid <- "all"
  dataset_list_valid <- list(
    "dummy1" = simple_dummy,
    "dummy2" = simple_dummy[1:5],
    "dummy3" = simple_dummy[5:10]
  )
  attributes(dataset_list_valid$dummy1)$label <- "My Label 1"
  attributes(dataset_list_valid$dummy2)$label <- "My Label 2"
  attributes(dataset_list_valid$dummy3)$label <- "My Label 3"
  current_data_valid <- dataset_list_valid[[1]]
  data_selection_name_valid <- names(dataset_list_valid)[1]

  # result
  res <- prep_export_data(
    data_selection_valid,
    current_data_valid,
    data_selection_name_valid,
    dataset_list_valid
  )

  # perform tests
  expect_identical(names(res), c("dummy1 (My Label 1)", "dummy2 (My Label 2)", "dummy3 (My Label 3)"))
  expect_identical(res[[1]], set_labels(data.frame(sapply(dataset_list_valid[[1]], as.character))))
  expect_identical(res[[2]], set_labels(data.frame(sapply(dataset_list_valid[[2]], as.character))))
  expect_identical(res[[3]], set_labels(data.frame(sapply(dataset_list_valid[[3]], as.character))))
})

test_that("prep_export_data() shortens dataset names if they exceed Excel's sheet name limit of 31 characters", {
  # arguments
  data_selection_valid <- "all"
  dataset_list_valid <- list("dummy1" = simple_dummy, "dummy2" = simple_dummy[1:5], "dummy3" = simple_dummy[5:10])
  attributes(dataset_list_valid$dummy1)$label <- "This is a very long dataset label"
  attributes(dataset_list_valid$dummy2)$label <- "This is another very long dataset label"
  attributes(dataset_list_valid$dummy3)$label <- "Short label"
  current_data_valid <- dataset_list_valid[[1]]
  data_selection_name_valid <- names(dataset_list_valid)[1]

  # result
  res <- nchar(
    names(prep_export_data(data_selection_valid, current_data_valid, data_selection_name_valid, dataset_list_valid))
  )

  # perform tests
  expect_identical(res, as.integer(c(31, 31, 20)))
})


test_that("excel_export() throws an error when argument types mismatch", {
  # arguments
  data_to_download_valid <- list("dummy1" = simple_dummy, "dummy2" = simple_dummy[1:5])
  data_to_download_invalid <- list(c("wrong", "type"), list(1, 2, "no dataframe"), NULL)
  file_valid <- "./testfile.xlsx"
  file_invalid <- list(42, "./testfile.pdf")

  # perform tests
  purrr::walk(data_to_download_invalid, ~ expect_error(excel_export(.x, file_valid)))
  purrr::walk(file_invalid, ~ expect_error(excel_export(data_to_download_valid, .x)))
})

test_that("excel_export() exports the .xlsx file as intended" %>%
  vdoc[["add_spec"]](specs$export_excel), {
  # arguments
  data_to_download <- list("dummy1" = simple_dummy, "dummy2" = simple_dummy[2:7])
  file <- paste0(getwd(), "/testfile.xlsx")

  # result
  excel_export(data_to_download, file, intended_use_label = "")
  res_sheet2 <- openxlsx::read.xlsx(file, "dummy2", sep.names = " ")

  # expected
  exp_sheet2 <- data_to_download$dummy2
  rownames(exp_sheet2) <- seq_len(nrow(exp_sheet2))
  colnames(exp_sheet2) <- paste0(colnames(exp_sheet2), " [", get_labels(exp_sheet2), "]")

  # perform tests
  expect_equal(res_sheet2, exp_sheet2)

  # remove file
  file.remove(file)
})

test_that("excel_export() generates the .xlsx file with a leading worksheet containing the disclaimer", {
  # arguments
  data_to_download <- list("dummy1" = simple_dummy, "dummy2" = simple_dummy[2:7])
  file <- paste0(getwd(), "/testfile.xlsx")

  # result
  excel_export(data_to_download, file, intended_use_label = "test label")
  res_info_sheet <- openxlsx::read.xlsx(file, 1, sep.names = " ", colNames = FALSE) # 1 = leading # nolint
  colnames(res_info_sheet) <- NULL

  # expected
  exp_info_sheet <- data.frame(c(EXP$EXP_TITLE, "test label"))
  colnames(exp_info_sheet) <- NULL

  # perform tests
  expect_equal(res_info_sheet, exp_info_sheet)

  # remove file
  file.remove(file)
})



test_that("pdf_export() throws an error when argument types mismatch", {
  # arguments
  data_to_download_valid <- list("dummy_data" = simple_dummy)
  data_to_download_invalid <- list(
    c("wrong", "type"), # wrong type
    list(1, 2, "no dataframe"), # wrong element type
    NULL, # NULL
    list("dummy_data1" = simple_dummy, "dummy_data2" = simple_dummy) # more than one list entry
  )
  ref_valid <- c("var2 [No label]")
  ref_invalid <- list(42)
  file_valid <- "./testfile.pdf"
  file_invalid <- list(42, "./testfile.xlsx")
  metadata_valid <- c("text 1", "text 2", "text 3")
  metadata_invalid <- list(42, c("too", "many", "header/footer", "components"))

  # perform tests
  purrr::walk(data_to_download_invalid, ~ expect_error(pdf_export(
    .x, ref_valid, file_valid, metadata_valid, FALSE
  )))
  purrr::walk(ref_invalid, ~ expect_error(pdf_export(
    data_to_download_valid, .x, file_valid, metadata_valid, FALSE
  )))
  purrr::walk(file_invalid, ~ expect_error(pdf_export(
    data_to_download_valid, ref_valid, .x, metadata_valid, FALSE
  )))
  purrr::walk(metadata_invalid, ~ expect_error(pdf_export(
    data_to_download_valid, ref_valid, file_valid, .x, FALSE
  )))
})

test_that("pdf_export() exports the .pdf file as intended" %>%
  vdoc[["add_spec"]](specs$export_pdf), {
  # arguments
  data_to_download <- list("dummy_data" = simple_dummy)
  ref <- c("var2 [No label]")
  file <- paste0(getwd(), "/testfile.pdf")
  metadata <- c("text 1", "text 2", "text 3")

  # result
  pdf_export(data_to_download, ref, file, metadata, FALSE, "")

  # perform tests
  expect_true(file.exists(file))

  # remove file
  file.remove(file)
})

test_that("pdf_export() generates the .pdf file with a title page containing the disclaimer", {
  skip("Setting titles and subtitles is Rmarkdown functionality and therefore not tested additionally.")
})



test_that("warn_function() throws an error when argument types mismatch", {
  # arguments
  cond_valid <- TRUE
  cond_invalid <- list(NULL, c("wrong", "type"))
  input_id_valid <- "id"
  input_id_invalid <- list(NULL, 42)
  text_valid <- "warning"
  text_invalid <- list(c("wrong", "type"))

  # perform tests
  purrr::walk(cond_invalid, ~ expect_error(warn_function(.x, input_id_valid, text_valid)))
  purrr::walk(input_id_invalid, ~ expect_error(warn_function(cond_valid, .x, text_valid)))
  purrr::walk(text_invalid, ~ expect_error(warn_function(cond_valid, input_id_valid, .x)))
})

test_that("warn_function() triggers the correct feedback", {
  skip("Highlighting problematic user input is not tested in an automated way.")
})
