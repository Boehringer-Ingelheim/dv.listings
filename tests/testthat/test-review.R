local({
  folder_contents <- NULL
  fs_callbacks <- list(
    attach = function(arg) NULL, list = function(arg) NULL, read = function(arg) NULL, write = function(arg) NULL,
    append = function(arg) NULL, execute_IO_plan = function(arg) NULL,
    read_folder = function(arg) folder_contents <<- arg
  )
  
  store_path <- file.path(tempdir(), "data_checks")
  dir.create(store_path)
  on.exit(unlink(store_path, recursive = TRUE), add = TRUE, after = FALSE)
  
  fs_client <- fs_init(callbacks = fs_callbacks, store_path)
  fs_client[["read_folder"]](subfolder_candidates = "dataset_list")
  
  # Review folder contents is initialized here
  review = list(
    datasets = list(
      ae = list(
        id_vars = c("USUBJID", "AESEQ"), 
        tracked_vars = c(
          "AESEV", "AETERM", "AEHLGT", "AEHLT", "AELLT", 
          "AEDECOD", "AESOC", "AESTDTC", "AEENDTC", "AESTDY","AEOUT", "AEACN", "AEREL"
        )
      )
    ),
    choices = c("choiceA", "choiceB"),
    roles = c("roleA", "roleB")
  )
  
  dataset_lists <- list(
    dataset_list = list(
      ae = safetyData::sdtm_ae[1:2,]
    )
  )
  
  info <- REV_load_annotation_info(folder_contents, review, dataset_lists)
  expect_length(info[["error"]], 0)
  fs_client[["execute_IO_plan"]](IO_plan = info[["folder_IO_plan"]], is_init = TRUE)
  fs_client[["read_folder"]](subfolder_candidates = "dataset_list")
  
  test_that("Detect modification of previously known row and output new delta file", {
    dataset_lists2 <- dataset_lists
    dataset_lists2[["dataset_list"]][["ae"]][["AESEV"]][[1]] <- "SEVERE"
    
    info <- REV_load_annotation_info(folder_contents, review, dataset_lists2)
    expect_length(info[["error"]], 0)
    expect_length(info[["folder_IO_plan"]], 1)
    folder_op <- info[["folder_IO_plan"]][[1]]
    expect_equal(folder_op[["type"]], "write_file")
    expect_equal(folder_op[["fname"]], "ae_001.delta")
    
    delta <- RS_parse_delta(info[["folder_IO_plan"]][[1]][["contents"]], 13)
    
    expect_equal(delta[["modified_row_indices"]], c(1L))
  })
  
  test_that("Review routines produce a descriptive error when asked to review an empty dataset", {
    folder_contents <- NULL
    dataset_lists <- list(
      dataset_list = list(
        ae = head(safetyData::sdtm_ae, 0)
      )
    )
    info <- REV_load_annotation_info(folder_contents, review, dataset_lists)
    expect_equal(info[["error"]], "Refusing to review 0-row dataset")
  })
  
  test_that("Review routines cope with 1-row datasets", {
    folder_contents <- NULL
    dataset_lists <- list(
      dataset_list = list(
        ae = head(safetyData::sdtm_ae, 1)
      )
    )
    info <- REV_load_annotation_info(folder_contents, review, dataset_lists)
    expect_length(info[["error"]], 0)
  })
})