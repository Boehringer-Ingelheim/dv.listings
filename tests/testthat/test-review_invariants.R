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
    datasets = list(ae = list(id_vars = c("USUBJID", "AESEQ"), untracked_vars = c())),
    choices = c("choiceA", "choiceB"),
    roles = c("roleA", "roleB")
  )
  
  dataset_lists <- list(
    dataset_list = list(
      ae = safetyData::adam_adae[1:2,]
    )
  )
  
  info <- REV_load_annotation_info(folder_contents, review, dataset_lists)
  expect_length(info[["error"]], 0)
  fs_client[["execute_IO_plan"]](IO_plan = info[["folder_IO_plan"]], is_init = TRUE)
  fs_client[["read_folder"]](subfolder_candidates = "dataset_list")
  
  test_that("Review error message when extra choice is provided", {
    review2 <- review
    review2[["choices"]] <- c(review2[["choices"]], "choiceC")
    info <- REV_load_annotation_info(folder_contents, review2, dataset_lists)
    expect_true(length(info[["error"]]) == 1 &&
                  startsWith(info[["error"]][[1]], "Review choices should remain stable during the course of a trial."))
  })
    
  test_that("Review error message when `id_vars` is modified", {
    review2 <- review
    review2[["datasets"]][["ae"]][["id_vars"]] <- c("USUBJID", "AESEQ", "STUDYID")
    info <- REV_load_annotation_info(folder_contents, review2, dataset_lists)
    expect_true(length(info[["error"]]) == 1 &&
                  startsWith(info[["error"]][[1]], "[ae] `id_vars` should remain stable during the course of a trial."))
  })
    
  test_that("Review error message when `tracked_vars` disappear", {
    review2 <- review
    review2[["datasets"]][["ae"]][["untracked_vars"]] <- c("STUDYID")
    info <- REV_load_annotation_info(folder_contents, review2, dataset_lists)
    expect_true(
      length(info[["error"]]) == 1 &&
        startsWith(info[["error"]][[1]], 
                   '[ae] The following variables are not available or have been specified as "untracked": "STUDYID"')
    )
  })
    
  test_that("Review error message when `tracked_vars` appear", {
    dataset_lists2 <- dataset_lists
    dataset_lists2[["dataset_list"]][["ae"]][["EXTRA_VAR"]] <- ''
    info <- REV_load_annotation_info(folder_contents, review, dataset_lists2)
    expect_true(
      length(info[["error"]]) == 1 &&
        startsWith(
          info[["error"]][[1]], 
          '[ae] The following variables were not available on a previous iteration of the review process: "EXTRA_VAR".'
        )
    )
  })
    
  test_that("Review error message when an `id_var` changes type", {
    review2 <- review
    # NOTE: This is not an error that can happen when the module is run under dv.manager because of the automatic
    #       character-to-factor- mapping, but it's still a simple way of testing that this error is caught
    dataset_lists2 <- dataset_lists
    dataset_lists2[["dataset_list"]][["ae"]][["USUBJID"]] <- 
      as.factor(dataset_lists2[["dataset_list"]][["ae"]][["USUBJID"]])
    
    info <- REV_load_annotation_info(folder_contents, review, dataset_lists2)
    
    expect_true(
      length(info[["error"]]) == 1 &&
        startsWith(
          info[["error"]][[1]], 
          "[ae] The following variables have changed type (VAR_NAME: BEFORE, AFTER): \nUSUBJID: character, factor"
        )
    )
  })
    
  test_that("Review error message when a `tracked_var` changes type", {
    review2 <- review
    dataset_lists2 <- dataset_lists
    dataset_lists2[["dataset_list"]][["ae"]][["TRTEDT"]] <- 
      as.numeric(dataset_lists2[["dataset_list"]][["ae"]][["TRTEDT"]])
    
    info <- REV_load_annotation_info(folder_contents, review, dataset_lists2)
    
    expect_true(
      length(info[["error"]]) == 1 &&
        startsWith(
          info[["error"]][[1]], 
          "[ae] The following variables have changed type (VAR_NAME: BEFORE, AFTER): \nTRTEDT: Date, numeric"
        )
    )
  })
  
  test_that("Review error message when previously known row is dropped", {
    dataset_lists2 <- dataset_lists
    dataset_lists2[["dataset_list"]][["ae"]] <- head(dataset_lists2[["dataset_list"]][["ae"]], 1)
    
    info <- REV_load_annotation_info(folder_contents, review, dataset_lists2)
    expect_true(length(info[["error"]]) == 1 &&
                  startsWith(info[["error"]][[1]], "[ae] Dataset update is missing 1 previously known row(s)"))
  })
})