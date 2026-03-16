local({
  store_path <- file.path(tempdir(), "data_checks")
  dir.create(store_path)
  on.exit(unlink(store_path, recursive = TRUE), add = TRUE, after = FALSE)
  
  fs_client <- fs_init(store_path)
  fs_state <- fs_client[["state"]]
  fs_contents <- fs_state[["contents"]]
  fs_client[["list"]]()
  
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
  
  info <- REV_load_annotation_info(fs_contents, review, dataset_lists)
  expect_length(info[["error"]], 0)
  fs_client[["execute_IO_plan"]](info[["IO_plan"]])
  expect_length(fs_state[["error"]], 0L)
  
  test_that("Review error message when extra choice is provided", {
    review2 <- review
    review2[["choices"]] <- c(review2[["choices"]], "choiceC")
    info <- REV_load_annotation_info(fs_contents, review2, dataset_lists)
    expect_true(length(info[["error"]]) == 1 &&
                  startsWith(info[["error"]][[1]], "Review choices should remain stable during the course of a trial."))
  })
    
  test_that("Review error message when `id_vars` is modified", {
    review2 <- review
    review2[["datasets"]][["ae"]][["id_vars"]] <- c("USUBJID", "AESEQ", "STUDYID")
    info <- REV_load_annotation_info(fs_contents, review2, dataset_lists)
    expect_true(length(info[["error"]]) == 1 &&
                  startsWith(info[["error"]][[1]], "[ae] `id_vars` should remain stable during the course of a trial."))
  })
    
  test_that("Review error message when `tracked_vars` disappear", {
    review2 <- review
    review2[["datasets"]][["ae"]][["tracked_vars"]] <- review2[["datasets"]][["ae"]][["tracked_vars"]][-1]
    info <- REV_load_annotation_info(fs_contents, review2, dataset_lists)
    expect_true(
      length(info[["error"]]) == 1 &&
        startsWith(info[["error"]][[1]], 
                   '[ae] The following variables have not been specified as `tracked_vars`: "AESEV"')
    )
  })
  
  test_that("Review error message when `tracked_vars` appear", {
    review2 <- review
    review2[["datasets"]][["ae"]][["tracked_vars"]] <- c(review2[["datasets"]][["ae"]][["tracked_vars"]], "DOMAIN")
    info <- REV_load_annotation_info(fs_contents, review2, dataset_lists)
    expect_true(
      length(info[["error"]]) == 1 &&
        startsWith(
          info[["error"]][[1]], 
          '[ae] The following variables were not available on a previous iteration of the review process: "DOMAIN".'
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
    
    info <- REV_load_annotation_info(fs_contents, review, dataset_lists2)
    
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
    dataset_lists2[["dataset_list"]][["ae"]][["AESTDY"]] <- 
      as.numeric(dataset_lists2[["dataset_list"]][["ae"]][["AESTDY"]])
    
    info <- REV_load_annotation_info(fs_contents, review, dataset_lists2)
    
    expect_true(
      length(info[["error"]]) == 1 &&
        startsWith(
          info[["error"]][[1]], 
          "[ae] The following variables have changed type (VAR_NAME: BEFORE, AFTER): \nAESTDY: integer, numeric"
        )
    )
  })
  
  test_that("There is an error message when previously known rows are dropped" |>
              vdoc[["add_spec"]](specs$review_reject_removal_of_rows_by_default), {
    dataset_lists2 <- dataset_lists
    dataset_lists2[["dataset_list"]][["ae"]] <- head(dataset_lists2[["dataset_list"]][["ae"]], 1)
    
    info <- REV_load_annotation_info(fs_contents, review, dataset_lists2)
    expect_true(any(grepl("allow_row_deletion", info[["error"]])))
  })
  
  test_that("The missing data error message can be bypassed by configuring the `allow_row_deletion` flag" |>
              vdoc[["add_spec"]](specs$review_accept_removal_of_rows_when_instructed_so), {
    dataset_lists2 <- dataset_lists
    dataset_lists2[["dataset_list"]][["ae"]] <- head(dataset_lists2[["dataset_list"]][["ae"]], 1)
   
    review_config_that_allows_row_deletion <- review
    review_config_that_allows_row_deletion[["allow_row_deletion"]] <- TRUE
    
    info <- REV_load_annotation_info(fs_contents, review_config_that_allows_row_deletion, dataset_lists2)
    expect_true(length(info[["error"]]) == 0)
  })
})
