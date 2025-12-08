local({
  store_path <- file.path(tempdir(), "data_checks")
  dir.create(store_path)
  on.exit(unlink(store_path, recursive = TRUE), add = TRUE, after = FALSE)
  
  fs_client <- fs_init(store_path)
  fs_state <- fs_client[["state"]]
  fs_contents <- fs_state[["contents"]]
  fs_client[["list"]]()
  
  # Review folder contents is initialized here
  tracked_vars = c(
    "AESEV", "AETERM", "AEHLGT", "AEHLT", "AELLT", 
    "AEDECOD", "AESOC", "AESTDTC", "AEENDTC", "AESTDY","AEOUT", "AEACN", "AEREL"
  )
  review = list(
    datasets = list(
      ae = list(
        id_vars = c("USUBJID", "AESEQ"), 
        tracked_vars = tracked_vars
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
  fs_client[["execute_IO_plan"]](IO_plan = info[["IO_plan"]])
  
  test_that("REV_load_annotation_info detects modification of previously known row and generates new delta file" |>
              vdoc[["add_spec"]](specs$review_delta_detection), {
    dataset_lists2 <- dataset_lists
    dataset_lists2[["dataset_list"]][["ae"]][["AESEV"]][[1]] <- "SEVERE"
    
    info <- REV_load_annotation_info(fs_contents, review, dataset_lists2)
    expect_length(info[["error"]], 0)
    expect_length(info[["IO_plan"]], 1)
    folder_op <- info[["IO_plan"]][[1]]
    expect_equal(folder_op[["kind"]], "write")
    expect_equal(folder_op[["path"]], "dataset_list/ae_001.delta")
    expect_equal(folder_op[["offset"]], 0L)
    
    delta <- RS_parse_delta(info[["IO_plan"]][[1]][["contents"]], length(tracked_vars))
    
    expect_equal(delta[["modified_row_indices"]], c(1L))
  })
  
  test_that("Review routines produce a descriptive error when asked to review an empty dataset" |>
              vdoc[["add_spec"]](specs$review_reject_empty_dataset), {
    dataset_lists <- list(
      dataset_list = list(
        ae = head(safetyData::sdtm_ae, 0)
      )
    )
    info <- REV_load_annotation_info(folder_contents = NULL, review, dataset_lists)
    expect_equal(info[["error"]], "Refusing to review 0-row dataset")
  })
  
  test_that("Review routines cope with 1-row datasets", {
    dataset_lists <- list(
      dataset_list = list(
        ae = head(safetyData::sdtm_ae, 1)
      )
    )
    info <- REV_load_annotation_info(folder_contents = NULL, review, dataset_lists)
    expect_length(info[["error"]], 0)
  })
  
  test_that("RS_compute_delta_memory identifies new and modified rows" |>
              vdoc[["add_spec"]](specs$review), {
    old_df <- data.frame(
      ID =        c(1L, 2L),
      TRACKED_1 = c(1L, 2L), 
      TRACKED_2 = c(1L, 2L), 
      TRACKED_3 = c(1L, 2L)
    )
    
    id_vars = c("ID")
    tracked_vars <- c("TRACKED_1", "TRACKED_2", "TRACKED_3")
    
    state <- RS_parse_base(
      RS_compute_base_memory(df_id = "df", df = old_df, id_vars = id_vars, tracked_vars = tracked_vars)
    )
    
    # NOTE: Change to ID 1, ID 2 disappears, ID 3 appears
    new_df <- data.frame(
      ID =        c(1L, 3L),
      TRACKED_1 = c(2L, 3L), 
      TRACKED_2 = c(1L, 3L), 
      TRACKED_3 = c(1L, 3L)
    )
    
    info <- RS_compute_delta_memory(state, new_df)
    expect_length(info[["error"]], 0)
    
    delta <- RS_parse_delta(info[['contents']], length(tracked_vars))
    expect_true(
      delta[["new_row_count"]] == 1 && delta[["modified_row_count"]] == 1 && delta[["modified_row_indices"]] == 1,
    )
  })
  
  test_that("REV_load_annotation_info updates .review files automatically from version 0 of the format to version 1", {
    zero_based_version_byte_pos <- 8L
    
    review_file_path <- file.path('dataset_list', 'ae_roleA.review')
    v1_first_bytes <- fs_contents[[review_file_path]][1L:(zero_based_version_byte_pos+1L)]
    # NOTE: This will break next time this file format changes
    expect_identical(v1_first_bytes[[zero_based_version_byte_pos+1L]], as.raw(1L))
    
    # Revert version marker to 0
    v0_contents <- as.raw(0)
    fs_client[["write"]](
      path = file.path('dataset_list', 'ae_roleA.review'), 
      contents = v0_contents, 
      offset = zero_based_version_byte_pos
    )
    
    info <- REV_load_annotation_info(fs_contents, review, dataset_lists)
    
    # Check that the module wants to upgrade the format to v1
    expect_length(info[["IO_plan"]], 1)
    action <- info[["IO_plan"]][[1]]
    expect_true(
      action[["kind"]] == 'write' && action[["path"]] == 'dataset_list/ae_roleA.review' &&
        action[["offset"]] == 0L && identical(action[["contents"]], v1_first_bytes)
    )
    
    # Check that the IO plan achieves that purpose
    fs_client[["execute_IO_plan"]](info[["IO_plan"]])
    expect_identical(fs_contents[["dataset_list/ae_roleA.review"]][[zero_based_version_byte_pos+1L]], as.raw(1L))
  })
})

local({
  test_that("review feature accepts empty folders" |> vdoc[["add_spec"]](specs$review), {
    error_message <- REV_compute_storage_folder_error_message(character(0), '')
    expect_length(error_message, 0)
  })
  
  test_that("review feature rejects selection of child storage subfolders" |> vdoc[["add_spec"]](c(specs$review, specs$review_reject_storage_subfolders)), {
    error_message <- REV_compute_storage_folder_error_message(c('foo.base'), '')
    expect_length(error_message, 1)
  })
  
  test_that("review feature rejects selection of storage folder initially created by a different Posit Connect app" |> vdoc[["add_spec"]](c(specs$review, specs$review_reject_conflicting_connect_app_storage)), {
    error_message <- REV_compute_storage_folder_error_message(
      c(paste0(REV$ID$APP_ID_prefix, '00000000-0000-0000-0000-000000000000')), '11111111-1111-1111-1111-111111111111'
    )
    expect_length(error_message, 1)
  })
})

