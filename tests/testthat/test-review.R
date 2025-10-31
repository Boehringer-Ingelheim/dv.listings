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
  
  info <- REV_load_annotation_info(folder_contents, review, dataset_lists)
  expect_length(info[["error"]], 0)
  fs_client[["execute_IO_plan"]](IO_plan = info[["folder_IO_plan"]], is_init = TRUE)
  fs_client[["read_folder"]](subfolder_candidates = "dataset_list")
  
  test_that("REV_load_annotation_info detects modification of previously known row and generates new delta file" |>
              vdoc[["add_spec"]](specs$review_delta_detection), {
    dataset_lists2 <- dataset_lists
    dataset_lists2[["dataset_list"]][["ae"]][["AESEV"]][[1]] <- "SEVERE"
    
    info <- REV_load_annotation_info(folder_contents, review, dataset_lists2)
    expect_length(info[["error"]], 0)
    expect_length(info[["folder_IO_plan"]], 1)
    folder_op <- info[["folder_IO_plan"]][[1]]
    expect_equal(folder_op[["type"]], "write_file")
    expect_equal(folder_op[["fname"]], "ae_001.delta")
    
    delta <- RS_parse_delta(info[["folder_IO_plan"]][[1]][["contents"]], length(tracked_vars))
    
    expect_equal(delta[["modified_row_indices"]], c(1L))
  })
  
  test_that("Review routines produce a descriptive error when asked to review an empty dataset" |>
              vdoc[["add_spec"]](specs$review_reject_empty_dataset), {
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
})