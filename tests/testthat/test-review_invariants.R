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
    review2[["datasets"]][["ae"]][["tracked_vars"]] <- review2[["datasets"]][["ae"]][["tracked_vars"]][-1]
    info <- REV_load_annotation_info(folder_contents, review2, dataset_lists)
    expect_true(
      length(info[["error"]]) == 1 &&
        startsWith(info[["error"]][[1]], 
                   '[ae] The following variables have not been specified as `tracked_vars`: "AESEV"')
      
    )
  })
  
  test_that("Review error message when `tracked_vars` appear", {
    review2 <- review
    review2[["datasets"]][["ae"]][["tracked_vars"]] <- c(review2[["datasets"]][["ae"]][["tracked_vars"]], "DOMAIN")
    info <- REV_load_annotation_info(folder_contents, review2, dataset_lists)
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
    dataset_lists2[["dataset_list"]][["ae"]][["AESTDY"]] <- 
      as.numeric(dataset_lists2[["dataset_list"]][["ae"]][["AESTDY"]])
    
    info <- REV_load_annotation_info(folder_contents, review, dataset_lists2)
    
    expect_true(
      length(info[["error"]]) == 1 &&
        startsWith(
          info[["error"]][[1]], 
          "[ae] The following variables have changed type (VAR_NAME: BEFORE, AFTER): \nAESTDY: integer, numeric"
        )
    )
  })
  
  test_that("No error message when previously known row is dropped", {
    dataset_lists2 <- dataset_lists
    dataset_lists2[["dataset_list"]][["ae"]] <- head(dataset_lists2[["dataset_list"]][["ae"]], 1)
    
    info <- REV_load_annotation_info(folder_contents, review, dataset_lists2)
    expect_true(length(info[["error"]]) == 0)
  })
})

local({
  # _R_eview _T_est
  RT <- local({
    folder_contents <- NULL
    fs_callbacks <- list(
      attach = function(arg) NULL, list = function(arg) NULL, read = function(arg) NULL, write = function(arg) NULL,
      append = function(arg) NULL, execute_IO_plan = function(arg) NULL,
      read_folder = function(arg) folder_contents <<- arg
    )
    
    store_path <- file.path(tempdir(), "random_data_test")
    unlink(store_path, recursive = TRUE, force = TRUE)
    dir.create(store_path)
    
    fs_client <- fs_init(callbacks = fs_callbacks, store_path)
    fs_client[["read_folder"]](subfolder_candidates = "dataset_list")
    
    # Review folder contents initialized here
    review_param = list(
      datasets = list(df = list(id_vars = c("ID"), tracked_vars = c("TRACKED_1", "TRACKED_2", "TRACKED_3"))),
      choices = c("choiceA", "choiceB"),
      roles = c("roleA", "roleB")
    )
    
    track <- function(df){
      dataset_lists <- list(dataset_list = list(df = df))
     
      # load and update if necessary
      info <- REV_load_annotation_info(folder_contents, review_param, dataset_lists)
      stopifnot(length(info[["error"]]) == 0)
      fs_client[["execute_IO_plan"]](IO_plan = info[["folder_IO_plan"]], is_init = TRUE)
      fs_client[["read_folder"]](subfolder_candidates = "dataset_list")
      
      contents <- folder_contents[["dataset_list"]]
      fnames <- names(contents)
      base_fname <- fnames[endsWith(fnames, '.base')]
      stopifnot(length(base_fname) == 1)
      base_info <- RS_parse_base(contents[[base_fname]][["contents"]])
      total_row_count <- base_info[["row_count"]]
      
      res <- list(
        added = seq_len(total_row_count),
        modified = integer(0),
        missing = integer(0)
      )
      
      delta_fnames <- fnames[endsWith(fnames, '.delta')]
      for(delta_fname in delta_fnames){
        delta_info <- RS_parse_delta(contents[[delta_fname]][["contents"]], 
                                     tracked_var_count = length(base_info[["tracked_vars"]]))
        
        added_count <- delta_info[["new_row_count"]]
        added_indices <- total_row_count + seq_len(added_count)
        total_row_count <- total_row_count + added_count
        
        missing_indices <- setdiff(seq_len(total_row_count), df[["ID"]])
       
        res <- list(
          added = added_indices,
          modified = delta_info[["modified_row_indices"]],
          missing = missing_indices
        )
      }
      
      return(res)
    }
    
    create <- function(base_col_contents = integer(0)){
      res <- data.frame(
        ID = base_col_contents,
        TRACKED_1 = 2L * base_col_contents,
        TRACKED_2 = 3L * base_col_contents,
        TRACKED_3 = 5L * base_col_contents,
        UNTRACKED = 7L * base_col_contents 
      )
      attr(res, 'missing') <- data.frame(
        ID = integer(0), TRACKED_1 = integer(0), TRACKED_2 = integer(0), TRACKED_3 = integer(0), UNTRACKED = integer(0)
      )
      attr(res, 'max_id') <- c(0L, base_col_contents)
      return(res)
    }
    
    shuffle <- function(df){
      order <- sample(nrow(df), nrow(df), replace = FALSE)
      res <- df[order,,drop=FALSE]
      return(res)
    }
    
    append <- function(df, row_count){
      last_row_id <- attr(df, 'max_id')
      col_contents <- last_row_id + seq_len(row_count)
     
      new_rows <- create(base_col_contents = col_contents)
      
      res <- rbind(df, new_rows)
      attr(res, 'max_id') <- last_row_id + as.integer(row_count)
      
      return(res)
    }
    
    remove <- function(df, row_ids){
      mask <- df[["ID"]] %in% row_ids
      res <- df
      attr(res, 'missing') <- rbind(attr(res, 'missing'), res[mask, , drop = FALSE])
      res <- res[!mask, , drop=FALSE]
      return(res)
    }
    
    recover <- function(df, row_ids){
      new_rows <- data.frame(
        ID = row_ids,
        TRACKED_1 = 2L * row_ids,
        TRACKED_2 = 3L * row_ids,
        TRACKED_3 = 5L * row_ids,
        UNTRACKED = 7L * row_ids 
      )
      
      missing <- attr(df, 'missing')
      recover_mask <- missing[["ID"]] %in% row_ids
      res <- rbind(df, missing[recover_mask, , drop = FALSE])
      attr(res, 'missing') <- missing[!recover_mask, , drop = FALSE]
      
      return(res)
    }
    
    mutate <- function(df, row_ids){
      mask <- df[["ID"]] %in% row_ids
      res <- df
      res[["TRACKED_1"]][mask] <- res[["TRACKED_1"]][mask] + 1L
      return(res)
    }
    
    review <- function(df, role, choice){
      browser()
    }
    
    return(list(
      track = track, create = create, append = append, shuffle = shuffle, remove = remove, recover = recover,
      mutate = mutate, review = review, store_path = store_path
    ))
  })
  
  # NOTE: A few manual tests
  df <- RT$create()
  # TODO: Add `info <- RT$track(df)` if we ever allow to review 0-row data frames
  
  df <- RT$append(df, 5)
  info <- RT$track(df)
  expect_identical(info, list(added = 1:5, modified = integer(0), missing = integer(0)))
  
  df <- RT$shuffle(df)
  info <- RT$track(df)
  expect_identical(info, list(added = integer(0), modified = integer(0), missing = integer(0)))
  
  df <- RT$append(df, 2)
  info <- RT$track(df)
  expect_identical(info, list(added = 6:7, modified = integer(0), missing = integer(0)))
  
  df <- RT$remove(df, c(2, 4))
  info <- RT$track(df)
  expect_identical(info, list(added = integer(0), modified = integer(0), missing = c(2L, 4L)))
  
  df <- RT$remove(df, c(3))
  info <- RT$track(df)
  expect_identical(info, list(added = integer(0), modified = integer(0), missing = 2:4))

  df <- RT$mutate(df, 5)
  info <- RT$track(df)
  expect_identical(info, list(added = integer(0), modified = 5L, missing = 2:4))
  
  df <- RT$recover(df, 2)
  info <- RT$track(df)
  expect_identical(info, list(added = integer(0), modified = integer(0), missing = 3:4))
  
  df <- RT$mutate(df, 6)
  info <- RT$track(df)
  expect_identical(info, list(added = integer(0), modified = 6L, missing = 3:4))
  
  unlink(RT$store_path, recursive = TRUE, force = TRUE)
  
  # TODO: 
  # Get seed, run torture test, include seed in failing diagnostic
  
  
})
