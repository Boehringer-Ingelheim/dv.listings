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
  
  test_that("No error message when previously known row is dropped" |>
              vdoc[["add_spec"]](specs$review_accept_removal_of_rows), {
    dataset_lists2 <- dataset_lists
    dataset_lists2[["dataset_list"]][["ae"]] <- head(dataset_lists2[["dataset_list"]][["ae"]], 1)
    
    info <- REV_load_annotation_info(folder_contents, review, dataset_lists2)
    expect_true(length(info[["error"]]) == 0)
  })
})

rng_seed <- local({
  runif(1)
  return(.Random.seed)
})

int_seed <- as.integer(Sys.time()) # Force random seed to get fresh tests
set.seed(int_seed)

test_that(sprintf("Running random review tests with seed: %dL", int_seed) |>
            vdoc[["add_spec"]](c(
              specs$review, specs$review_per_row, specs$review_per_role,
              specs$review_delta_detection, specs$review_accept_removal_of_rows)
            ), {
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
      
      fs_client[["read_folder"]](subfolder_candidates = "dataset_list") # Refreshes potentially new review info
     
      # TODO: This initial `_load_` is here to load reviews through a side channel. It would be preferable to 
      #       load them explicitly through a separate function (both inside the module and in this oracle-like test)
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
          modified = sort(delta_info[["modified_row_indices"]]),
          missing = missing_indices
        )
      }
            
      latest_reviews <- character(0) 
      loaded_latest_reviews <- info[["loaded_annotation_info"]][["dataset_list"]][["df"]][["latest_reviews"]]
      for(i_row_review in seq_along(loaded_latest_reviews)){
        row_reviews <- loaded_latest_reviews[[i_row_review]][["reviews"]]
        latest_review <- review_param[["choices"]][[1]]
        latest_review_timestamp <- 0
        for(role in review_param[["roles"]]){
          role_row_review <- row_reviews[[role]]
          if(!is.null(role_row_review) && latest_review_timestamp < role_row_review[["timestamp"]]){
            latest_review <- role_row_review[["review"]]
            latest_review_timestamp <- role_row_review[["timestamp"]]
          }
        }
        latest_reviews <- c(latest_reviews, latest_review)
      }
      
      res[["reviews"]] <- latest_reviews
      
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
    
    review <- function(df, row_ids, role, choice){
      res <- df
      
      choice_index <- match(choice, review_param[['choices']])
      timestamp <- SH$get_UTC_time_in_seconds()
      dataset_list_name <- 'dataset_list'
      dataset_name <- 'df'
     
      IO_plan <- REV_produce_IO_plan_for_review_action(
        canonical_row_indices = row_ids, role = role, choice_index, timestamp, dataset_list_name, dataset_name
      )
      
      fs_client[["execute_IO_plan"]](IO_plan = IO_plan, is_init = FALSE)
      
      return(res)
    }
    
    return(list(
      track = track, create = create, append = append, shuffle = shuffle, remove = remove, recover = recover,
      mutate = mutate, review = review, store_path = store_path, review_param = review_param
    ))
  })
  
  # NOTE: Random testing
  initial_row_count <- 20
  default_choice <- RT$review_param[['choices']][[1]]

  df <- RT$create()
  # TODO: Add `info <- RT$track(df)` if we ever allow to review 0-row data frames
  df <- RT$append(df, initial_row_count)
  RT$track(df)
  
  rand_0_to_max <- function(max) sample(0:max, 1)
 
  iteration_count <- 100
  
  max_delta_count <- 3
  missing_ids <- integer(0)
  reviews_st <- rep(default_choice, initial_row_count)
  
  for(i_iteration in seq_len(iteration_count)){
    expected <- list(
      # Reset these to check only the change since last updated
      added = integer(0),
      modified = integer(0),
      # Carry this one over from previous iteration to check all missing IDs regardless of when they disappeared
      missing = missing_ids
      # `reviews` appended near the end of this `for` loop
    )
    
    # NOTE: Review
    present_ids <- df[['ID']]
    row_count <- rand_0_to_max(min(length(present_ids), max_delta_count))
    row_ids <- sample(present_ids, row_count)
    role <- sample(RT$review_param[['roles']], 1)
    choice <- sample(RT$review_param[['choices']], 1)
    df <- RT$review(df, row_ids, role, choice)
    reviews_st[row_ids] <- choice
    
    # NOTE: Recover
    missing_ids <- attr(df, 'missing')[['ID']]
    row_count <- rand_0_to_max(min(length(missing_ids), max_delta_count))
    row_ids <- sample(missing_ids, row_count)
    expected[['missing']] <- setdiff(expected[['missing']], row_ids)
    df <- RT$recover(df, row_ids)
    
    # NOTE: Remove 
    present_ids <- df[['ID']]
    row_count <- rand_0_to_max(min(length(present_ids)-1, max_delta_count))  # We don't allow nrow(df) to reach 0
    row_ids <- sample(present_ids, row_count)
    expected[['missing']] <- sort(union(expected[['missing']], row_ids))
    df <- RT$remove(df, row_ids)
    
    # NOTE: Mutate
    present_ids <- df[['ID']]
    row_count <- rand_0_to_max(min(length(present_ids), max_delta_count))
    row_ids <- sample(present_ids, row_count)
    expected[['modified']] <- sort(union(expected[['modified']], row_ids))
    df <- RT$mutate(df, row_ids)
    
    # NOTE: Shuffle (before `append` so that new IDs are introduced in df[['ID']] order)
    df <- RT$shuffle(df)
    
    # NOTE: Append
    extra_row_count <- rand_0_to_max(max_delta_count)
    expected[['added']] <- c(expected[['added']], attr(df, 'max_id') + seq_len(extra_row_count))
    df <- RT$append(df, extra_row_count)
    reviews_st <- c(reviews_st, rep(default_choice, extra_row_count))
    
    expected[['reviews']] <- reviews_st[df[["ID"]]]
    
    # NOTE: Test
    info <- RT$track(df)
    expect_identical(info, expected, info = paste('Iteration:', i_iteration))
    
    missing_ids <- expected[['missing']]
  }
  
  unlink(RT$store_path, recursive = TRUE, force = TRUE)
})

# NOTE: Restore old RNG state
set.seed(rng_seed)
