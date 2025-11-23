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
              
  # Sane sample (`base::sample` without the weird edge case)
  ssample <- function(from, count) from[sample.int(length(from), count, replace = FALSE, prob = NULL)]
  # Shadow `base::sample` for good measure
  sample <- function(...) stop("`sample` is Out of Office today")
              
  # _R_eview _T_est
  RT <- local({
    store_path <- file.path(tempdir(), "random_data_test")
    unlink(store_path, recursive = TRUE, force = TRUE)
    dir.create(store_path)
    
    fs_client <- fs_init(store_path)
    fs_state <- fs_client[["state"]]
    fs_contents <- fs_state[["contents"]]
    fs_client[["list"]]()
    
    # Review folder contents initialized here
    review_param = list(
      datasets = list(df = list(id_vars = c("ID"), tracked_vars = c("TRACKED_1", "TRACKED_2", "TRACKED_3"))),
      choices = c("choiceA", "choiceB"),
      roles = c("roleA", "roleB")
    )
    
    # This function summarizes the latest changes (related either to the tracking of new dataset (`.base`) or to its 
    # updates (`.delta`))
    # If no changes are available since the invocation, no changes are notified.
    # So this is very much _not_ a pure function. Calling it twice on a row will produce different results.
    # Reviews are always returned, however.
    track <- function(df){
      dataset_lists <- list(dataset_list = list(df = df))
     
      # TODO: This initial `_load_` is here to load reviews through a side channel. It would be preferable to 
      #       load them explicitly through a separate function (both inside the module and in this oracle-like test)
      info <- REV_load_annotation_info(fs_contents, review_param, dataset_lists)
      stopifnot(length(info[["error"]]) == 0)
     
      base_and_delta_change_count <- 0L
      for(action in info[["IO_plan"]]) {
        if(action[["kind"]] == "write" && (endsWith(action[["path"]], '.base') ||
                                           endsWith(action[["path"]], '.delta'))){
          base_and_delta_change_count <- base_and_delta_change_count + 1
        }
      }
      fs_client[["execute_IO_plan"]](IO_plan = info[["IO_plan"]])
      
      fnames <- names(fs_contents)
      base_fname <- fnames[endsWith(fnames, '.base')]
      stopifnot(length(base_fname) == 1)
      base_info <- RS_parse_base(fs_contents[[base_fname]])
      total_row_count <- base_info[["row_count"]]
      
      res <- list(
        added = seq_len(total_row_count),
        modified = integer(0),
        missing = integer(0)
      )
      
      sorted_delta_fnames <- sort(fnames[endsWith(fnames, '.delta')])
      for(delta_fname in sorted_delta_fnames){
        delta_info <- RS_parse_delta(fs_contents[[delta_fname]], 
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
      
      if(base_and_delta_change_count == 0L){ # Nothing new since we were last called
        res[["added"]] <- integer(0)
        res[["modified"]] <- integer(0)
        # we do keep the "missing" info
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
      order <- ssample(seq_len(nrow(df)), nrow(df))
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
      
      fs_client[["execute_IO_plan"]](IO_plan = IO_plan)
      
      return(res)
    }
    
    return(list(
      track = track, create = create, append = append, shuffle = shuffle, remove = remove, recover = recover,
      mutate = mutate, review = review, store_path = store_path, review_param = review_param
    ))
  })
  
  # NOTE: Random testing
  tracked_vars <- c("TRACKED_1", "TRACKED_2", "TRACKED_3")

  initial_row_count <- 3
  default_choice <- RT$review_param[['choices']][[1]]

  df <- RT$create()
  # TODO: Add `info <- RT$track(df)` if we ever allow to review 0-row data frames
  df <- RT$append(df, initial_row_count)
  
  reviews_oracle <- rep(default_choice, initial_row_count) # reviews for present and absent rows, in canonical order
  
  oracle_expected <- list(
    added = seq_len(initial_row_count),
    modified = integer(0),
    missing = integer(0),
    reviews = reviews_oracle
  )
  info <- RT$track(df)
  expect_identical(info, oracle_expected, info = paste('Iteration:', i_iteration))
  
  rand_0_to_max <- function(max) ssample(0:max, 1)
  max_review_action_count <- 3L

  iteration_count <- 100L
  max_delta_count <- 3L
  
  # TODO: Add operations that add and remove non-tracked columns
  for(i_iteration in seq_len(iteration_count)){
    oracle_expected[["added"]] <- integer(0)
    oracle_expected[["modified"]] <- integer(0)
    oracle_expected[["reviews"]] <- NULL # appended at the end of this `for` loop
    # "missing" field preserved
    # "reviews"

    # --------------------------------[ ANATOMY OF A TEST ORACLE ITERATION ]-------------------------------------
    #
    # REVIEW PHASE --> <------------------ DATASET UPDATE PHASE --------------------------------> <--- CHECK PHASE
    # 
    #                    recover                                               shuffle
    #    review   -->       +       -->    mutate   -->    append   -->           +            -->   oracle check
    #                    remove                                            sort newly appended
    #
    # We start each iteration with a dataset that has just been updated and loaded in the app. The `.base` and `.delta`
    # files are up to date.
    # 
    # - REVIEW PHASE
    # Up to `max_review_action_count` review passes of up to as many different roles.
    # It's important to allow interleaved reviews of at least two different roles (e.g. A, B, A), 
    # so `max_review_action_count` should be at least three:
    stopifnot(max_review_action_count >= 3L)
    
    
    # NOTE: Review
    review_action_count <- rand_0_to_max(max_review_action_count)
    for(i in seq_len(review_action_count)){
      present_ids <- df[['ID']]
      row_count <- rand_0_to_max(min(length(present_ids), max_delta_count))
      row_ids <- ssample(present_ids, row_count)
      role <- ssample(RT$review_param[['roles']], 1)
      choice <- ssample(RT$review_param[['choices']], 1)
      df <- RT$review(df, row_ids, role, choice)
      reviews_oracle[row_ids] <- choice
    }
    
    # - DATASET UPDATE PHASE
    
    # NOTE: Recover + Remove
    recover_remove_result <- local({
      actions <- ssample(c('recover', 'remove'), 2)
      for(action in actions){
        if(action == 'recover'){
          # NOTE: Recover
          missing_ids <- attr(df, 'missing')[['ID']]
          row_count <- rand_0_to_max(min(length(missing_ids), max_delta_count))
          row_ids <- ssample(missing_ids, row_count)
          df <- RT$recover(df, row_ids)
          oracle_expected[['missing']] <- setdiff(oracle_expected[['missing']], row_ids)
        } else {
          stopifnot(action == 'remove')
          # NOTE: Remove 
          present_ids <- df[['ID']]
          row_count <- rand_0_to_max(min(length(present_ids)-1, max_delta_count))  # We don't allow nrow(df) to reach 0
          row_ids <- ssample(present_ids, row_count)
          df <- RT$remove(df, row_ids)
          oracle_expected[['missing']] <- sort(union(oracle_expected[['missing']], row_ids))
        }
      }
      return(list(df = df, expected = oracle_expected))
    })
    df <- recover_remove_result[["df"]]
    oracle_expected <- recover_remove_result[["expected"]]
    
    # NOTE: Mutate
    #       This step needs to:
    #       - Follow Recover: To expose the module to data that has gone missing and is recovered after a change.
    #       - Follow Remove: To prevent the mutation of data that is immediately removed. The module would not be able
    #                        to see the change. Reproducing that behavior in the logic of the oracle increases its
    #                        complexity. Instead of that we skirt the issue through the order of the operations. This
    #                        approach does not impact the generality of the testing routine.
    present_ids <- df[['ID']]
    row_count <- rand_0_to_max(min(length(present_ids), max_delta_count))
    row_ids <- ssample(present_ids, row_count)
    oracle_expected[['modified']] <- sort(union(oracle_expected[['modified']], row_ids))
    df <- RT$mutate(df, row_ids)
    
    # NOTE: Append
    #       This step needs to:
    #       - Follow Mutate: The act of modifying data that the module still hasn't had a chance to see is pointless.
    #                        The module will treat it just as new data.
    #       - Follow Remove: Adding new data and removing it before the module can see it only complicates matters and
    #                        does not generate a new meaningful testing conditions.
    extra_row_count <- rand_0_to_max(max_delta_count)
    oracle_expected[['added']] <- c(oracle_expected[['added']], attr(df, 'max_id') + seq_len(extra_row_count))
    df <- RT$append(df, extra_row_count)
    reviews_oracle <- c(reviews_oracle, rep(default_choice, extra_row_count))
    newly_appended_row_ids <- tail(df, extra_row_count)[["ID"]]
    
    # NOTE: Shuffle + Sort newly appended
    #       This step is best left for the end, so that it has a chance to affect all records.
    #       We do restore the order of newly appended rows so that we can df[["ID"]] follows canonical order.
    #       This does not impact the generality of the testing routine.
    #       We could place Shuffle before Append to avoid this reordering, but then we would have to Insert the new
    #       records in arbitrary row numbers, which is a bit involved. 
    #       Appending, Shuffling and doing a partial reordering has the same effect.
    df <- RT$shuffle(df)
    
    newly_appended_row_mask <- df[["ID"]] %in% newly_appended_row_ids
    if(any(newly_appended_row_mask)){ # Restore order of data the module has not seen (See [1] below)
      subdf <- df[newly_appended_row_mask,]
      subdf <- subdf[order(subdf[['ID']]),]
      df[newly_appended_row_mask,] <- subdf
    }
    
    oracle_expected[['reviews']] <- reviews_oracle[df[["ID"]]]
    
    # - CHECK PHASE
    # NOTE: Oracle check
    info <- RT$track(df)
    expect_identical(info, oracle_expected, info = paste('Iteration:', i_iteration))
  }
  
  unlink(RT$store_path, recursive = TRUE, force = TRUE)
})

# NOTE: Restore old RNG state
set.seed(rng_seed)
