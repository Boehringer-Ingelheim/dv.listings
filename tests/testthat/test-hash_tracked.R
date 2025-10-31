test_that("SH$hash_tracked exhibits no false negatives and few false positives" |>
            vdoc[["add_spec"]](specs$review_hash_no_false_negatives), {
  mutate <- function(df, i_row, i_col){
    v <- df[[i_col]][[i_row]]
    cl <- class(df[[i_col]])[[1]]
    if(cl == 'character'){
      v <- paste0(v, '+extra')
    } else if(cl %in% c('numeric', 'Date')){
      v <- v + 1
      if(is.na(v)) v <- 0
    } else {
      browser()
    }
    df[[i_col]][[i_row]] <- v
    return(df)
  }
  
  stress <- function(df, test_count, changes_per_test){
    h0 <- SH$hash_tracked(df[colnames(df)])
    
    n_row <- nrow(df)
    n_col <- ncol(df)
    
    false_positives <- list()
    false_positive_first_delta <- list()
    false_negatives <- list()
    false_negative_first_delta <- list()
    
    for(i_test in seq(test_count)){
      change_coords <- rep(list(integer(0)), n_row)
      row <- sample(x = seq(n_row), size = 1)
      for(i_change in seq(changes_per_test)){
        while(TRUE){
          col <- sample(x = seq(n_col), size = 1)
          if(!(col %in% change_coords[[row]])) break
        }
        change_coords[[row]] <- sort(c(change_coords[[row]], col))
      }
      
      dfp <- df
      expected_changes <- list()
      for(i_row in seq(n_row)){
        for(i_col in change_coords[[i_row]]){
          expected_changes[[length(expected_changes)+1]] <- c(i_row, i_col)
          dfp <- mutate(dfp, i_row, i_col)
        }
      }
      
      h1 <- SH$hash_tracked(dfp[colnames(dfp)])
   
      reported_changes_row_cols <- REV_report_changes(h0, h1)
      
      # Flatten changes for simpler downstream comparison
      reported_changes <- list()
      for(row_cols in reported_changes_row_cols){
        row <- row_cols[["row"]]
        for(col in row_cols[["cols"]]) reported_changes[[length(reported_changes) + 1]] <- c(row, col)
      }
      
      if(!identical(expected_changes, reported_changes)) {
        i_exp <- 1
        i_rep <- 1
        while(i_exp <= length(expected_changes) && i_rep <= length(reported_changes)){
          exp <- expected_changes[[i_exp]]
          rep <- reported_changes[[i_rep]]
          if(identical(exp, rep)){
            i_exp <- i_exp+1
            i_rep <- i_rep+1
          } else {
            exp_lower <- exp[[1]] < rep[[1]] || (exp[[1]] == rep[[1]] && exp[[2]] < rep[[2]])
            if(exp_lower){
              if(length(false_negatives) == 0) false_negative_first_delta <- expected_changes
              false_negatives <- append(false_negatives, list(exp))
              i_exp <- i_exp+1
            } else{
              if(length(false_positives) == 0) false_positive_first_delta <- expected_changes
              false_positives <- append(false_positives, list(rep))
              i_rep <- i_rep+1
            }
          }
        }
        if(i_exp <= length(expected_changes)) {
          if(length(false_negatives) == 0) false_negative_first_delta <- expected_changes
          tail <- expected_changes[i_exp:length(expected_changes)]
          false_negatives <- append(false_negatives, tail)
        }
        if(i_rep <= length(reported_changes)) {
          if(length(false_positives) == 0) false_positive_first_delta <- expected_changes
          tail <- reported_changes[i_rep:length(reported_changes)]
          false_positives <- append(false_positives, tail)
        }
      }
    }
    
    return(list(fp = false_positives, fpfd = false_positive_first_delta, 
                fn = false_negatives, fnfd = false_negative_first_delta))
  }
  
  ae <- safetyData::adam_adae[1:10,]
  test_count <- 1000
  
  res <- stress(ae, test_count, changes_per_test = 1)
  expect_lte(length(res[["fp"]]), 10)
  expect_lte(length(res[["fn"]]), 0)
  res <- stress(ae, test_count, changes_per_test = 2)
  expect_lte(length(res[["fp"]]), 20)
  expect_lte(length(res[["fn"]]), 0)
  res <- stress(ae, test_count, changes_per_test = 3)
  expect_lte(length(res[["fp"]]), 40)
  expect_lte(length(res[["fn"]]), 0)
  res <- stress(ae, test_count, changes_per_test = 4)
  expect_lte(length(res[["fp"]]), 80)
  expect_lte(length(res[["fn"]]), 0)
  
 
  # Examine false negatives with: 
  if(FALSE){
    print(fn[[1]])
    print("Delta:")
    delta <- res[["fnfd"]]
    
    print(delta)
    
    dfp <- ae
    for(change in delta){
      i_row <- change[[1]]
      i_col <- change[[2]]
      dfp <- mutate(dfp, i_row, i_col)
    }
    info <- hash(ae, tracked_vars = colnames(ae), c, b)
    reported_changes <- report_changes(dfp, info, c, b, verbose = TRUE)
  }
})

test_that("Row changes can be attributed to specific modified columns" |>
            vdoc[["add_spec"]](specs$review_change_attribution), {
  folder_contents <- NULL
  fs_callbacks <- list(
    attach = function(arg) NULL, list = function(arg) NULL, read = function(arg) NULL, write = function(arg) NULL,
    append = function(arg) NULL, execute_IO_plan = function(arg) NULL,
    read_folder = function(arg) folder_contents <<- arg
  )
  
  store_path <- file.path(tempdir(), "data_checks")
  dir.create(store_path, showWarnings = FALSE)
  on.exit(unlink(store_path, recursive = TRUE), add = TRUE, after = FALSE)
  
  fs_client <- fs_init(callbacks = fs_callbacks, store_path)
  fs_client[["read_folder"]](subfolder_candidates = "dataset_list")
  
  tracked_vars <- c(
    "AESEV", "AETERM", "AEHLGT", "AEHLT", "AELLT", 
    "AEDECOD", "AESOC", "AESTDTC", "AEENDTC", "AESTDY","AEOUT", "AEACN", "AEREL"
  )
  
  # Review folder contents initialized here with a dataset consisting of two rows from `sdtm_ae`
  review = list(
    datasets = list(ae = list(id_vars = c("USUBJID", "AESEQ"),  tracked_vars = tracked_vars)),
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
  
  Sys.sleep(1)
  
  # The severity of the first record changes on the first dataset update (#first_change)
  
  dataset_lists[["dataset_list"]][["ae"]][["AESEV"]][[1]] <- "SEVERE"
  info <- REV_load_annotation_info(folder_contents, review, dataset_lists)
  fs_client[["execute_IO_plan"]](IO_plan = info[["folder_IO_plan"]], is_init = TRUE)
  fs_client[["read_folder"]](subfolder_candidates = "dataset_list")
  
  Sys.sleep(1)
 
  # The severity of the second record changes on the second dataset update (#second_change)
  dataset_lists[["dataset_list"]][["ae"]][["AESEV"]][[2]] <- "SEVERE"
  # On the same update two new rows appear _before_ the two already known rows
  dataset_lists[["dataset_list"]][["ae"]] <- rbind(safetyData::sdtm_ae[3:4,], dataset_lists[["dataset_list"]][["ae"]])
  
  info <- REV_load_annotation_info(folder_contents, review, dataset_lists)
  fs_client[["execute_IO_plan"]](IO_plan = info[["folder_IO_plan"]], is_init = TRUE)
  fs_client[["read_folder"]](subfolder_candidates = "dataset_list")
  
  # Compute which columns have changed
  revisions <- attr(info[["loaded_annotation_info"]][["dataset_list"]][["ae"]], "revisions")
  
  canonical_tracked_vars <- sort(tracked_vars)
  
  # We fake a complete review process that happens before the latest dataset update
  revision_count <- length(revisions$tracked_hashes)
  timestamp_a_bit_lower_than_latest_update <- (revisions$timestamps[[revision_count]] + 
                                                 revisions$timestamps[[revision_count-1]])/2.
  
  latest_revision_row_count <- ncol(revisions$tracked_hashes[[revision_count]])
  review_timestamps <- rep(timestamp_a_bit_lower_than_latest_update, latest_revision_row_count)
  
  h0 <- REV_collect_latest_review_hashes(revisions, review_timestamps)
  h1 <- revisions$tracked_hashes[[revision_count]]
  
  changes <- REV_report_changes(h0, h1)

  # Row and column numbers reported here are canonical. Meaning:
  # - rows denote order in which they were added. 1 and 2 are the first rows even though they are now rows 3 and 4
  #   in the last revision of the dataset
  # - columns refer to the indices of columns after _sorting them by name_
   
  expected_changes <- list(
    # Row 1 is not listed here because we've specified that #first_change preceeds the review time of this row
    # AESEV of second row, modified here #second_change
    list(row = 2L, cols = 9L),
    # All columns for the third and fourth rows (added on #second_change) are notified
    list(row = 3L, cols = 1:13), 
    list(row = 4L, cols = 1:13)
  )
  
  expect_equal(changes, expected_changes)
})
