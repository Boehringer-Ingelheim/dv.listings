test_that("SH$hash_tracked exhibits almost no false negatives and few false positives", {
  hash_df <- function(df, tracked_vars) {
    hashes <- SH$hash_trackeddf[tracked_vars]
    return(hashes)
  }
 
  # TODO(miguel): Refactor and move next to hash_tracked into SH  
  report_changes <- function(df, h0, verbose = FALSE){
    res <- list()
    h1 <- hash_df(df, tracked_vars = colnames(df))
    
    offsets <- c(0, 2, 3)
    
    n_col <- ncol(df)
    row_diff_indices <- which(apply(h0 != h1, 2, any))
    for (i_row in row_diff_indices) {
      prev <- as.integer(h0[,i_row])
      cur <- as.integer(h1[,i_row])
      diff <- (prev != cur)
      evidence <- integer(n_col)
      for(i in seq_len(n_col)){
        v <- diff[[i]]
        affected_indices <- (((i-1) + offsets) %% n_col) + 1
        delta <- isTRUE(v)
        evidence[affected_indices] <- evidence[affected_indices] + delta
        
        if(verbose) print(evidence)
      }
      inferred_change_count <- ceiling(sum(diff)/length(offsets))
      
      # removes false negatives at the cost of false positives
      threshold <- min(head(sort(evidence, decreasing = TRUE), inferred_change_count))
      col_indices <- which(evidence >= threshold)
      col_names <- paste(names(df)[col_indices], collapse = ', ')
      
      for(i_col in col_indices){
        res[[length(res)+1]] <- c(i_row, i_col)
      }
    }
    return(res)
  }
  
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
    hashes <- hash_df(df, tracked_vars = colnames(df))
    
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
   
      reported_changes <- report_changes(dfp, hashes)
      
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
          if(length(false_positives) == 0)  false_positive_first_delta <- expected_changes
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
  expect_lte(length(res[["fn"]]), 3)
  
 
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
