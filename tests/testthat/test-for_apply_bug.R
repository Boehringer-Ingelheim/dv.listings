# 303747-replace-apply-for-bug
# 
# When using apply to iterate through the rows of a data.frame it automatically casts the type to the a common
# representation. In the case below, the common representation is character therefore before the loop starts all
# df is transformed into character. Curiously, the casting does not work as one would expect and because the widest
# element in b is two characters b will be casted in a character vector of elements of width 2 left-padded with spaces.
#
# # apply(as.data.frame(list(a = c(1L,2L), b = c(2L, 20L), c = "B")), 1, function(row) paste(row, collapse = "%%"))
# # [1] "1%% 2%%B" "2%%20%%B" # Notice the space before the two, when one element is of at least width 2
#
# # apply(as.data.frame(list(a = c(1L,2L), b = c(2L, 0L), c = "B")), 1, function(row) paste0(row, collapse = "%%"))
# # [1] "1%%2%%B" "2%%0%%B" # Notice how the space is not there when all elements are of width 1
#
# # apply(as.data.frame(list(a = c(1L,2L), b = c(2L, 100L), c = "B")), 1, function(row) paste0(row, collapse = "%%"))
# # [1] "1%%  2%%B" "2%%100%%B" # Notice the two spaces when widest element is of length three
#
# Also fails for 
# # apply(as.data.frame(list(a = c(1.21,2.2), c = "B")), 1, function(row) paste0(row, collapse = "%%")) # See trailing 0 in 2.2
# # apply(as.data.frame(list(a = c(1.2,2.2), c = "B")), 1, function(row) paste0(row, collapse = "%%")) 
#
# When calculating hashes the following call was used
# # apply(df, 1, SH$hash_tracked, simplify = TRUE)
# Therefore returned hashes for each row differ even when the row in df was the same, as the casted contents
# differ.
#

# Save current RNG state
if (!exists(".Random.seed", envir = .GlobalEnv)) runif(1)  # ensure seed exists
old_seed <- .Random.seed
on.exit(assign(".Random.seed", old_seed, envir = .GlobalEnv), add = TRUE)
set.seed(Sys.time())

generate_random_df <- function(n_rows, equal_length = FALSE) {
  
  random_strings <- function(n) {
    min_len <- 1
    max_len <- 20
    replicate(
      n,
      {
        l <- if(equal_length) max_len else sample(min_len:max_len, 1)
        paste0(sample(c(letters, LETTERS), l, replace = TRUE), collapse = "")
      }
    )
  }

  # Random date range
  start_date <- as.Date("2000-01-01")
  end_date <- as.Date("2020-12-31")
  date_range <- as.numeric(end_date - start_date)

  num <- if(equal_length) sample(10:99, n_rows, replace = TRUE) + sample(1:9, n_rows, replace = TRUE)/100 else rnorm(n_rows, mean = 100)
  int <- as.integer(if(equal_length) sample(10:99, n_rows, replace = TRUE) else sample(1:100, n_rows, replace = TRUE))

  df <- data.frame(
    num    = num,             
    int    = int,          
    date   = start_date + sample(0:date_range, n_rows, TRUE),
    log    = sample(c(TRUE, FALSE), n_rows, replace = TRUE), 
    factor = factor(random_strings(n_rows)),     
    char   = random_strings(n_rows)
  )

  return(df)
}


# Tested by double programming with the previous hash functions
fixed_apply_hash <- function(df, fun) {
  char <- lapply(df, as.character)
  mat <- matrix(unlist(char), ncol = length(char), nrow = length(char[[1]]))
  apply(mat, 1, fun)
}


test_that(
  "apply, fixed_lapply and hash_id are identical when rows of each column have the same width", {
  
  df <- generate_random_df(100, equal_length = TRUE)
  apply_hash_res <- apply(df, 1, SH$`..old`$hash_id, simplify = TRUE)
  fixed_apply_hash_res <- fixed_apply_hash(df, SH$`..old`$hash_id)
  vectorized_hash <- SH$hash_id(df)
  expect_identical(apply_hash_res, fixed_apply_hash_res)
  expect_identical(apply_hash_res, vectorized_hash)  
})

test_that(
  "fixed_lapply and hash_id are identical when rows of each column have different width", {  
  df <- generate_random_df(100, equal_length = FALSE)  
  fixed_apply_hash_res <- fixed_apply_hash(df, SH$`..old`$hash_id)
  vectorized_hash <- SH$hash_id(df)
  expect_identical(fixed_apply_hash_res, vectorized_hash)  
})

test_that(
  "apply, fixed_lapply and hash_tracked are identical when rows of each column have the same width", {
  
  df <- generate_random_df(100, equal_length = TRUE)
  apply_hash_res <- apply(df, 1, SH$`..old`$hash_tracked, simplify = TRUE)
  fixed_apply_hash_res <- fixed_apply_hash(df, SH$`..old`$hash_tracked)
  vectorized_hash <- SH$hash_tracked(df)
  expect_identical(apply_hash_res, fixed_apply_hash_res)
  expect_identical(apply_hash_res, vectorized_hash)  
})

test_that(
  "fixed_lapply and hash_tracked are identical when rows of each column have different width", {  
  df <- generate_random_df(100, equal_length = FALSE)  
  fixed_apply_hash_res <- fixed_apply_hash(df, SH$`..old`$hash_tracked)
  vectorized_hash <- SH$hash_tracked(df)
  expect_identical(fixed_apply_hash_res, vectorized_hash)  
})

# df <- generate_random_df(10000, equal_length = TRUE)
# microbenchmark::microbenchmark(
#   apply = apply(df, 1, SH$`..old`$hash_id, simplify = TRUE),
#   fixed_apply = fixed_apply_hash(df, SH$`..old`$hash_id),
#   vectorized = SH$hash_id(df)
# )

# microbenchmark::microbenchmark(
#   apply = apply(df, 1, SH$`..old`$hash_tracked, simplify = TRUE),
#   fixed_apply = fixed_apply_hash(df, SH$`..old`$hash_tracked),  
#   vectorized = SH$hash_tracked(df)
# )
