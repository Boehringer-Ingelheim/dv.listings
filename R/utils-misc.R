# It doesn't make sense for a reactive to trigger when its value has not changed,
# given the fact that every reactive caches its value.
# `reactiveVal` does not suffer from that design mistake, so we can use it to
# filter spureous reactive invalidations.
# We have to apply this treatment judiciously because it makes an extra copy of
# the value returned by the reactive it wraps around, which may prove expensive.
#
# TODO: test this when r() returns a silent error, this may crash the app, or the error value will not be propagated
# though rv()
trigger_only_on_change <- function(r) {
  rv <- shiny::reactiveVal()
  shiny::observe(rv(r()))
  rv
}

#' If x is `NULL`, return y, otherwise return x
#'
#' @param x,y Two elements to test, one potentially `NULL`
#'
#' @noRd
#'

#' @keywords internal
"%||%" <- function(x, y) {
  if (is.null(x)) {
    y
  } else {
    x
  }
}

relocate_column <- function(df, column_name, destination_index) {
  if(ncol(df) < destination_index) return(simpleCondition("Not enough columns"))
  if(destination_index < 1L) return(simpleCondition("`destination_index` must be positive"))
  
  original_index <- which(names(df) == column_name)
  if(length(original_index) != 1L)
    return(simpleCondition("`column_name` must uniquely identify a single column of the input data.frame"))
  
  if(original_index == destination_index) return(df)
 
  all_but_one_column_indices <- setdiff(1:ncol(df), original_index)
  target_order <- append(all_but_one_column_indices, original_index, after = destination_index - 1L)
  df <- df[, target_order]
  
  return(df)
}

