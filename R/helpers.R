#' Internal helper function to gather either column or name labels of data.frames
#'
#' @param dataset `[data.frame | list(data.frame)]`
#'
#' Single data.frame to gather its column labels or a list of data.frames to gather their name labels.
#'
#' @return Character vector of labels of \code{dataset}. Named with the respective column or data.frame names. Contains
#' "No label" for missing labels.
#'
#' @keywords internal
get_labels <- function(dataset) {
  # Catch special cases (e.g. when global filter returns empty dataset)
  return_null <- any(length(dataset) == 0, is.null(dataset))
  if (return_null) {
    return(NULL)
  }

  # Check validity of parameters
  # Note: In case you change something here, consider adding a check in generate_choices() instead
  checkmate::assert(
    checkmate::check_data_frame(dataset, null.ok = TRUE),
    checkmate::check_list(dataset, types = "data.frame", null.ok = TRUE)
  )

  if (checkmate::test_list(dataset, null.ok = TRUE)) {
    checkmate::assert_names(names(dataset))
  }


  # Gather labels
  all_labels <- names(dataset) |>
    purrr::set_names() |>
    purrr::map_chr(function(name) {
      label <- attributes(dataset[[name]])$label
      if (is.null(label)) {
        label <- "No label"
      }

      return(label)
    })

  return(all_labels)
}

#' Internal helper function to generate either column or dataset choices for the corresponding UI dropdown menues
#'
#' @param dataset `[data.frame | list(data.frame)]`
#'
#' Single data.frame to retrieve its column labels or a list of data.frames to retrieve their name labels. In both
#' cases the labels will be pasted to the column/dataset names and returned in order to provide meaningful choices in
#' the corresponding dropdown menues to the end-user.
#'
#' @return Character vector of the type `<name> [<label>]`. Adds "No label" as placeholder for missing labels.
#'
#' @keywords internal
generate_choices <- function(dataset) {
  # No checkmate checks since it is catched in get_labels()

  labels <- get_labels(dataset)
  extended_names <- paste0(names(dataset), " [", labels, "]")
  choices <- stats::setNames(names(dataset), extended_names)

  return(choices)
}


#' Internal helper function to set missing default columns
#'
#' Avoid empty displays in case no default columns were specified by showing the first six columns of a dataset. If the
#' dataset does contain less than six columns, the whole dataset will be displayed.
#'
#' @param default_vars `[list(character(0+))]`
#'
#' Named list of character vectors which contain the names of columns to be displayed at module
#'   launch per dataset. List entries are named according to the \code{dataset} names but can be NULL for some or all
#'   datasets.
#' @param dataset `[list(data.frame)]`
#'
#' A list of data.framish dataset(s) that will be shown as listings.
#'
#' @return Named list of character vectors which Contain the names of columns to be displayed at module launch for
#'   every dataset. List entries are named according to the \code{dataset} names.
#' @keywords internal
fill_default_vars <- function(default_vars, dataset) {
  # Check arguments
  checkmate::assert(
    checkmate::check_list(dataset, types = "data.frame"),
    checkmate::check_names(names(dataset), type = "unique"),
    checkmate::check_list(default_vars, types = "character", null.ok = TRUE),
    combine = "and"
  )
  if (!is.null(default_vars)) {
    checkmate::assert_names(names(default_vars), type = "unique", subset.of = names(dataset))
  }
  purrr::walk2(
    default_vars, names(default_vars),
    ~ checkmate::assert_character(.x, unique = TRUE, .var.name = paste0("default_vars$", .y))
  )
  purrr::walk(
    names(default_vars),
    ~ checkmate::assert_subset(default_vars[[.x]], names(dataset[[.x]]), .var.name = paste0("default_vars$", .x))
  )

  # Fill default_vars
  col_list <- purrr::imap(dataset, function(df, name) {
    if (is.null(default_vars[[name]])) {
      n_def <- min(ncol(df), 6)
      cols <- names(df)[1:n_def]
    } else {
      cols <- default_vars[[name]]
    }
    return(cols)
  })

  return(col_list)
}


#' Prepare data as it should be displayed in the module
#'
#' Reduces the dataset to only those columns specified in \code{selector} and produces descriptive column names
#' by adding their labels to the variable names.
#'
#' @param base_data `[data.frame]`
#'
#' Single data.frame.
#' @param selector `[character(0+)]`
#'
#' Character vector of a selection of column names from \code{base_data}.
#'
#' @return A list of tree elements: \code{data} is the data.frame prepared to be displayed as-is. \code{col_names} is a
#'   vector of column names created by combining the variable names with their labels.
#'   \code{row_names} is a character vector containing the number of the corresponding row.
#' @keywords internal
set_data <- function(base_data, selector) {
  # Check validity of parameters
  checkmate::assert(
    checkmate::check_data_frame(base_data, null.ok = TRUE),
    checkmate::check_character(selector, null.ok = TRUE),
    combine = "and"
  )

  # Return NULL in case no columns were selected
  return_null <- any(is.null(selector), length(selector) == 0)
  if (return_null) {
    return(NULL)
  }

  # Select user specified (or default) columns from data and force order as determined by the user
  data <- base_data |>
    dplyr::select(dplyr::all_of(selector)) |>
    dplyr::relocate(dplyr::all_of(selector))

  # Get labels for selected columns
  labels <- get_labels(data)
  # Combine names with labels
  col_names <- paste0(selector, " [", labels, "]")

  row_names <- as.character(seq_len(nrow(data)))

  return(
    list(
      data = data,
      col_names = col_names,
      row_names = row_names
    )
  )
}


#' Internal helper function to set labels for all dataset columns
#'
#' @param dataset `[data.frame]`
#'
#' Single data.frame
#' @param labels Character vector of labels named with the respective column names \code{dataset}.
#'
#' @return A data.frame with column labels
#' @keywords internal
set_labels <- function(dataset, labels) {
  checkmate::assert(
    checkmate::check_data_frame(dataset),
    checkmate::check_character(labels),
    checkmate::check_names(names(labels), type = "named", permutation.of = names(dataset)),
    combine = "and"
  )

  ret_dataset <- dataset |>
    dplyr::mutate(dplyr::across(dplyr::everything(), ~ structure(.x, label = labels[[dplyr::cur_column()]])))

  return(ret_dataset)
}


#' This function is a wrapper for [utils::type.convert()] to keep column labels
#'
#' Converts data types of data.frame columns to logical, integer, numeric, complex, character or factor as appropriate.
#' Re-assigns column labels after converting data type.
#'
#' @param dataset `[data.frame]`
#'
#' Single data.frame to convert the column data types
#'
#' @return data.frame with converted data types of data.frame columns
#' @export
convert_data <- function(dataset) {
  # check validity of parameter
  checkmate::assert_data_frame(dataset)

  labels <- get_labels(dataset)
  data <- utils::type.convert(dataset, as.is = FALSE)
  data <- set_labels(data, labels)

  return(data)
}


#' Internal helper function to set up colnames, rownames, and paging arguments for DT's datatable.
#' Main purpose is to easily test returned arguments.
#'
#' @param dataset  `[data.frame]`
#'
#' Single data.frame
#' @param selected_cols `[character(0+)]`
#'
#' Character vector of a selection of column names from the dataset
#' @param pagination `[logical(1) | NULL]`
#'
#' Either a boolean indicating if pagination should be activated, or
#' NULL for which pagination will be activated for large datasets (nrows > 1000) automatically.
#' @return List containing character vectors for column names and row names and
#' a logical value for de-/activating paging
#' @keywords internal
set_up_datatable <- function(dataset, pagination) {
  # skip checkmate checks because this function only exists to be able to test paging

  labels <- get_labels(dataset) # Get labels for selected columns
  col_names <- paste0(names(dataset), " [", labels, "]") # Combine names with labels
  row_names <- as.character(seq_len(nrow(dataset)))
  paging <- if (is.null(pagination)) nrow(dataset) > 1000 else pagination

  return(
    list(
      col_names = col_names,
      row_names = row_names,
      paging = paging
    )
  )
}
