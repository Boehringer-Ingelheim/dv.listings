# YT#VH1202cc468f3cbf448dd25ff917fc4762#VH00000000000000000000000000000000#
TC <- local({ # _T_ype C_hecks
  # basic types
  T_logical <- function() list(kind = "logical")
  T_factor <- function() list(kind = "factor")
  T_character <- function() list(kind = "character")
  T_date <- function() list(kind = "date")
  T_datetime <- function() list(kind = "datetime")
  T_integer <- function(min = NA, max = NA) list(kind = "integer", min = min, max = max) # allows numeric if all values are integer
  T_numeric <- function(min = NA, max = NA) list(kind = "numeric", min = min, max = max)

  # permissive types
  T_anything <- function() list(kind = "anything")

  # sum types
  T_or <- function(...) list(kind = "or", options = list(...))

  # known- and variable-length collections
  T_group <- function(...) list(kind = "group", elements = list(...))

  # domain-specific types
  T_mod_ID <- function() list(kind = "mod")
  T_dataset_name <- function() list(kind = "dataset_name")
  T_col <- function(dataset_name, sub_kind = T_anything()) {
    list(kind = "col", dataset_name = dataset_name, sub_kind = sub_kind)
  }
  T_color <- function() list(kind = "color")
  T_CDISC_study_day <- function() list(kind = "cdisc_study_day", min = NA, max = NA)
  T_YN <- function() list(kind = "YN")
  T_choice_from_col_contents <- function(param) list(kind = "choice_from_col_contents", param = param)
  T_choice <- function(param) list(kind = "choice", param = param)
  T_fn <- function(arg_count) list(kind = "function", arg_count = arg_count)

  T_is_of_kind <- function(var, type) {
    res <- FALSE
    if (length(type) == 1 && is.na(type)) browser()

    if (type[["kind"]] == "or") {
      for (option in type[["options"]]) res <- res || T_is_of_kind(var, option)
    } else if (type[["kind"]] == "anything") {
      res <- TRUE
    } else if (type[["kind"]] == "factor") {
      res <- is.factor(var)
    } else if (type[["kind"]] == "character") {
      res <- is.character(var)
    } else if (type[["kind"]] == "date") {
      res <- inherits(var, "Date")
    } else if (type[["kind"]] == "datetime") {
      res <- inherits(var, "POSIXt")
    } else if (type[["kind"]] == "numeric") {
      res <- is.numeric(var)
    } else if (type[["kind"]] == "integer") {
      res <- is.integer(var) || (is.numeric(var) && all(var[is.finite(var)] %% 1 == 0))
    } else if (type[["kind"]] == "logical") {
      res <- is.logical(var)
    } else if (type[["kind"]] == "cdisc_study_day") {
      res <- (is.integer(var) || (is.numeric(var) && all(var[is.finite(var)] %% 1 == 0))) && all(var[is.finite(var)] != 0)
    } else if (type[["kind"]] == "YN") {
      res <- ((is.character(var) && setequal(unique(var), c("Y", "N"))) ||
        is.factor(var) && setequal(levels(var), c("Y", "N")))
    } else {
      browser()
    }
    return(res)
  }

  # flags
  T_flag <- function(x, ...) {
    flag_names <- list(...)

    unknown_flags <- setdiff(
      flag_names,
      c( # common flags
        "optional",                   # parameter can be skipped
        "zero_or_more",               # array/list contains a variable number of elements
        "one_or_more",                # array/list contains at least one element and possibly more
        "as_array",                   # list is converted to array for the sake of implementation simplicity (miguel: I believe it's only used in papo)
        "named",                      # elements of targetted argument must be named
        "map_character_to_factor",    # the target TC$col() will be transformed to factor prior to reaching the module
        "ignore",                     # argument should be ignored by Dressing Room, for now
        # domain-specific flags
        "subject_level_dataset_name", # indicates dataset with one row per subject
        "subjid_var"                  # indicates unique subject identifier column on dataset pointed at by subject_level_dataset_name 
      )
    )
    if (length(unknown_flags)) browser()

    flag_values <- as.list(rep(TRUE, length(flag_names)))
    flags <- stats::setNames(flag_values, flag_names)
    return(do.call(structure, append(list(x), flags)))
  }

  T_map_to <- function(orig, dest) structure(orig, map_to = dest) # maps dataset col to a type the module understands

  # Pair documentation with module API ----

  T_get_type_as_text <- function(elem) {
    res <- ""

    types <- list(
      group = "list",
      logical = "logical",
      factor = "factor",
      integer = "integer",
      cdisc_study_day = "integer",
      numeric = "numeric",
      mod = "character",
      dataset_name = "character",
      col = "character",
      color = "character",
      character = "character",
      date = "Date",
      datetime = "POSIXt",
      YN = '"Y"/"N"',
      `function` = "function"
    )

    if (elem$kind == "or") {
      res <- paste(Map(T_get_type_as_text, elem$options), collapse = "|")
    } else if (elem$kind == "choice") {
      res <- "character" # FIXME: Refer to the type of the column
    } else if (elem$kind == "choice_from_col_contents") {
      res <- "character" # FIXME: Refer to the type of the column
    } else if (!(elem$kind %in% names(types))) {
      message(paste("Missing kind", elem$kind))
    } else {
      res <- types[[elem$kind]]
    }

    return(res)
  }

  T_get_use_as_text_lines <- function(elem) {
    res <- character(0)

    if (elem$kind == "mod") {
      res <- "Unique Shiny module identifier"
    } else if (elem$kind == "dataset_name") {
      if (isTRUE(attr(elem, "subject_level_dataset_name"))) {
        res <- "Subject-level dataset name"
      } else {
        res <- "Dataset name"
      }
    } else if (elem$kind == "col") {
      if (isTRUE(attr(elem, "subjid_var"))) {
        res <- "Unique subject identifier column"
      } else {
        res <- sprintf("Indexes into dataset `%s`", elem$dataset_name)
        if (!identical(elem$sub_kind, T_anything())) {
          res <- c(res, sprintf("Expects `[%s]` values", T_get_type_as_text(elem$sub_kind)))
        }
      }
    } else if (elem$kind == "cdisc_study_day") {
      res <- "Represents a CDISC (non-zero) Study Day"
    } else if (elem$kind == "color") {
      res <- "Contains either an HTML (#xxxxxx) or an R color"
    } else if (elem$kind == "choice") {
      res <- "<placeholder>" # TODO: Refer to the actual column
    } else if (elem$kind == "choice_from_col_contents") {
      res <- "<placeholder>" # TODO: Refer to the actual column
    } else if (elem$kind %in% c("logical", "integer", "numeric", "character", "group", "function")) {
      # nothing
    } else {
      message(paste("Missing use for kind", elem$kind))
    }

    return(res)
  }

  T_attach_docs <- function(api, docs) {
    stopifnot(is.character(docs[[1]]))

    attr(api, "docs") <- list(
      type = T_get_type_as_text(api),
      auto_desc = T_get_use_as_text_lines(api),
      manual_desc = docs[[1]]
    )

    if (api$kind == "group") {
      docs[[1]] <- NULL

      if (length(api$elements) != length(docs)) {
        stop(sprintf("api and docs are of different lengths (%d and %d)", length(api), length(docs)))
      } else if (!identical(names(api$elements), names(docs))) {
        stop(sprintf(
          "api and docs have different names (%s and %s)",
          paste(names(api$elements), collapse = ","), paste(names(docs), collapse = ",")
        ))
      }

      for (i in seq_along(api$elements)) {
        api$elements[[i]] <- T_attach_docs(api$elements[[i]], docs[[i]])
      }
    }

    return(api)
  }

  T_eval_args <- function(args, eval_env) {
    # evaluate arguments before handing them down to arg-rewriting routines
    arg_names <- names(args)
    for (i_arg in seq_along(args)) {
      name <- arg_names[[i_arg]]
      eval_res <- eval(args[[i_arg]], envir = eval_env)
      args[i_arg] <- stats::setNames(list(eval_res), name) # R inferno 8.1.55
    }
    return(args)
  }

  # Permit caller to provide lists when arrays are desired by the module ----

  T_honor_as_array_flag_inner <- function(api_field, elem) {
    if (isTRUE(attr(api_field, "zero_or_more")) || isTRUE(attr(api_field, "zero_or_more"))) {
      attr(api_field, "zero_or_more") <- FALSE
      attr(api_field, "one_or_more") <- FALSE
      for (i in seq_along(elem)) {
        elem[[i]] <- T_honor_as_array_flag_inner(api_field, elem[[i]])
      }
    } else if (api_field$kind == "group") {
      elem_names <- names(elem)
      for (i in seq_along(elem)) {
        name <- elem_names[[i]]
        if (!is.null(name) && name %in% names(api_field[["elements"]]) && !is.null(elem[[i]])) {
          elem[i] <- stats::setNames(
            list(T_honor_as_array_flag_inner(api_field[["elements"]][[name]], elem[[i]])), name
          ) # R inferno 8.1.55
        }
      }
    }

    if (isTRUE(attr(api_field, "as_array")) && is.list(elem)) {
      elem <- unlist(elem)
    }

    return(elem)
  }

  T_honor_as_array_flag <- function(mod_API, args) {
    env_that_called_the_module_function <- parent.frame(2)
    args <- T_eval_args(args, eval_env = env_that_called_the_module_function)
    args <- T_honor_as_array_flag_inner(mod_API, args)
    return(args)
  }

  # Map allowed types to those expected by the module ----

  T_honor_map_to_flag_inner <- function(datasets, api_field, elem, field_to_dataset_map, current_field_name) {
    res <- list(map = field_to_dataset_map, actions = list())

    if (isTRUE(attr(api_field, "zero_or_more")) || isTRUE(attr(api_field, "zero_or_more"))) {
      attr(api_field, "zero_or_more") <- FALSE
      attr(api_field, "one_or_more") <- FALSE
      for (i in seq_along(elem)) {
        res <- T_honor_map_to_flag_inner(datasets, api_field, elem[[i]], field_to_dataset_map, current_field_name)
      }
    } else if (api_field$kind == "group") {
      group_field_to_dataset_map <- field_to_dataset_map # push new mapping used only inside group

      elem_names <- names(elem)
      for (i in seq_along(elem)) {
        name <- elem_names[[i]]
        if (!is.null(name) && name %in% names(api_field[["elements"]]) && !is.null(elem[[i]])) {
          subres <- T_honor_map_to_flag_inner(
            datasets, api_field[["elements"]][[name]], elem[[i]], group_field_to_dataset_map, name
          )
          res[["actions"]] <- append(res[["actions"]], subres[["actions"]])
          group_field_to_dataset_map <- subres[["map"]] # carry mappings defined inside this group
        }
      }

      res[["map"]] <- field_to_dataset_map # pop old mapping
    } else if (api_field$kind == "dataset_name") {
      res[["map"]][[current_field_name]] <- elem
    } else if (api_field$kind == "col") {
      map_to <- attr(api_field$sub_kind, "map_to")
      if (!is.null(map_to)) {
        dataset <- field_to_dataset_map[[api_field$dataset_name]]
        if (is.null(dataset)) stop("Column refers to unknown dataset") # TODO: Check this upstream, warn earlier
        res[["actions"]][[length(res[["actions"]]) + 1]] <- list(dataset = dataset, col = elem, kind = map_to)
      }
    }

    return(res)
  }

  T_do_map <- function(datasets, action) {
    dataset <- action[["dataset"]]
    col <- action[["col"]]
    kind <- action[["kind"]]

    col_data <- datasets[[dataset]][[col]]
    if (!T_is_of_kind(col_data, kind)) {
      mapped_from <- attr(col_data, "mapped_from")
      if (!is.null(mapped_from)) {
        stop(sprintf(
          "Dataset %s column %s has already been mapped from %s to %s",
          dataset, col, mapped_from, T_get_type_as_text(kind)
        ))
      }

      mapped_from <- class(col_data)

      attrs <- attributes(col_data)
      if (kind == "logical" && T_is_of_kind(col_data, T_YN())) {
        col_data <- (col_data == "Y")
      } else {
        kind_s <- T_get_type_as_text(kind)
        stop(sprintf("Can't map data from type %s to %s", paste(mapped_from, collapse = ", "), kind_s))
      }

      attributes(col_data) <- attrs
      attr(col_data, "mapped_from") <- mapped_from
    }

    return(col_data)
  }

  T_honor_map_to_flag <- function(datasets, mod_API, args) {
    # NOTE: Here we overwrite affected dataset columns with the desired type for the purpose of
    #       a particular argument. A 'Y/N' field will be cast to `logical` an thus will become
    #       unavailable as a character variable.
    #       Ideally we would like to cast dataset columns to separate columns with a different
    #       name and overwrite args to point to those new columns, which would sidestep that
    #       restriction. This, however, would entail modifying the argument list in reactive
    #       time depending on the contents of the dataset, which would force mod_*_server to
    #       treat column name arguments as reactives. That seems too much of a hassle for little
    #       benefit.
    env_that_called_the_module_function <- parent.frame(2)
    args <- T_eval_args(args, eval_env = env_that_called_the_module_function)

    mapping_actions <- T_honor_map_to_flag_inner(datasets, mod_API, args,
      field_to_dataset_map = list(),
      current_field_name = "<module_API>"
    )[["actions"]]

    for (action in mapping_actions) {
      dataset <- action[["dataset"]]
      col <- action[["col"]]
      datasets[[dataset]][[col]] <- T_do_map(datasets, action)
    }

    return(datasets)
  }

  list(
    logical = T_logical,
    factor = T_factor,
    character = T_character,
    date = T_date,
    datetime = T_datetime,
    integer = T_integer,
    numeric = T_numeric,
    anything = T_anything,
    or = T_or,
    group = T_group,
    mod_ID = T_mod_ID,
    dataset_name = T_dataset_name,
    col = T_col,
    color = T_color,
    CDISC_study_day = T_CDISC_study_day,
    YN = T_YN,
    choice_from_col_contents = T_choice_from_col_contents,
    choice = T_choice,
    fn = T_fn,
    is_of_kind = T_is_of_kind,
    flag = T_flag,
    map_to = T_map_to,
    attach_docs = T_attach_docs,
    honor_as_array_flag_inner = T_honor_as_array_flag_inner,
    honor_as_array_flag = T_honor_as_array_flag,
    honor_map_to_flag_inner = T_honor_map_to_flag_inner,
    honor_map_to_flag = T_honor_map_to_flag,
    get_type_as_text = T_get_type_as_text
  )
})
