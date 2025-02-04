# YT#VH0ae1b0c3bf862b3b93194fa0f023686d#VHd74cfd54b905c92b95c251b87af2e842#
CM <- local({ # _C_hecked _M_odule
  message_well <- function(title, contents, color = "f5f5f5") { # repeats #iewahg
    style <- sprintf(r"---(
      padding: 0.5rem;
      padding-left: 1rem;
      margin-bottom: 20px;
      background-color: %s;
      border: 1px solid #e3e3e3;
      border-radius: 4px;
      -webkit-box-shadow: inset 0 1px 1px rgba(0,0,0,.05);
      box-shadow: inset 0 1px 1px rgba(0,0,0,.05);
    )---", color)

    res <- list(shiny::h3(title))
    if (length(contents)) res <- append(res, list(shiny::tags[["div"]](contents, style = style)))
    return(res)
  }

  app_creator_feedback_ui <- function(id, ui) {
    id <- paste(c(id, "validator"), collapse = "-")
    ns <- shiny::NS(id)
    
    hide <- function(e) shiny::tags[["div"]](e, style = "display: none")
    
    res <- list(
      shiny::uiOutput(ns("ui")),
      hide(shiny::checkboxInput(inputId = ns("show_ui"), label = NULL)),
      shiny::conditionalPanel(condition = "input.show_ui == true", ui, ns = ns)
    )
    return(res)
  }

  app_creator_feedback_server <- function(id, warning_messages, error_messages) {
    id <- paste(c(id, "validator"), collapse = "-")
    module <- shiny::moduleServer(
      id,
      function(input, output, session) {
        output[["ui"]] <- shiny::renderUI({
          res <- list()
          warn <- warning_messages
          if (length(warn)) {
            res[[length(res) + 1]] <-
              message_well("Module configuration warnings",
                Map(function(x) htmltools::p(htmltools::HTML(paste("\u2022", x))), warn),
                color = "#fff7ef"
              )
          }

          err <- error_messages
          if (length(err)) {
            res[[length(res) + 1]] <-
              message_well("Module configuration errors",
                Map(function(x) htmltools::p(htmltools::HTML(paste("\u2022", x))), err),
                color = "#f4d7d7"
              )
          }

          return(res)
        })
        shiny::outputOptions(output, "ui", suspendWhenHidden = FALSE)
        
        if (length(error_messages) == 0) {
          shiny::updateCheckboxInput(inputId = "show_ui", value = TRUE)
        }
      }
    )

    return(module)
  }

  # Wrap the UI and server of a module so that, once parameterized, they go through a check function prior to running.
  module <- function(module, check_mod_fn, dataset_info_fn) {
    local({
      # Make sure that the signature of `check_mod_fn` matches that of `module` except for the expected differences
      check_formals <- names(formals(check_mod_fn))
      if (!identical(head(check_formals, 2), c("afmm", "datasets"))) {
        stop("The first two arguments of check functions passed onto `module` should be `afmm` and `datasets`")
      }
      check_formals <- check_formals[c(-1, -2)]

      mod_formals <- names(formals(module))
      if (!identical(check_formals, mod_formals)) {
        stop(paste(
          "Check function arguments do not exactly match those of the module function",
          "(after discarding `afmm` and `datasets`)"
        ))
      }
    })

    mandatory_module_args <- local({
      args <- formals(module)
      names(args)[sapply(args, function(x) is.name(x) && nchar(x) == 0)]
    })

    wrapper <- function(...) {
      # Match arguments explicitly to provide graphical error feedback
      # https://cran.r-project.org/doc/manuals/r-release/R-lang.html#Argument-matching

      module_ui <- function(...) list()
      module_server <- function(...) NULL
      module_id <- "error_id"

      matched_args <- try(as.list(match.call(module)), silent = TRUE)
      error_message <- attr(matched_args, "condition")$message
      if (is.null(error_message)) {
        missing_args <- setdiff(mandatory_module_args, names(matched_args))
        if (length(missing_args)) {
          error_message <- sprintf("Missing mandatory arguments: `%s`.", paste(missing_args, collapse = ", "))
        }
      }

      if (is.null(error_message)) {
        args <- list(...)
        evaluated_module <- do.call(module, args)
        module_ui <- evaluated_module[["ui"]]
        module_server <- evaluated_module[["server"]]
        module_id <- evaluated_module[["module_id"]]
      }

      res <- list(
        ui = function(module_id) app_creator_feedback_ui(module_id, module_ui(module_id)), # `module` UI gated by app_creator_feedback_server
        server = function(afmm) {
          fb <- local({
            res <- NULL
            if (!is.null(error_message)) {
              res <- list(
                warnings = character(0),
                errors = error_message
              )
            } else {
              # NOTE: We check the call here and not inside the module server function because:
              #       - app creators interact with the davinci module and not with the ui-server combo, so
              #         errors reported with respect to the module signature will make sense to them.
              #         The module server function might use a different function signature.
              #       - Here we also have access to the original datasets, which allows us to ensure call
              #         correctness independent of filter state or operation in a single pass.
              #       - "catch errors early"

              args <- append(
                list(
                  afmm = afmm, # To check receiver_ids, among others
                  datasets = afmm[["data"]][[1]] # Allows data checks prior to reactive time
                ),
                args
              )

              # check functions do not have defaults, so we extract them from the formals of the module for consistency
              missing_args <- setdiff(names(formals(module)), names(args))
              res <- do.call(check_mod_fn, args)
            }
            return(res)
          })

          app_creator_feedback_server(
            id = module_id, warning_messages = fb[["warnings"]], error_messages = fb[["errors"]]
          )

          # TODO: Modify afmm to the `map_to` flags in the API. `dv.papo` relies on this
          # nolint start
          if (FALSE) {
            filtered_mapped_datasets <- shiny::reactive(
              TC$honor_map_to_flag(afmm$filtered_dataset(), mod_lineplot_API, args)
            )

            bm_dataset <- shiny::reactive({
              shiny::req(bm_dataset_name)
              ds <- filtered_mapped_datasets()[[bm_dataset_name]]
              shiny::validate(
                shiny::need(!is.null(ds), paste("Could not find dataset", bm_dataset_name))
              )
              return(ds)
            })

            # TODO:
            corr_hm_server(
              id = module_id,
              bm_dataset = bm_dataset,
              default_value = default_value, subjid_var = subjid_var, cat_var = cat_var, par_var = par_var,
              visit_var = visit_var, value_vars = value_vars
            )
          }
          # nolint end

          if (length(fb[["errors"]]) == 0) {
            res <- try(module_server(afmm), silent = TRUE)
          }

          return(res)
        },
        module_id = module_id,
        meta = list(
          dataset_info = {
            # extract defaults from the formals for consistency
            missing_args <- setdiff(names(formals(module)), names(matched_args))
            args <- c(args, formals(module)[missing_args])
            do.call(dataset_info_fn, args)
          }
        )
      )

      return(res)
    }

    roxygen_wrapper <- function() { # to keep parameters in the reference docs
      args <- (match.call() |> as.list())[c(-1)]
      do.call(wrapper, args, env = parent.frame())
    }
    formals(roxygen_wrapper) <- formals(module)
    return(roxygen_wrapper)
  }

  container <- function() list2env(x = list(messages = character(0)), parent = emptyenv())
  assert <- function(container, cond, msg) {
    ok <- isTRUE(cond)
    if (!ok) container[["messages"]] <- c(container[["messages"]], msg)
    return(ok)
  }

  is_valid_shiny_id <- function(s) grepl("^$|^[a-zA-Z][a-zA-Z0-9_-]*$", s)

  generate_check_function <- function(spec) {
    stopifnot(spec$kind == "group")

    # TODO: Check that arguments that depend on arguments TC$flagged as `optional` are optional too.

    res <- character(0)
    push <- function(s) res <<- c(res, s)
    push("function(afmm, datasets,")
    param_names <- paste(names(spec$elements), collapse = ",")
    push(param_names)
    push(", warn, err){\n")

    push("OK <- logical(0)\n")
    push("used_dataset_names <- new.env(parent = emptyenv())\n")

    subjid_vars <- character(0)

    for (elem_name in names(spec$elements)) {
      elem <- spec$elements[[elem_name]]
      attrs_ids <- setdiff(names(attributes(elem)), c("names", "docs"))
      attrs <- attributes(elem)[attrs_ids]

      if (isTRUE(attrs[["subjid_var"]])) {
        subjid_vars <- c(subjid_vars, elem_name)
      }

      if (elem$kind == "mod") {
        push(sprintf("OK[['%s']] <- CM$check_module_id('%s', %s, warn, err)\n", elem_name, elem_name, elem_name))
      } else if (elem$kind == "dataset_name") {
        push(sprintf("flags <- %s\n", deparse(attrs) |> paste(collapse = "")))
        push(sprintf(
          "OK[['%s']] <- CM$check_dataset_name('%s', %s, flags, datasets, used_dataset_names, warn, err)\n",
          elem_name, elem_name, elem_name
        ))
      } else if (elem$kind == "col") {
        push(sprintf("subkind <- %s\n", deparse(elem$sub_kind) |> paste(collapse = "")))
        push(sprintf("flags <- %s\n", deparse(attrs) |> paste(collapse = "")))
        push(sprintf(
          "OK[['%s']] <- OK[['%s']] && CM$check_dataset_colum_name('%s', %s, subkind, flags, %s, datasets[[%s]], warn, err)\n",
          elem_name, elem$dataset_name, elem_name, elem_name, elem$dataset_name, elem$dataset_name
        ))
      } else if (elem$kind == "choice_from_col_contents") {
        dataset_param_name <- spec$elements[[elem$param]]$dataset_name
        push(sprintf("flags <- %s\n", deparse(attrs) |> paste(collapse = "")))
        push(sprintf(
          "OK[['%s']] <- OK[['%s']] && CM$check_choice_from_col_contents('%s', %s, flags, '%s', datasets[[%s]], %s, warn, err)\n",
          elem_name, elem$param, elem_name, elem_name, dataset_param_name, dataset_param_name, elem$param
        ))
      } else if (elem$kind == "choice") {
        push(sprintf("flags <- %s\n", deparse(attrs) |> paste(collapse = "")))
        push(sprintf(
          "OK[['%s']] <- OK[['%s']] && CM$check_choice('%s', %s, flags, '%s', %s, warn, err)\n",
          elem_name, elem$param, elem_name, elem_name, elem$param, elem$param
        ))
      } else if (elem$kind == "function") {
        push(sprintf("flags <- %s\n", deparse(attrs) |> paste(collapse = "")))
        push(sprintf(
          "OK[['%s']] <- CM$check_function('%s', %s, %d, flags, warn, err)\n",
          elem_name, elem_name, elem_name, elem$arg_count
        ))
      } else {
        push(sprintf("'NOTE: %s (%s) has no associated automated checks'\n", elem_name, elem$kind))
        push(sprintf("'      The expectation is that it either does not require them or that'\n"))
        push(sprintf("'      the caller of this function has written manual checks near the call site.'\n"))
      }
    }

    if (length(subjid_vars) > 1) {
      stop(sprintf("This API specifies more than one subjid variable: ", paste(subjid_vars, collapse = ", ")))
    }

    if (length(subjid_vars) == 1) {
      subjid_var <- subjid_vars[[1]]
      push("for(ds_name in names(used_dataset_names)){\n")
      push(sprintf(
        "OK[['%s']] <- OK[['%s']] && CM$check_subjid_col(datasets, ds_name, get(ds_name), '%s', %s, warn, err)",
        subjid_var, subjid_var, subjid_var, subjid_var
      ))
      push("}\n")
      # TODO: If there is a dataset flagged as `subject_level_dataset_name`:
      #       [ ] check that subjid_var is unique
      #       [ ] check that the subjid_var values of all other datasets are a subset of its values
    }

    push(sprintf("return(OK)\n"))

    push("}\n")

    return(res)
  }

  # NOTE: For the moment call by running: devtools::load_all(); CM$generate_check_functions()
  generate_check_functions <- function(specs = module_specifications, output_file = "R/check_call_auto.R") {
    styler_off <- "({\n# styler: off"
    styler_on <- "\n\n})\n# styler: on\n"

    res <- c("# Automatically generated module API check functions. Think twice before editing them manually.\n")
    res <- c(res, styler_off)

    style_code <- function(code) {
      s <- paste(code, collapse = "")
      s <- parse(text = s, keep.source = FALSE)[[1]] |>
        deparse(width.cutoff = 100) |>
        trimws("right") |>
        paste(collapse = "\n")
      return(s)
    }

    for (spec_name in names(specs)) {
      if (!grepl("::", spec_name, fixed = TRUE)) stop(paste("Expected API spec name to be namespaced (`::`):", spec_name))
      denamespaced_spec_name <- strsplit(spec_name, "::")[[1]][[2]]
      check_function_name <- paste0("check_", denamespaced_spec_name, "_auto")
      res <- c(res, sprintf("\n\n# %s\n", spec_name))
      res <- c(
        res,
        c(check_function_name, "<-", generate_check_function(specs[[spec_name]])) |> style_code()
      )
    }

    res <- c(res, styler_on)

    contents <- paste(res, collapse = "")
    writeChar(contents, output_file, eos = NULL)

    return(NULL)
  }

  test_string <- function(s) {
    is.character(s) && length(s) == 1
  }

  check_module_id <- function(name, value, warn, err) {
    assert(err, test_string(value), sprintf("`%s` should be a string", name)) &&
      assert(warn, nchar(value) > 0, sprintf("Consider providing a non-empty `%s`.", name)) &&
      assert(
        err,
        is_valid_shiny_id(value),
        paste(
          sprintf("`%s` should be a valid identifier, starting with a letter and followed by", name),
          "alphanumeric characters, hyphens and underscores."
        )
      )
  }

  check_dataset_name <- function(name, value, flags, available_datasets, used_dataset_names, warn, err) {
    ok <- check_flags(name, value, flags, warn, err)

    if (ok) {
      zero_or_more <- isTRUE(flags[["zero_or_more"]])
      one_or_more <- isTRUE(flags[["one_or_more"]])
      zero_or_one_or_more <- zero_or_more || one_or_more
      if (zero_or_one_or_more) {
        min_len <- 0
        if (one_or_more) min_len <- 1
        ok <- assert(
          err,
          is.character(value) &&
            all(value %in% names(available_datasets)) &&
            length(value) >= min_len,
          paste(
            sprintf(
              "`%s` should be a character vector of length greater than %s referring to the following dataset names: ",
              name, c("zero", "one")[[min_len + 1]]
            ),
            paste(sprintf('"%s"', names(available_datasets)), collapse = ", "), "."
          )
        )
      } else {
        ok <- (
          assert(err, !missing(value), sprintf("`%s` missing", name)) && # TODO: ? Remove this one
            assert(
              err,
              test_string(value) &&
                value %in% names(available_datasets),
              paste(
                sprintf("`%s` should be a string referring to one of the available dataset names: ", name),
                paste(sprintf('"%s"', names(available_datasets)), collapse = ", "), "."
              )
            )
        )
        if (ok) used_dataset_names[[name]] <- value
      }
    }
    return(ok)
  }

  list_columns_of_kind <- function(dataset, type) {
    res <- names(dataset)[sapply(seq_len(ncol(dataset)), function(x) TC$is_of_kind(dataset[[x]], type))]
    return(res)
  }

  # TODO: use check_flags instead and remove
  optional_and_empty <- function(flags, value) {
    return(isTRUE(flags[["optional"]]) && length(value) == 0)
  }

  check_dataset_colum_name <- function(name, value, subkind, flags, dataset_name, dataset_value, warn, err) {
    if (optional_and_empty(flags, value)) {
      return(TRUE)
    }

    ok <- FALSE

    valid_column_names <- list_columns_of_kind(dataset_value, subkind)

    zero_or_more <- isTRUE(flags[["zero_or_more"]])
    one_or_more <- isTRUE(flags[["one_or_more"]])
    zero_or_one_or_more <- zero_or_more || one_or_more
    if (zero_or_one_or_more) {
      min_len <- 0
      if (one_or_more) min_len <- 1
      ok <- assert(
        err,
        is.character(value) &&
          all(value %in% valid_column_names) &&
          length(value) >= min_len,
        paste(
          sprintf(
            "`%s` should be a character vector of length greater than %s referring to one of the following columns of dataset `%s`: ",
            name, c("zero", "one")[[min_len + 1]], dataset_name
          ),
          paste(sprintf('"%s"', valid_column_names), collapse = ", "), "."
        )
      )
    } else {
      ok <- assert(
        err,
        test_string(value) &&
          all(value %in% valid_column_names),
        paste(
          sprintf("`%s` should be a string referring to one of the following columns of dataset `%s`: ", name, dataset_name),
          paste(sprintf('"%s"', valid_column_names), collapse = ", "), "."
        )
      )
    }
    return(ok)
  }

  list_values <- function(v) {
    res <- ""
    if (is.factor(v)) {
      res <- sprintf('"%s"', levels(v))
    } else if (is.character(v)) {
      res <- sprintf('"%s"', unique(v))
    } else {
      browser()
    }

    res <- paste(res, collapse = ", ")

    return(res)
  }

  check_flags <- function(name, value, flags, warn, err) {
    ok <- FALSE
    min_len <- max_len <- 1L
    if (isTRUE(flags[["optional"]]) && is.null(value)) {
      ok <- TRUE
    } else {
      if (isTRUE(flags[["zero_or_more"]])) {
        min_len <- 0L
        max_len <- +Inf
      } else if (isTRUE(flags[["one_or_more"]])) {
        min_len <- 1L
        max_len <- +Inf
      }

      ok <- assert(
        err, min_len <= length(value) && length(value) <= max_len,
        ifelse(min_len < max_len,
          sprintf(
            "`%s` has length %s but should have length in the range [%s, %s].",
            name, length(value), min_len, max_len
          ),
          sprintf(
            "`%s` has length %s but should have length %s.",
            name, length(value), min_len
          )
        )
      )
    }

    if (ok && isTRUE(flags[["named"]])) {
      ok <- assert(
        err, length(value) == length(names(value)) && all(nchar(names(value)) > 0),
        sprintf("All elements of `%s` should be named", name)
      )
    }

    return(ok)
  }

  check_choice_from_col_contents <- function(name, value, flags, dataset_name, dataset_value, column, warn, err) {
    ok <- check_flags(name, value, flags, warn, err) &&
      assert(
        err, all(value %in% dataset_value[[column]]),
        sprintf(
          "`%s` should contain only values present in column `%s` of dataset `%s`: %s.",
          name, column, dataset_name, list_values(dataset_value[[column]])
        )
      )

    return(ok)
  }

  check_choice <- function(name, value, flags, values_name, values, warn, err) {
    ok <- check_flags(name, value, flags, warn, err) &&
      assert(
        err, all(value %in% values),
        sprintf(
          "`%s` should contain only the following values: %s.",
          name, list_values(values)
        )
      )

    return(ok)
  }

  format_inline_asis <- function(s) {
    paste("<code style='white-space: pre; color:#333'>", s, "</code>")
  }

  check_function <- function(name, value, arg_count, flags, warn, err) {
    ok <- check_flags(name, value, flags, warn, err)
    if (ok) {
      if (is.function(value)) {
        value <- list(value) # make single functions behave like vectors of one element, for simplicity
      }

      for (i in seq_along(value)) {
        f <- value[[i]]
        ok <- ok && assert(
          err, is.function(f) && length(formals(f)) == arg_count,
          sprintf("`%s[[%d]]` should be a function of %d arguments", name, i, arg_count)
        )
      }
    }

    return(ok)
  }

  check_subjid_col <- function(datasets, ds_name, ds_value, col_name, col_var, warn, err) {
    ok <- assert(
      err, col_var %in% names(datasets[[ds_value]]),
      sprintf(
        "Expected `%s` value (%s) to be present in the dataset indicated by name `%s` (%s)",
        col_name, col_var, ds_name, ds_value
      )
    )
    return(ok)
  }

  check_unique_sub_cat_par_vis <- function(datasets, ds_name, ds_value, sub, cat, par, vis, warn, err) {
    ok <- TRUE

    df_to_string <- function(df) {
      names(df) <- sprintf("[%s] ", names(df))
      lines <- capture.output(print(as.data.frame(df), right = FALSE, row.names = FALSE, quote = TRUE)) |> trimws()
      return(paste(lines, collapse = "\n"))
    }

    dataset <- datasets[[ds_value]]

    unique_cat_par_combinations <- unique(dataset[c(cat, par)])
    dup_params_across_categories <- duplicated(unique_cat_par_combinations[par])

    ok <- assert(err, !any(dup_params_across_categories), {
      prefixes <- c(rep("Category:", length(cat)), rep("Parameter:", length(par)))
      first_duplicates <- head(unique_cat_par_combinations[dup_params_across_categories, ], 5)

      names(first_duplicates) <- paste(prefixes, names(first_duplicates))
      dups <- df_to_string(first_duplicates)
      paste(
        sprintf("The dataset provided by `%s` (%s) contains parameter names that repeat across categories.", ds_name, ds_value),
        "This module expects them to be unique. Here are the first few duplicates:",
        paste0("<pre>", dups, "</pre>")
      )
    })

    supposedly_unique <- dataset[c(sub, cat, par, vis)]
    dups <- duplicated(supposedly_unique)

    ok <- ok && assert(err, !any(dups), {
      prefixes <- c(
        rep("Subject:", length(sub)), rep("Category:", length(cat)),
        rep("Parameter:", length(par)), rep("Visit:", length(vis))
      )

      first_duplicates <- head(supposedly_unique[dups, ], 5)
      names(first_duplicates) <- paste(prefixes, names(first_duplicates))
      dups <- df_to_string(first_duplicates)
      paste(
        sprintf("The dataset provided by `%s` (%s) contains repeated rows with identical subject, category, parameter", ds_name, ds_value),
        "and visit values. This module expects them to be unique. Here are the first few duplicates:",
        paste0("<pre>", dups, "</pre>")
      )
    })

    return(ok)
  }

  list(
    module = module,
    container = container,
    assert = assert,
    generate_check_functions = generate_check_functions,
    check_module_id = check_module_id,
    check_dataset_name = check_dataset_name,
    check_dataset_colum_name = check_dataset_colum_name,
    check_flags = check_flags,
    check_choice_from_col_contents = check_choice_from_col_contents,
    check_choice = check_choice,
    check_function = check_function,
    check_subjid_col = check_subjid_col,
    check_unique_sub_cat_par_vis = check_unique_sub_cat_par_vis,
    message_well = message_well
  )
})
