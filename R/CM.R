# YT#VHf6c40bb7738a4549da708e6cffa92411#VHa84423515cdb57d0fffac288f003e279#
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

  app_creator_feedback_server <- function(id, warning_messages, error_messages, preface) {
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
              message_well("Module configuration errors", {
                tmp <- Map(function(x) htmltools::p(htmltools::HTML(paste("\u2022", x))), err)
                if (!is.null(preface)) {
                  tmp <- append(list(htmltools::p(htmltools::HTML(preface))), tmp)
                }
                tmp
              }, color = "#f4d7d7")
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

  # Wrap the UI and server of a module so that, once parameterized, they:
  # - go through a check function prior to running
  # - provide `dataset_info` to module manager
  # - transform afmm arbitrarily to allow simplifying the logic of the module (e.g. mapping character columns to factors)
  module <- function(module, check_mod_fn, dataset_info_fn, map_afmm_fn = NULL) {
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
      error_message_dataset_index <- NULL
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
              for (i_dataset in seq_along(afmm[["data"]])) {
                check_args <- append(
                  list(
                    afmm = afmm, # To check receiver_ids, among others
                    datasets = afmm[["data"]][[i_dataset]] # Allows data checks prior to reactive time
                  ),
                  args
                )
                res <- do.call(check_mod_fn, check_args)
                # NOTE: Stop when errors are found on a single dataset to avoid overwhelming users with repeat messages
                if (length(res[["errors"]])) { # NOTE: Not checking "warnings" because they are going away soon
                  error_message_dataset_index <- i_dataset
                  break
                }
              }
            }
            
            if (!is.null(error_message_dataset_index) && length(afmm[["data"]]) > 1) {
              dataset_name <- names(afmm[["data"]])[[error_message_dataset_index]]
              res[["preface"]] <- paste(
                "This application has been configured with more than one dataset.",
                sprintf("The following error messages apply to the dataset named <b>%s</b>.<br>", dataset_name),
                "No error checking has been performed on datasets specified after it. <hr>"
              )
            }
            return(res)
          })

          app_creator_feedback_server(
            id = module_id, warning_messages = fb[["warnings"]], error_messages = fb[["errors"]], 
            preface = fb[["preface"]]
          )

          if (length(fb[["errors"]]) == 0) {
            if (!is.null(map_afmm_fn)) {
              afmm_and_args <- append(list(afmm = afmm), args)
              afmm <- do.call(map_afmm_fn, afmm_and_args)
            }
            
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

  style_code <- function(code) {
    s <- paste(code, collapse = "")
    s <- parse(text = s, keep.source = FALSE)[[1]] |>
      deparse(width.cutoff = 100) |>
      trimws("right") |>
      paste(collapse = "\n")
    return(s)
  }

  # NOTE: For the moment call by running: devtools::load_all(); CM$generate_check_functions()
  generate_check_functions <- function(specs = module_specifications, output_file = "R/check_call_auto.R") {
    # TODO: Fuse with generate_map_afmm_functions
    styler_off <- "({\n# styler: off"
    styler_on <- "\n\n})\n# styler: on\n"

    res <- c("# Automatically generated module API check functions. Think twice before editing them manually.\n")
    res <- c(res, styler_off)

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
  
  generate_map_afmm_function <- function(spec, module_name) {
    stopifnot(spec$kind == "group")
   
    # TODO: At the time of writing, this code generator is only used by dv.explorer.parameter and it covers its needs.
    #       It modifies afmm[["filtered_dataset"]] based on parameters flagged as "map_character_to_factor"
    #       so that specific columns of target datasets are transformed to factors prior to going into a module.
    #
    #       In order to complete this functionality, we would have to map afmm[["unfiltered_dataset"]] as well
    #       as afmm[["data"]]. Moreover, we would have to look for "map_character_to_factor" flags inside possibly 
    #       nested column definitions, such as those used in papo.
    
    res <- character(0)
    
    push <- function(s) res <<- c(res, s)
    push("function(afmm, ")
    param_names <- paste(names(spec$elements), collapse = ",")
    push(param_names)
    push("){\n")
    
    push("res <- afmm\n")
    
    elements_that_require_mapping <- character(0)
    for (elem_name in names(spec$elements))
      if (isTRUE(attr(spec$elements[[elem_name]], "map_character_to_factor")))
        elements_that_require_mapping <- c(elements_that_require_mapping, elem_name)
    
    if (length(elements_that_require_mapping)) {
      push("mapping_summary <- character(0)\n")
      push("for(ds_name in names(afmm[['data']])){\n")
      push("  ds <- afmm[['data']][[ds_name]]\n")  
      for (elem_name in elements_that_require_mapping){
        elem <- spec$elements[[elem_name]]
        stopifnot(elem$kind == "col")
        dataset_name <- elem[["dataset_name"]]
        push(sprintf("if(is.character(ds[[%s]][[%s]])){\n", dataset_name, elem_name))
        push("mapping_summary <- c(mapping_summary,")
        push(sprintf("paste0('(', ds_name, ') ', %s, '[[\"', %s, '\"]]')", dataset_name, elem_name))
        push(")\n")
        push("}\n")
      }
      push("}\n")
      
      push("if(length(mapping_summary)){\n")
     
      push(
        paste0(
          "warning_message <- paste0('[", module_name,
          "] This module will map the following dataset columns from `character` to `factor`:\\n', ",
          "paste(mapping_summary, collapse = ', '), '.\\nThe extra memory cost associated to this operation can be ",
          "avoided by turning those columns into factors during data pre-processing.')\n",
          "warning(warning_message)\n"
        )
      )
      
      push("res[['filtered_dataset']] <- shiny::reactive({\n")
      push("  res <- afmm[['filtered_dataset']]()\n")
      
      for (elem_name in elements_that_require_mapping){
        elem <- spec$elements[[elem_name]]
        dataset_name <- elem[["dataset_name"]]
        
        push(sprintf("if (is.character(res[[%s]][[%s]])) {\n", dataset_name, elem_name))
        push(sprintf("  res[[%s]][[%s]] <- ", dataset_name, elem_name))
        push(sprintf("    as.factor(res[[%s]][[%s]])\n", dataset_name, elem_name))
        push("}\n")
      }
      
      push("  return(res)\n")
      push("})\n")
      push("}\n")
    }
    
    push("return(res)\n")
    push("}\n")

    return(res)
  }

  # NOTE: For the moment, call by running: devtools::load_all(); CM$generate_map_afmm_functions()
  generate_map_afmm_functions <- function(specs = module_specifications, output_file = "R/map_afmm_auto.R") {
    # TODO: Fuse with generate_check_functions
    styler_off <- "({\n# styler: off"
    styler_on <- "\n\n})\n# styler: on\n"

    res <- c("# Automatically generated module API afmm mapping functions. Think twice before editing them manually.\n")
    res <- c(res, styler_off)

    for (spec_name in names(specs)) {
      if (!grepl("::", spec_name, fixed = TRUE)) stop(paste("Expected API spec name to be namespaced (`::`):", spec_name))
      denamespaced_spec_name <- strsplit(spec_name, "::")[[1]][[2]]
      map_afmm_function_name <- paste0("map_afmm_", denamespaced_spec_name, "_auto")
      res <- c(res, sprintf("\n\n# %s\n", spec_name))
      
      res <- c(
        res,
        c(map_afmm_function_name, "<-", 
          generate_map_afmm_function(specs[[spec_name]], module_name = denamespaced_spec_name)) |> style_code()
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

    ok <- assert(err, is.character(value), 
                 paste(sprintf("The value assigned to parameter `%s` should be of type `character`", name),
                       sprintf("and it's instead of type `%s`.", class(value)[[1]])))
    
    valid_column_names <- list_columns_of_kind(dataset_value, subkind)
    invalid_column_names <- value[!value %in% valid_column_names]
    wrong_subkind_column_names <- invalid_column_names[invalid_column_names %in% names(dataset_value)]

    ok <- ok && assert(
      err, length(wrong_subkind_column_names) == 0, {
        cnames <- paste(sprintf('"%s"', wrong_subkind_column_names), collapse = ", ")
        type_desc <- TC$get_type_as_text(subkind)
        types_found <- unname(sapply(dataset_value[wrong_subkind_column_names], function(x) class(x)[[1]]))
        types_found_desc <- paste(sprintf("`%s`", types_found), collapse = ", ")
        paste(
          sprintf("Variables assigned to parameter <b>`%s`</b> should refer to columns of dataset <b>`%s`</b>",
                  name, dataset_name),
          sprintf("of type `%s`, but some (<b>%s</b>) have other types (%s).", 
                  type_desc, cnames, types_found_desc)
        )
      }
    )
    
    ok <- ok && assert(
      err, length(invalid_column_names) == 0, {
        cnames <- paste(sprintf('"%s"', invalid_column_names), collapse = ", ")
        paste(
          sprintf("The value of parameter <b>`%s`</b> includes one or more variables (<b>%s</b>)", name, cnames),
          sprintf("that are not columns of the <b>`%s`</b> dataset.", dataset_name)
        )
      }
    )

    zero_or_more <- isTRUE(flags[["zero_or_more"]])
    one_or_more <- isTRUE(flags[["one_or_more"]])
    zero_or_one_or_more <- zero_or_more || one_or_more
    if (zero_or_one_or_more) {
      min_len <- 0
      if (one_or_more) min_len <- 1
      
      ok <- ok && assert(
        err,
        length(value) >= min_len, {
          col_names <- paste(sprintf('"%s"', valid_column_names), collapse = ", ")
          paste0(
            sprintf("`%s` should be a character vector of length greater than %s ", name, c("zero", "one")[[min_len + 1]]),
            sprintf("referring to the following columns of dataset `%s`: ", dataset_name),
            col_names, "."
          )
        }
      )
    } else {
      ok <- ok && assert(
        err,
        length(value) == 1,
        paste(
          sprintf("`%s` should be a string referring to a single column of dataset `%s`: ", name, dataset_name),
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
        err, is.null(value) || all(value %in% dataset_value[[column]]),
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
    paste0("<code style='white-space: pre; color:#333'>", s, "</code>")
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
    dup_mask <- duplicated(unique_cat_par_combinations[par])
    unique_repeat_params <- unique_cat_par_combinations[[par]][dup_mask]
    
    ok <- assert(err, length(unique_repeat_params) == 0, {
      dups <- df_to_string(
        data.frame(
          check.names = FALSE,
          Parameter = unique_repeat_params,
          "Inside categories" = sapply(
            unique_repeat_params, function(param) {
              dup_mask <- (unique_cat_par_combinations[[par]] == param)
              return(paste(unique_cat_par_combinations[dup_mask, ][[cat]], collapse = ", "))
            }
          )
        )
      )
      prefix_repeat_params_command <- 
        sprintf('%s <- dv.explorer.parameter::prefix_repeat_parameters(%s, cat_var = "%s", par_var = "%s")', 
                ds_value, ds_value, cat, par)

      mask <- unique_cat_par_combinations[["PARAM"]] %in% unique_repeat_params
      deduplicated_table <- df_to_string({
        cats <- unique_cat_par_combinations[mask, ][[cat]]
        pars <- unique_cat_par_combinations[mask, ][[par]]
        data.frame(
          check.names = FALSE,
          Category = cats, "Old parameter name" = pars, "New parameter name" = paste0(cats, "-", pars)
        )
      })

      paste0(
        sprintf('The dataset provided by %s ("%s") contains parameter names that repeat across categories.', 
                format_inline_asis(ds_name), ds_value),
        "This module expects them to be unique. This is the list of duplicates:",
        paste0("<pre>", dups, "</pre>"),
        "In order to bypass this issue, we suggest you preprocess that dataset with this command:",
        paste0("<pre>", prefix_repeat_params_command, "</pre>"),
        sprintf('<small><i>In case the dataset labeled as "%s" has a different name in your application code, ', ds_value),
        "substitute it with the actual name of the variable holding that dataset.</i></small><br>",
        "The ", format_inline_asis("dv.explorer.parameter::prefix_repeat_parameters"), " function ",
        "will rename the repeat parameters by prefixing them with the category they belong to, as shown on this table:",
        "<pre>", deduplicated_table, "</pre>"
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
    generate_map_afmm_functions = generate_map_afmm_functions,
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
