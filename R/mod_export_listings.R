# CONSTANTS ----
EXP <- pack_of_constants( #nolint
  EXPORT_WINDOW_TITLE = "Download options",
  EXPORTBTN_ID = "download_data",
  EXPORTBTN_LABEL = "Download",
  DOWNLOADBTN_ID = "download",
  DATASEL_ID = "which_data",
  DATASEL_LABEL = "What should be downloaded?",
  FILETYPE_ID = "file_type",
  FILETYPE_LABEL = "Select file type",
  FILENAME_ID = "name",
  FILENAME_LABEL = "Insert file name",
  REFCOL_ID = "ref",
  REFCOL_LABEL = "Select reference columns (optional)",
  REFCOL_INFO_ID = "ref_info",
  REFCOL_INFO_LABEL = "Reference columns are displayed on each page of the pdf.
  Zero, one or multiple columns can be selected.",
  SNAPSHOT_ID = "snapshot",
  SNAPSHOT_LABEL = "Enter footnote (optional)",
  SNAPSHOT_INFO_ID = "snapshot_info",
  SNAPSHOT_INFO_LABEL = "Additional information displayed within footnote of the pdf, e.g., snapshot name.",
  DATAPROTECT_ID = "check",
  DATAPROTECT_LABEL = paste(
    "I agree to the following:"
  ), # label is pasted to avoid test failure
  DATAPROTECT_WARN = "Check box needs to be checked.",
  WARN_COLOR = "#CC3333",
  EXP_TITLE = paste0(
    "Export produced by DaVinci's dv.listings module (version ", packageVersion("dv.listings"), "). "
  )
)


#' Create user interface for the export listings shiny module of \pkg{dv.listings}
#'
#' @param module_id `[character(1)]` A unique ID string to create a namespace. Must match the ID of
#'   \code{mod_exp_listings_server()}.
#'
#' @return A shiny \code{uiOutput} element.
#' @keywords internal
mod_export_listings_UI <- function(module_id) { #nolint

  # Check validity of arguments
  checkmate::assert_string(module_id, min.chars = 1)

  ns <- shiny::NS(module_id)

  ui <- shiny::tagList(
    shinyFeedback::useShinyFeedback(), # needed to use shinyFeedback functionalities
    shinyjs::useShinyjs(),             # needed to use shinyjs functionalities

    shiny::actionButton(ns(EXP$EXPORTBTN_ID), label = EXP$EXPORTBTN_LABEL)
  )

  return(ui)
}


#' Create server for export listings shiny module of \pkg{dv.listings}
#'
#' @param module_id `[character(1)]` A unique ID string to create a namespace. Must match the ID of
#' \code{mod_exp_listings_UI()}.
#'
#' @param dataset_metadata `[list(character(1), character(1+))]` A list with the following two elements:
#' \code{dataset_metadata$name()} containing a reactive string specifying the name of the selected
#' dataset and \code{dataset_metadata$date_range()} containing a reactive character vector with two entries
#' specifying the earliest and latest modification date in the dataset.
#' Usually obtained from module manager.
#'
#' @param dataset_list `[shiny::reactive(list(data.frame))]` A reactive list of named datasets.
#' Usually obtained from module manager.
#'
#' @param data `[shiny::reactive(list(data.frame, character(0+)))]` A reactive list with the
#' following two elements: \code{data()$data} containing a single data frame and \code{data()$col_names}
#' containing the column names of \code{data()$data}.
#'
#' @param data_selection_name `[shiny::reactive(character(1))]` A reactive string specifying the name of
#' \code{data()$data}.
#'
#' @param current_rows `[shiny::reactive(character(0+))]` A reactive integer vector containing a selection
#' of row numbers from \code{data()$data}.
#'
#' @param intended_use_label `[character(1) | NULL]` Either a character indicating the intended use for the download, or
#' NULL. If a label is provided it will be shown before the download and will also be included in the downloaded file.
#'
#' @keywords internal
mod_export_listings_server <- function(module_id,
                                       dataset_metadata,
                                       dataset_list,
                                       data,
                                       data_selection_name,
                                       current_rows,
                                       intended_use_label) {

  # check validity of parameters
  checkmate::assert(
    checkmate::check_string(module_id, min.chars = 1),
    checkmate::check_list(dataset_metadata, names = "named"),
    checkmate::check_character(names(dataset_metadata), unique = TRUE),
    checkmate::check_subset(names(dataset_metadata), choices = c("name", "date_range")),
    checkmate::check_multi_class(dataset_list, c("reactive", "shinymeta_reactive")),
    checkmate::check_multi_class(data, c("reactive", "shinymeta_reactive")),
    checkmate::check_string(shiny::isolate(data_selection_name()), null.ok = TRUE),
    checkmate::check_integer(shiny::isolate(current_rows()), null.ok = TRUE),
    checkmate::check_string(intended_use_label, null.ok = TRUE),
    combine = "and"
  )

  shiny::moduleServer(
    module_id,
    function(input, output, session) {

      v_dataset_list <- shiny::reactive({
        checkmate::assert_list(dataset_list(), types = "data.frame", null.ok = TRUE, names = "named")
        dataset_list()
      })

      ns <- session$ns

      # dataset validation
      v_data <- shiny::reactive({
        shiny::req(data())
        checkmate::assert(
          checkmate::check_list(data(), null.ok = TRUE, min.len = 2, names = "named"),
          checkmate::check_character(names(data()), unique = TRUE, null.ok = TRUE),
          checkmate::check_subset(names(data()), choices = c("data", "col_names")),
          checkmate::check_data_frame(data()$data, null.ok = TRUE),
          checkmate::check_character(data()$col_names, n.chars = dim(data())[2], null.ok = TRUE),
        )

        # return data after we checked that everything is fine
        list("df" = data()$data, "col_names" = data()$col_names)
      })

      # Determine currently displayed data (taking set filters into account)
      current_data <- shiny::reactive({

        if (is.null(current_rows())) {
          NULL
        } else {
          # subsetting using dplyr::filter() is needed to avoid attribute loss (in case of datasets as mtcars)
          v_data()$df %>% dplyr::filter(rownames(v_data()$df) %in% rownames(v_data()$df)[current_rows()])
        }
      })

      # Download modal
      shiny::observeEvent(input[[EXP$EXPORTBTN_ID]], {
        shiny::req(v_data()$col_names)

        if (is.null(current_data())) {
          shinyFeedback::showToast(
            "info", "There is no dataset displayed currently. This may arise due to your filter choices or
            because the dataset is still loading.",
            .options = list(positionClass = "toast-top-right")
          )
        } else {
          shiny::showModal(
            shiny::modalDialog(
              export_modal_content(
                ns = ns, file_name = dataset_metadata$name(),
                cond = paste0("input.", EXP$FILETYPE_ID, " === '.pdf'"), colnames = v_data()$col_names,
                activate_checkbox = !is.null(intended_use_label)
              ),
              title = EXP$EXPORT_WINDOW_TITLE,
              footer = list(shiny::fluidRow(
                shiny::column(5, shinyjs::disabled(shiny::downloadButton(ns(EXP$DOWNLOADBTN_ID))), offset = 5),
                shiny::column(1, shiny::modalButton("Cancel"))
              )),
              easyClose = TRUE
            )
          )
        }
      })

      if (!is.null(intended_use_label)) {
        # Add/remove checkbox warning
        checkbox_label <- shiny::eventReactive(input[[EXP$DATAPROTECT_ID]], {
          if (input[[EXP$DATAPROTECT_ID]]) {
            return(paste(EXP$DATAPROTECT_LABEL, intended_use_label))
          } else {
            return(shiny::tags$embed(
              paste(EXP$DATAPROTECT_LABEL, intended_use_label),
              shiny::br(shiny::tags$span(style = paste("color:", EXP$WARN_COLOR), EXP$DATAPROTECT_WARN))
            ))
          }
        })

        # Update checkbox label separately (to allow testing of logic)
        output$label_id <- shiny::renderUI(checkbox_label())
      }
      # For exporting all listings, only excel is available
      type_choices <- shiny::eventReactive(input[[EXP$DATASEL_ID]], {
        if (input[[EXP$DATASEL_ID]] == "single") {
          type_choices_tmp <- c("Excel" = ".xlsx", "PDF" = ".pdf")
        } else {
          shinyFeedback::showToast(
            "info", "Note that all listings can only be downloaded as Excel file.",
            .options = list(positionClass = "toast-top-center")
          )
          type_choices_tmp <- c("Excel" = ".xlsx")
        }
        return(type_choices_tmp)
      })

      # Update file type choices separately (to allow testing of logic)
      shiny::observeEvent(type_choices(), {
        shiny::updateRadioButtons(inputId = EXP$FILETYPE_ID, choices = type_choices())
      })

      # Warnings
      check_ref_cols <- shiny::eventReactive(
        c(input[[EXP$REFCOL_ID]], input[[EXP$FILENAME_ID]], input[[EXP$SNAPSHOT_ID]]),
        {
          warn_function(input[[EXP$FILENAME_ID]] == "", EXP$FILENAME_ID, "Type in a file name.")
          check_ref_cols_tmp <- calculate_col_width(current_data(), input[[EXP$REFCOL_ID]])$check_ref_cols
          warn_function(
            check_ref_cols_tmp, EXP$REFCOL_ID,
            "Selected reference columns take up too much space or all columns are selected as reference columns."
          )
          warn_function(
            ifelse(is.null(input[[EXP$SNAPSHOT_ID]]), 0, nchar(input[[EXP$SNAPSHOT_ID]])) > 50,
            EXP$SNAPSHOT_ID,
            "Entered footnote exceeds the maximum length of 50 characters."
          )
          return(check_ref_cols_tmp)
        }
      )

      # Check if we should enable/disable download button
      download_enable <- shiny::eventReactive(
        c(check_ref_cols(), input[[EXP$FILENAME_ID]], input[[EXP$DATAPROTECT_ID]], input[[EXP$SNAPSHOT_ID]]),
        {
          dataprotect <- ifelse(!is.null(intended_use_label), input[[EXP$DATAPROTECT_ID]], TRUE)

          if (!check_ref_cols()
              & (input[[EXP$FILENAME_ID]] != "")
              & dataprotect
              & (ifelse(is.null(input[[EXP$SNAPSHOT_ID]]), 0, nchar(input[[EXP$SNAPSHOT_ID]])) <= 50)) {
            return(TRUE)
          } else {
            return(FALSE)
          }
        }
      )

      # Enable/disable download button separately (to allow testing of logic)
      shiny::observeEvent(download_enable(), {
        if (download_enable()) {
          shinyjs::enable(EXP$DOWNLOADBTN_ID)
        } else {
          shinyjs::disable(EXP$DOWNLOADBTN_ID)
        }
      })

      # Download
      output[[EXP$DOWNLOADBTN_ID]] <- shiny::downloadHandler(
        filename = function() {
          paste0(input[[EXP$FILENAME_ID]], input[[EXP$FILETYPE_ID]])
        },
        content = function(file) {
          shiny::removeModal() # close pop up

          data_to_download <- prep_export_data(
            input[[EXP$DATASEL_ID]], current_data(), data_selection_name(), v_dataset_list()
          )

          if (input[[EXP$FILETYPE_ID]] == ".xlsx") {

            excel_export(data_to_download, file, intended_use_label)

          } else {

            shiny::withProgress(message = "Creating pdf.", value = 0, {

              num_pages <- pdf_export(
                data_to_download = data_to_download,
                ref_cols = input[[EXP$REFCOL_ID]],
                file = file,
                metadata = c(
                  dataset_metadata$name(),
                  #to make sure that the date is readable and wont display as numeric value
                  as.character(dataset_metadata$date_range()[2]),
                  input[[EXP$SNAPSHOT_ID]]
                ),
                intended_use_label = intended_use_label
              )

              shiny::setProgress(value = num_pages) # completion of progress bar
              Sys.sleep(0.5) # to make the completion of progress bar visible
            })
          }
        }
      )

      # Export values for shinytest2 (development tests)
      shiny::exportTestValues(
        current_rows = {
          current_rows()
        }
      )

    }
  )
}
