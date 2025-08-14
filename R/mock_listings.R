#' Mock app without layout
#'
#' \code{mock_listings_app} launches a mock app for the dv.listings shiny module. Only for development purposes.
#'
#' @param mode Character value that indicates whether to include one or multiple datasets. Could either be "single" or
#'   "multi".
#'
#' @keywords internal
#' @family mock apps
mock_listings_app <- function(mode = "single") {
  stopifnot(mode %in% c("single", "multi"))

  mock_listings_UI <- function(id) { # nolint
    ns <- ifelse(is.character(id), shiny::NS(id), shiny::NS(NULL))
    shiny::fluidPage(listings_UI(ns("listings")))
  }

  mock_listings_server <- function(input, output, session) {
    if (mode == "single") {
      data <- list(mtcars = datasets::mtcars)
      data$mtcars["index"] <- rownames(data$mtcars)
      attributes(data$mtcars$mpg)$label <- "Miles/(US) gallon"
      def_col <- list(mtcars = c("index", "mpg", "cyl"))
    } else {
      mtcars <- datasets::mtcars
      iris <- datasets::iris
      attributes(mtcars$mpg)$label <- "Miles/(US) gallon"
      attributes(mtcars)$label <- "Motor Trend Car Road Tests"
      data <- list("mtcars" = mtcars, "iris" = iris)
      def_col <- list(mtcars = names(mtcars[1:3]), iris = names(iris))
    }

    listings_server(
      "listings",
      dataset_list = shiny::reactive({
        data
      }), default_vars = def_col,
      dataset_metadata = list(
        name = shiny::reactive("test_name"),
        date_range = shiny::reactive({
          c("2022-01-01", "2022-12-03")
        })
      )
    )
  }

  shiny::shinyApp(
    mock_listings_UI,
    mock_listings_server
  )
}


#' Mock app integrated in the module manager
#'
#' \code{mock_listings_mm} launches a mock app for the dv.listings shiny module by means of
#' the module manager (dv.manager).
#'
#' @keywords internal
#' @family mock apps
mock_listings_mm <- function() {
  data <- list()
  data[["adsl"]] <- convert_data(pharmaverseadam::adsl)
  data[["adae"]] <- convert_data(pharmaverseadam::adae)
  data[["adtte"]] <- convert_data(pharmaverseadam::adtte_onco)
  data[["test"]] <- data.frame(
    USUBJID = c("01-701-1015", "01-701-1023"),
    test_date = as.POSIXct(c("2023-03-09", "2023-03-10"))
  )


  # Add dataset with less than six columns to check behavior for default number of columns
  data[["small"]] <- convert_data(pharmaverseadam::adsl[1:4])

  # Add data list without any labels for comparison purposes
  data_no_lab <- data

  # Labels for dataset names of first data list
  attributes(data$adsl)$label <- "Subject Level"
  attributes(data$adae)$label <- "Adverse Events"
  attributes(data$adtte)$label <- "Time-to-Event"
  attributes(data$small)$label <- "Few columns"

  # Define default columns
  default_vars_multi <- list(adsl = names(data$adsl)[1:8], adae = names(data$adae)[1:8])
  default_vars_single <- NULL

  # No Label within column header
  attributes(data$adsl[["STUDYID"]])$label <- NULL
  
  mod_receiver <- function(module_id, sender_id) {
    list(
      ui = function(id) shiny::verbatimTextOutput(shiny::NS(id)("out")),
      server = function(afmm) {
        shiny::moduleServer(
          module_id,
          function(input, output, session) {
            output[["out"]] <- shiny::reactive({
              sprintf('Message from module "%s": %s', sender_id, afmm[["module_output"]]()[[sender_id]][["subj_id"]]())
            })
          }
        )
      },
      module_id = module_id
    )
  }

  # Define and launch mock app
  module_list <- list(
    "Multiple Listings" = dv.listings::mod_listings(
      dataset_names = c("adsl", "adae", "adtte", "small", "test"),
      module_id = "multi",
      default_vars = default_vars_multi,
      pagination = TRUE, receiver_id = "receiver"
    ),
    "Single Listing" = dv.listings::mod_listings(
      dataset_names = "adsl",
      module_id = "single",
      default_vars = default_vars_single,
      intended_use_label = NULL
    ),
    "Message receiver" = mod_receiver(module_id = "receiver", sender_id = "multi")
  )

  dv.manager::run_app(
    data = list("demo" = data, "demo no labels" = data_no_lab),
    module_list = module_list,
    filter_data = "adsl",
    enableBookmarking = "url"
  )
}

mock_review <- function() {
  ae <- safetyData::sdtm_ae
  dm <- safetyData::sdtm_dm
  
  # Filter out subjects that didn't pass initial screening
  screen_fail_mask <- dm[["ACTARMCD"]] == "Scrnfail"
  dm <- dm[!screen_fail_mask, ]
  
  # Subset AE columns and extend the domain with a couple of date columns from the demographic data frame
  ae_cols <- c("STUDYID", "USUBJID", "AETERM", "AEHLGT", "AEHLT", "AELLT", "AEDECOD", "AESOC",
               "AESTDTC", "AEENDTC", "AEOUT", "AEACN", "AEREL", "AESEV", "AESEQ")
  ae <- merge(dm[c("USUBJID", "RFXSTDTC", "RFSTDTC")], ae[ae_cols], by = "USUBJID")
  
  # Add labels
  var_labels <- c(
    STUDYID = "Study Identifier",
    USUBJID = "Unique Subject Identifier",
    AETERM = "Reported Term for the Adverse Event",
    AEHLGT = "High Level Group Term",
    AEHLT = "High Level Term",
    AELLT = "Lowest Level Term",
    AEDECOD = "Dictionary-Derived Term",
    AESEQ = "Sequence Number",
    AESOC = "Primary System Organ Class",
    AESTDTC = "Start Date/Time of Adverse Event",
    AEENDTC = "End Date/Time of Adverse Event",
    AEOUT = "Outcome of Adverse Event",
    AEACN = "Action Taken with Study Treatment",
    AEREL = "Causality",
    AESEV = "Severity/Intensity",
    RFXSTDTC = "Date/Time of First Study Treatment",
    RFSTDTC = "Subject Reference Start Date/Time",
    DOMAIN = "Domain Abbreviation",
    SUBJID = "Subject Identifier for the Study",
    RFENDTC = "Subject Reference End Date/Time",
    RFXENDTC = "Date/Time of Last Study Treatment",
    RFICDTC = "Date/Time of Informed Consent",
    RFPENDTC = "Date/Time of End of Participation",
    DTHDTC = "Date/Time of Death",
    DTHFL = "Subject Death Flag",
    SITEID = "Study Site Identifier",
    AGE = "Age",
    AGEU = "Age Units",
    SEX = "Sex",
    RACE = "Race",
    ETHNIC = "Ethnicity",
    ARMCD = "Planned Arm Code",
    ARM = "Description of Planned Arm",
    ACTARMCD = "Actual Arm Code",
    ACTARM = "Description of Actual Arm",
    COUNTRY = "Country",
    DMDTC = "Date/Time of Collection",
    DMDY = "Study Day of Collection"
  )
 
  dm <- set_labels(dm, var_labels[names(dm)]) 
  ae <- set_labels(ae, var_labels[names(ae)])
  
  data_list <- list(ae = ae, dm = dm)
  
  attr(data_list[["ae"]], "label") <- "Adverse Events"  # NOTE(miguel): Otherwise uses label from the `dm` dataset
  
  # Step 4 - Module specification
  listing <- mod_listings(
    module_id = "listing",
    dataset_names = c("ae", "dm"),
    default_vars = list(
      ae = c(
        "USUBJID",
        "AESEV",
        "RFXSTDTC",
        "RFSTDTC",
        "AETERM",
        "AEHLGT", "AEHLT", "AELLT", "AEDECOD", "AESOC",
        "AESTDTC", "AEENDTC",
        "AEOUT",
        "AEACN",
        "AEREL"
      ),
      dm = c("COUNTRY", "RFXSTDTC")
    ),
    # Jumping to the Patient Profile module is possible, provided that it is included as well:
    receiver_id = "papo",
    review = list(
      datasets = list(
        ae = list(
          id_vars = c("USUBJID", "AESEQ"), 
          tracked_vars = c(
            "AESEV", "RFXSTDTC", "RFSTDTC", "AETERM", "AEHLGT", "AEHLT", "AELLT", 
            "AEDECOD", "AESOC", "AESTDTC", "AEENDTC", "AEOUT", "AEACN", "AEREL"
          ))
      ),
      choices = c("Pending", "Reviewed with no issues", "Action required", "Resolved"),
      roles = c("TSTAT", "SP", "Safety", "CTL"),
      store_path = tempdir()
    )
  )
  
  mod_receiver <- function(module_id, sender_id) {
    list(
      ui = function(id) shiny::verbatimTextOutput(shiny::NS(id)("out")),
      server = function(afmm) {
        shiny::moduleServer(
          module_id,
          function(input, output, session) {
            output[["out"]] <- shiny::reactive({
              sprintf('Message from module "%s": %s', sender_id, afmm[["module_output"]]()[[sender_id]][["subj_id"]]())
            })
          }
        )
      },
      module_id = module_id
    )
  }
  
  # Step 5 - Run app
  dv.manager::run_app(
    data = list("AE_list_review" = data_list),
    module_list = list(
      "Listing" = listing,
      "Signal receiver" = mod_receiver(module_id = "papo", sender_id = "listing")
    ),
    filter_data = "dm",
    filter_key = "USUBJID"
  )
}
