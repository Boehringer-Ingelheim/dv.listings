use_load_all <- isTRUE(as.logical(Sys.getenv("TEST_LOCAL"))) | isTRUE(as.logical(Sys.getenv("TEST_PUSH")))
if (use_load_all) {
  devtools::load_all("../../../../", quiet = TRUE)
} else {
  library(dv.listings)
}

# 1. Create a data list with example data
data_list <- list(
  adsl  = pharmaverseadam::adsl,
  adae  = pharmaverseadam::adae,
  adtte = pharmaverseadam::adtte_onco
)

#Convert data to appropriate types
data_list$adsl <- dv.listings::convert_data(data_list$adsl)
data_list$adae <- dv.listings::convert_data(data_list$adae)
data_list$adtte <- dv.listings::convert_data(data_list$adtte)

# Assign meaningful labels to data domain names
attributes(data_list$adsl)$label <- "Subject Level"
attributes(data_list$adae)$label <- "Adverse Events"
attributes(data_list$adtte)$label <- "Time-to-Event"

# Specify default variables
default_vars <- list(
  adsl = c("STUDYID", "USUBJID", "SITEID", "ARM"),
  adae = c("STUDYID", "ASTDY", "AENDT", "AESER")
)

# 2. Create list of modules - must include listings module and dv.papo module.
module_list <- list(
  "Exemplary listings" = dv.listings::mod_listings(
    module_id = "listings1",
    dataset_names = c("adsl", "adae", "adtte"),
    default_vars = default_vars,
    receiver_id = "papo1"
  ),
  "Patient Profile" = dv.papo::mod_patient_profile(
    module_id = "papo1",
    subject_level_dataset_name = "adsl",
    subjid_var = "USUBJID",
    summary = list(
      vars = c("SUBJID", "SITEID", "ARM", "TRTSDT", "TRTEDT", "AGE", "RACE", "SEX"),
      column_count = 3L
    ),
    listings = list(
      "Adverse Events" = list(
        dataset = "adae",
        default_vars = c("ASTDT", "ASTDY", "AENDT", "AENDY", "AEDECOD", "AESEV")
      )
    ),
    sender_ids = "listings1"
  )
)

dv.manager::run_app(
  data = list("MyData" = data_list),
  module_list = module_list,
  filter_data = "adsl"
)
