# 1. Create a data list with example data
data_list <- list(
  adsl  = pharmaverseadam::adsl,
  adae  = pharmaverseadam::adae,
  adtte = pharmaverseadam::adtte_onco
)

#Convert data to appropriate types
data_list$adsl <- convert_data(data_list$adsl)
data_list$adae <- convert_data(data_list$adae)
data_list$adtte <- convert_data(data_list$adtte)

# Assign meaningful labels to data domain names
attributes(data_list$adsl)$label <- "Subject Level"
attributes(data_list$adae)$label <- "Adverse Events"
attributes(data_list$adtte)$label <- "Time-to-Event"

# Specify default variables
default_vars <- list(
  adsl = c("STUDYID", "USUBJID", "SITEID", "ARM"),
  adae = c("STUDYID", "ASTDY", "AENDT", "AESER")
)

mod <- dv.listings::mod_listings(
  module_id = "mod",
  dataset_names = c("adsl", "adae", "adtte"),
  default_vars = default_vars,
  receiver_id = "papo"
)

trigger_input_id <- "mod-listing_rows_selected"
test_communication_with_papo(mod, data_list, trigger_input_id)
