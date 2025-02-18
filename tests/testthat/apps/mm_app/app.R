use_load_all <- isTRUE(as.logical(Sys.getenv("TEST_LOCAL"))) | isTRUE(as.logical(Sys.getenv("TEST_PUSH")))
if (use_load_all) {
  devtools::load_all("../../../../", quiet = TRUE)
} else {
  library(dv.listings)
}

# Load dummy data for testing purpose
source("../../dummy-data.R")

# Test app
data <- list()
data[["adsl"]] <- convert_data(dm_dummy)
data[["adae"]] <- convert_data(ae_dummy)
data[["small"]] <- convert_data(dm_dummy[1:4])


# Add data list without any labels for comparison purposes
data_no_lab <- data

# Labels for dataset names of first data list
attributes(data$adsl)$label <- "Subject Level"
attributes(data$adae)$label <- "Adverse Events"
attributes(data$small)$label <- "Few columns"

# Define default columns
default_vars_multi <- list(adsl = names(data$adsl)[1:8], adae = names(data$adae)[1:8])
default_vars_single <- NULL

# No Label within column header
attributes(data$adsl[["STUDYID"]])$label <- NULL

# Define and launch mock app
module_list <- list(
  "Multiple listings" = dv.listings::mod_listings(
    module_id = "multi",
    default_vars = default_vars_multi,
    pagination = TRUE,
    dataset_names = c("adsl", "adae", "small")
  ),
  "Single listing" = dv.listings::mod_listings(
    module_id = "single",
    default_vars = default_vars_single,
    dataset_names = c("adsl")
  )
)

dv.manager::run_app(
  data = list("demo" = data, "demo no labels" = data_no_lab),
  module_list = module_list,
  filter_data = "adsl",
  enableBookmarking = "url"
)
