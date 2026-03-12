use_load_all <- isTRUE(as.logical(Sys.getenv("TEST_LOCAL"))) | isTRUE(as.logical(Sys.getenv("TEST_PUSH")))
if (use_load_all) {
  devtools::load_all("../../../../", quiet = TRUE)
} else {
  library(dv.listings)
}

data <- list(adsl = head(safetyData::adam_adsl, 0))

# Define and launch mock app
module_list <- list(
  "Single listing" = dv.listings::mod_listings(
    module_id = "single",
    default_vars = NULL,
    dataset_names = c("adsl")
  )
)

dv.manager::run_app(
  data = list("demo" = data),
  module_list = module_list,
  filter_data = "adsl",
  enableBookmarking = "url"
)
