# Data listings module for DaVinci's module manager

This module will present the dataset as listing using the DT package.

## Usage

``` r
mod_listings(
  module_id,
  dataset_names,
  default_vars = NULL,
  pagination = NULL,
  intended_use_label =
    "Use only for internal review and monitoring during the conduct of clinical trials.",
  subjid_var = "USUBJID",
  receiver_id = NULL,
  review = NULL
)
```

## Arguments

- module_id:

  `[character(1)]` Unique module_id identifier. It can only contain
  alphanumeric characters and underscores.

- dataset_names:

  `[character(1+)]`

  Name(s) of the dataset(s) that will be displayed.

- default_vars:

  `[list(characters(1+)) | NULL]`

  A list of character vectors which contain the variable names to be
  displayed as default per dataset. Named according to the
  `dataset_names`. If `NULL`, the first six variables are displayed for
  each dataset.

- pagination:

  `[logical(1) | NULL]` Either a boolean indicating if pagination should
  be activated, or NULL for which pagination will be activated for large
  datasets (nrows \> 1000) automatically.

- intended_use_label:

  `[character(1) | NULL]` Either a string indicating the intended use
  for export, or NULL. The provided label will be displayed prior to the
  download and will also be included in the exported file.

- subjid_var:

  `[character(1)]`

  Column corresponding to subject ID. Default value is 'USUBJID'

- receiver_id:

  `[character(1) | NULL]`

  Character string defining the ID of the module to which to send a
  subject ID. The module must exist in the module list. The default is
  NULL which disables communication.

- review:

  `[list()]`

  Configuration of the experimental data review feature. Only one
  instance of the listings module can use this feature on any given app.
  For more details, please refer to
  [`vignette("data_review")`](https://boehringer-ingelheim.github.io/dv.listings/articles/data_review.md).

## See also

Other data_listings:
[`listings_UI()`](https://boehringer-ingelheim.github.io/dv.listings/reference/listings_UI.md)

## Examples

``` r
if (FALSE) { # interactive()
library(dv.listings)

# 1. Create a data list with example data
data_list <- list(
  adsl  = pharmaverseadam::adsl,
  adae  = pharmaverseadam::adae,
  adtte = pharmaverseadam::adtte_onco
)

# 2. Preprocessing
# Convert data to appropriate types
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

# 3. Module list
module_list <- list(
  "Exemplary listings" = mod_listings(
    module_id = "mod1",
    dataset_names = c("adsl", "adae", "adtte"),
    default_vars = default_vars
  )
)

# 4. Launch the app
dv.manager::run_app(
  data = list("MyData" = data_list),
  module_list = module_list,
  filter_data = "adsl"
)
}
```
