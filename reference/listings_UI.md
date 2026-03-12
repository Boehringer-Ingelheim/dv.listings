# A module that displays datasets as listings

This module displays a given dataset as listing. It allows switching
between datasets if it receives more than one.

## Usage

``` r
listings_UI(module_id)

listings_server(
  module_id,
  dataset_list,
  default_vars = NULL,
  dataset_metadata,
  pagination = NULL,
  intended_use_label = NULL,
  subjid_var = "USUBJID",
  on_sbj_click = NULL,
  review = NULL
)
```

## Arguments

- module_id:

  Shiny id

- dataset_list:

  `[shiny::reactive(list(data.frame)]`

  A reactive list of data.framish dataset(s) that will be shown as
  listing.

- default_vars:

  `[list(characters(1+)) | NULL]`

  A list of character vectors which contain the variable names to be
  displayed as default per dataset. Named according to the
  `dataset_names`. If `NULL`, the first six variables are displayed for
  each dataset.

- dataset_metadata:

  `[list(character(1), character(1+))]` A list with the following two
  elements: `dataset_metadata$name()` containing a reactive string
  specifying the name of the selected dataset and
  `dataset_metadata$date_range()` containing a reactive character vector
  with two entries specifying the earliest and latest modification date
  in the dataset. Usually obtained from module manager.

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

- on_sbj_click:

  `[function()]`

  Function to invoke when a subject ID is clicked in a listing

- review:

  `[list()]`

  Configuration of the experimental data review feature. Only one
  instance of the listings module can use this feature on any given app.
  For more details, please refer to
  [`vignette("data_review")`](https://boehringer-ingelheim.github.io/dv.listings/articles/data_review.md).

## Functions

- `listings_server()`:

## See also

Other data_listings:
[`mod_listings()`](https://boehringer-ingelheim.github.io/dv.listings/reference/mod_listings.md)
