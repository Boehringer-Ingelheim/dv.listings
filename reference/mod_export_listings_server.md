# Create server for export listings shiny module of dv.listings

Create server for export listings shiny module of dv.listings

## Usage

``` r
mod_export_listings_server(
  module_id,
  dataset_metadata,
  dataset_list,
  data,
  data_selection_name,
  current_rows,
  intended_use_label
)
```

## Arguments

- module_id:

  `[character(1)]` A unique ID string to create a namespace. Must match
  the ID of `mod_exp_listings_UI()`.

- dataset_metadata:

  `[list(character(1), character(1+))]` A list with the following two
  elements: `dataset_metadata$name()` containing a reactive string
  specifying the name of the selected dataset and
  `dataset_metadata$date_range()` containing a reactive character vector
  with two entries specifying the earliest and latest modification date
  in the dataset. Usually obtained from module manager.

- dataset_list:

  `[shiny::reactive(list(data.frame))]` A reactive list of named
  datasets. Usually obtained from module manager.

- data:

  `[shiny::reactive(list(data.frame, character(0+)))]` A reactive list
  with the following two elements: `data()$data` containing a single
  data frame and `data()$col_names` containing the column names of
  `data()$data`.

- data_selection_name:

  `[shiny::reactive(character(1))]` A reactive string specifying the
  name of `data()$data`.

- current_rows:

  `[shiny::reactive(character(0+))]` A reactive integer vector
  containing a selection of row numbers from `data()$data`.

- intended_use_label:

  `[character(1) | NULL]` Either a character indicating the intended use
  for the download, or NULL. If a label is provided it will be shown
  before the download and will also be included in the downloaded file.
