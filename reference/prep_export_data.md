# Internal helper function for preparing the dataset(s) to download.

Internal helper function for preparing the dataset(s) to download.

## Usage

``` r
prep_export_data(
  data_selection,
  current_data,
  data_selection_name,
  dataset_list
)
```

## Arguments

- data_selection:

  `[character(1)]` Either `"single"` or `"all"` depending on whether the
  currently displayed dataset (`current_data`) or all datasets
  (`dataset_list`) should be downloaded.

- current_data:

  `[data.frame]` A single data frame with named columns.

- data_selection_name:

  `[character(1)]` A string specifying the name of `current_data`.

- dataset_list:

  `[list(data.frame)]` A list of named datasets.

## Value

Named list containing the data frames which are now ready for download.
