# Internal helper function to set up colnames, rownames, and paging arguments for DT's datatable. Main purpose is to easily test returned arguments.

Internal helper function to set up colnames, rownames, and paging
arguments for DT's datatable. Main purpose is to easily test returned
arguments.

## Usage

``` r
set_up_datatable(dataset, pagination)
```

## Arguments

- dataset:

  `[data.frame]`

  Single data.frame

- pagination:

  `[logical(1) | NULL]`

  Either a boolean indicating if pagination should be activated, or NULL
  for which pagination will be activated for large datasets (nrows
  \> 1000) automatically.

## Value

List containing character vectors for column names and row names and a
logical value for de-/activating paging
