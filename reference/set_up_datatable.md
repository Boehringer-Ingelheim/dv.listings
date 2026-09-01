# Internal helper function to set up colnames, rownames, and paging arguments for DT's datatable. Main purpose is to easily test returned arguments.

Internal helper function to set up colnames, rownames, and paging
arguments for DT's datatable. Main purpose is to easily test returned
arguments.

## Usage

``` r
set_up_datatable(dataset, pagination, exclude_var_names_from_column_headings)
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

- exclude_var_names_from_column_headings:

  `[logical(1)]`

  Use only dataset variable labels for variables that have them (output
  "Var Label" instead of "VAR_NAME \[Var Label\]").

## Value

List containing character vectors for column names and row names and a
logical value for de-/activating paging
