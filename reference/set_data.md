# Prepare data as it should be displayed in the module

Reduces the dataset to only those columns specified in `selector` and
produces descriptive column names by adding their labels to the variable
names.

## Usage

``` r
set_data(base_data, selector)
```

## Arguments

- base_data:

  `[data.frame]`

  Single data.frame.

- selector:

  `[character(0+)]`

  Character vector of a selection of column names from `base_data`.

## Value

A list of tree elements: `data` is the data.frame prepared to be
displayed as-is. `col_names` is a vector of column names created by
combining the variable names with their labels. `row_names` is a
character vector containing the number of the corresponding row.
