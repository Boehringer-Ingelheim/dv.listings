# Internal helper function to set missing default columns

Avoid empty displays in case no default columns were specified by
showing the first six columns of a dataset. If the dataset does contain
less than six columns, the whole dataset will be displayed.

## Usage

``` r
fill_default_vars(default_vars, dataset)
```

## Arguments

- default_vars:

  `[list(character(0+))]`

  Named list of character vectors which contain the names of columns to
  be displayed at module launch per dataset. List entries are named
  according to the `dataset` names but can be NULL for some or all
  datasets.

- dataset:

  `[list(data.frame)]`

  A list of data.framish dataset(s) that will be shown as listings.

## Value

Named list of character vectors which Contain the names of columns to be
displayed at module launch for every dataset. List entries are named
according to the `dataset` names.
