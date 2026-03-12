# Internal helper function to divide a data frame into smaller data frames that fit on one PDF page each

Needed as preprocessing step before PDF creation by means of RMarkdown
can take place.

## Usage

``` r
pdf_preprocessing(df, ref)
```

## Arguments

- df:

  `[data.frame]` A single data frame with named columns.

- ref:

  `[character(0+)]` A character vector whose entries specify a selection
  of columns of `df`. The format of the entries follows: `name [label]`.

## Value

Named list containing a list of data frames (`list_of_df`) whose entries
fit on one PDF page each.
