# Internal helper function to prepare a data frame and determine the width of its columns for pdf export

Shortens and splits the column labels if necessary, adapts the entries
of the reference column vector for easier handling, and determines the
width of reference columns and data frame columns for upcoming pdf
generation. Widths are given in amount of characters.

## Usage

``` r
calculate_col_width(df, ref)
```

## Arguments

- df:

  `[data.frame]` A single data frame with named columns.

- ref:

  `[character(0+)]` A character vector whose entries specify a selection
  of columns of `df`. The format of the entries follows: `name [label]`.

## Value

The transformed reference column vector whose entries include solely the
column names (`ref`), the splitted labels as vectors stored within a
list (`label_vecs`), the maximal column widths for all data frame
columns (`width_max`), the maximal column widths for the reference
columns (`ref_width`), the maximal column widths for the non-reference
columns of the dataframe (`width`), the table width exclusively row
names (`table_width`), and a logical indicating whether the reference
columns specification is valid (`check_ref_cols`).
