# Internal helper function to prepare a data frame and determine the width of its columns for pdf export

Shortens and splits the column label if necessary, so that it fits into
`label_width` rows of its column.

## Usage

``` r
split_label(label, min_width, max_width, label_width)
```

## Arguments

- label:

  `[character(1)]` A string representing the label.

- min_width:

  `[integer(1)]` An integer specifying the minimal width of the column
  (not less than 1).

- max_width:

  `[integer(1)]` An integer specifying the maximal width of the column
  (greater than 3 and also not less than min_width `[integer(1)]`).

- label_width:

  `[integer(1)]` An integer specifying the maximal number of rows which
  is allocated for the label (not less than 1).

## Value

A list containing the `label_vec` which includes the splitted label and
the actual width `col_width`.
