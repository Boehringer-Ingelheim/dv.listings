# Internal helper function to gather either column or name labels of data.frames

Internal helper function to gather either column or name labels of
data.frames

## Usage

``` r
get_labels(dataset)
```

## Arguments

- dataset:

  `[data.frame | list(data.frame)]`

  Single data.frame to gather its column labels or a list of data.frames
  to gather their name labels.

## Value

Character vector of labels of `dataset`. Named with the respective
column or data.frame names. Contains "No label" for missing labels.
