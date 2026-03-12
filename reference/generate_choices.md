# Internal helper function to generate either column or dataset choices for the corresponding UI dropdown menues

Internal helper function to generate either column or dataset choices
for the corresponding UI dropdown menues

## Usage

``` r
generate_choices(dataset)
```

## Arguments

- dataset:

  `[data.frame | list(data.frame)]`

  Single data.frame to retrieve its column labels or a list of
  data.frames to retrieve their name labels. In both cases the labels
  will be pasted to the column/dataset names and returned in order to
  provide meaningful choices in the corresponding dropdown menues to the
  end-user.

## Value

Character vector of the type `<name> [<label>]`. Adds "No label" as
placeholder for missing labels.
