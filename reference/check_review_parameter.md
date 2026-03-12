# Early error feedback function for the optional review parameter

Early error feedback function for the optional review parameter

## Usage

``` r
check_review_parameter(datasets, dataset_names, review, err)
```

## Arguments

- datasets:

  `[list(data.frame)]`

  Available datasets for review.

- dataset_names:

  `[character(n)]`

  Names of the datasets provided by the previous parameter.

- review:

  `[list()]`

  Configuration of the experimental data review feature. Please refer to
  [`vignette("data_review")`](https://boehringer-ingelheim.github.io/dv.listings/articles/data_review.md).

- err:

  `[environment]` This environment has at least one element named
  "messages". It is a character vector. Diagnostic messages related to
  the configuration of the review parameter will be placed here.
