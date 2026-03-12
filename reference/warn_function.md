# Internal helper function for warnings in case of missing/wrong selections done by the user.

Triggers a warning feedback if a particular condition is true and hides
the feedback if it is false by using shinyFeedback.

## Usage

``` r
warn_function(cond, input_id, text)
```

## Arguments

- cond:

  `[logical(1)]` Logical value that indicates whether the warning should
  be triggered (if `TRUE`) or not (if `FALSE`).

- input_id:

  `[character(1)]` ID string indicating at which input field the warning
  should appear.

- text:

  `[character(1)]` A string containing the warning message to be
  displayed.
