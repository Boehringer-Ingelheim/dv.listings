# Internal helper function to cut strings that exceed a specific length

Shortens strings that are too long and denotes cutting by pasting "..."
at the end of the string.

## Usage

``` r
shorten_entries(vec, len_max)
```

## Arguments

- vec:

  `[character(0+)]` A character vector containing the strings to be cut
  if exceeding a maximal length.

- len_max:

  `[integer(1)]` An integer specifying the maximal length, must be equal
  to or greater than 3.

## Value

A character vector containing the shortened strings in case they
exceeded the maximal length.
