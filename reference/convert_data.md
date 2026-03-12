# This function is a wrapper for [`utils::type.convert()`](https://rdrr.io/r/utils/type.convert.html) to keep column labels

Converts data types of data.frame columns to logical, integer, numeric,
complex, character or factor as appropriate. Re-assigns column labels
after converting data type.

## Usage

``` r
convert_data(dataset)
```

## Arguments

- dataset:

  `[data.frame]`

  Single data.frame to convert the column data types

## Value

data.frame with converted data types of data.frame columns
