# Internal helper function which performs the download as .xlsx file.

Will export all datasets listed within `data_to_download` into a
separate Excel worksheet. Column names of the datasets will be extended
by their labels.

## Usage

``` r
excel_export(data_to_download, file, intended_use_label)
```

## Arguments

- data_to_download:

  `[list(data.frame)]` A list of data frames to be downloaded.

- file:

  `[character(1)]` A string specifying the filename with ending ".xlsx".
