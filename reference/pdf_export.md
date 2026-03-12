# Internal helper function which performs the download as .pdf file.

Will export the dataset listed within `data_to_download` into a PDF
file.

## Usage

``` r
pdf_export(
  data_to_download,
  ref_cols,
  file,
  metadata,
  active_session = TRUE,
  intended_use_label
)
```

## Arguments

- data_to_download:

  `[list(data.frame)]` A named list containing one data frame to be
  downloaded. \#' @param ref `[character(0+)]` A character vector whose
  entries specify a selection of columns of the dataset to download. The
  format of the entries follows: `name [label]`.

- file:

  `[character(1)]` A string specifying the filename with ending ".pdf".

- metadata:

  `[character(3)]` A character vector specifying the dataset's name,
  date, and an additional footnote text.

- active_session:

  `[logical(1)]` Logical value that indicates if the helper function is
  used within a shiny session. If yes, the parts for displaying a
  progress bar get activated. Defaults to `TRUE`.

## Value

Number of PDF pages that are generated.
