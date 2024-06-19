# Use a list to declare the specs
# nolint start
specs_list <- list

listing <- specs_list(
  "display_listing" = "dv.listings displays a dataset in a tabular form",
  "listing_selection" = "dv.listings includes a dropdown menu to select which listing to be shown.",
  "listings_label" = "dv.listings displays the label of a listing if available. The label is concatenated to the listing’s dataset name and the resulting strings are provided as choices in the listings dropdown menu.",
  "column_selection" = "dv.listings includes a dropdown menu to select the columns from the selected listing to be shown and arrange their order.",
  "column_label" = "dv.listings displays the column labels of a listing if available. Column names are pasted together with their label. These extended column titles replace the original column names, so that they are visible in the listings display and column dropdown menu.",
  "sorting_columns" = "dv.listings includes sorting functionality for each of the different variables included in the dataset",
  "restore_row_order" = "dv.listings includes a button to restore the row order of a listing to the state as it is in the original data.",
  "default_vars" = "If pre-specifications for default columns are available, dv.listings will display them at app launch for the respective listing. If not, dv.listings will show the first six columns of the listing - or all columns, in case the number of columns is less than six.",
  "retain_last_selection" = "dv.listings can remember and retain the last column selections after switching listings during the current session. It also restores the ",
  "bookmarking" = "The module is compatible with the bookmarking feature of the dv.manager."
)
export <- specs_list(
  "export" = "dv.listings includes a button to export the listing(s). A click to the button envokes a pop-up to appear that allows the user to decide whether the download should only contain the displayed listing or all available listings, provide a file name (defaulted to the dataset name), and select from available file types.",
  "export_active_listing" = "For downloading only the currently active listing, the listing will be saved as it is displayed, either in .xlsx or .pdf format. In case filters were applied, the downloaded output will only contain the filtered data.",
  "export_excel" = "For downloading all listings, the tables can be saved in .xlsx format only without considering local filters. Each listing will be placed in an individual worksheet within the file.",
  "export_pdf" = "For downloading in .pdf format, users can select one or multiple reference column(s), which will be displayed on all document pages."
)

specs <- c(
  listing,
  export
)
# nolint end