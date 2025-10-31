# Use a list to declare the specs
# nolint start line_length_linter
specs_list <- list

listing <- specs_list(
  "display_listing" = "dv.listings displays a dataset as listing",
  "listing_selection" = "dv.listings includes a dropdown menu to select which dataset to be shown.",
  "listings_label" = "dv.listings displays the label of a dataset if available. The label is concatenated to the dataset name and the resulting strings are provided as choices in the module's dropdown menu.",
  "column_selection" = "dv.listings includes a dropdown menu to select the columns from the selected listing to be shown and arrange their order.",
  "column_label" = "dv.listings displays extended column headers consisting of the variable name pasted together with its label, if available. These extended column headers replace the original variable names in the column dropdown menu.",
  "sorting_columns" = "dv.listings includes sorting functionality for each of the column.",
  "searching" = "dv.listings includes a search box.",
  "restore_row_order" = "dv.listings includes a button to restore the row order of a listing to the state as it is in the original data.",
  "default_vars" = "If pre-specifications for default columns are available, dv.listings will display them at app launch for the respective listing. If not, dv.listings will show the first six columns of the listing - or all columns, in case the number of columns is less than six.",
  "retain_last_selection" = "dv.listings can remember and retain the last column selections after switching listings during the current session. It also restores the remembered selections for all listings after bookmarking.",
  "bookmarking" = "The module is compatible with the bookmarking feature of the dv.manager.",
  "select_all_columns" = "dv.listings includes functionality to select all the columns.",
  "unselect_all_columns" = "dv.listings includes functionality to unselect all the columns.",
  "reset_columns" = "dv.listings includes functionality to reset to the default columns.",
  "reset_filters" = "dv.listings includes functionality to reset all filters.",
  "jumping_feature" = "dv.listings includes jumping feature."
)
export <- specs_list(
  "export" = "dv.listings includes a button to export the listing(s). A click to the button envokes a pop-up to appear that allows the user to decide whether the download should only contain the displayed listing or all available listings, provide a file name (defaulted to the dataset name), and select from available file types.",
  "export_active_listing" = "For downloading only the currently active listing, the listing will be saved as it is displayed, either in .xlsx or .pdf format. In case filters were applied, the downloaded output will only contain the filtered data.",
  "export_excel" = "For downloading all listings, the tables can be saved in .xlsx format only without considering local filters. Each listing will be placed in an individual worksheet within the file.",
  "export_pdf" = "For downloading in .pdf format, users can select one or multiple reference column(s), which will be displayed on all document pages."
)
review <- specs_list(
  "review" = "dv.listings offers data review functionality",
  "review_per_row" = "review feature offers per-row reviews",
  "review_per_role" = "review feature offers per-role reviews",
  "review_delta_detection" = "review feature detects changes in tracked variables across dataset updates",
  "review_reject_empty_dataset" = "review feature refuses to review empty datasets",
  "review_hash_no_false_negatives" = "review hash function catches all cell changes up to four changes per row",
  "review_change_attribution" = "review hash function changes can be attributed to specific modified cells",
  "review_accept_removal_of_rows" = "review feature tolerates the removal of rows"
)
# nolint end line_length_linter

specs <- c(
  listing,
  export,
  review
)
