# Changelog

## dv.listings 4.3.5

- New `exclude_var_names_from_column_headings` parameter allows simpler
  headings
- New `footers` parameter allows providing per-dataset footer text
- Enforce Excel sheet name validity rules prior to export
- Fix header and body offsetting when dv.manager’s sidebar
  collapses/expands
- Review functionality:
  - Features:
    - Allow expanding review choices
    - Include review information in single listing exports, if available
    - Expand revision count limit from 1000 to 10000 entries
  - Bug fixes:
    - Cope with initially empty datasets
    - Guard against problematic characters in dataset names
    - Remove column change highlighting from rows that have been
      modified but restored afterwards
    - Fix bug in column change highlight when known rows are removed

## dv.listings 4.3.4

- Review functionality:
  - Fix regressions when global filter is active

## dv.listings 4.3.3

- Review functionality:
  - Guard against the removal of known rows and provide parameter to
    disable this check

## dv.listings 4.3.2

- Limited support for column width resizing
- Remove dependencies on jsonlite and lastpage.sty latex package
- Review functionality:
  - Per-role undo of review actions
  - Faster response to review actions
  - Progress indicators during long review actions
  - Improved checks for asynchronous javascript-mediated review write
    actions

## dv.listings 4.3.1

- Guard against selection of nested or conflicting review storage
  location.
- Fix mapping of current listing to canonical row indices.

## dv.listings 4.3.0

- Display title of current table
- Review functionality
  - Bulk editing interface
  - Highlighting of modified columns for outdated reviews
  - Tolerate previously known data rows that go missing or reappearing
    across dataset updates

## dv.listings 4.2.0

- Includes the experimental review functionality
- Adapt dataset selection to accommodate for switching of dataset_list
- Adapt fill_default_vars to keep specified default values for datasets
  which are not present
- Adapt checkmate calls to allow default values for datasets which are
  not present
- Top buttons are reordered in a single line to avoid consuming
  excessive space

## dv.listings 4.1.0

- Add jumping feature
- The module allows now to
  - select all columns
  - unselect all columns
  - reset to default columns
  - reset filters
- Remove support for data dispatchers
- Provide early feedback of module misconfiguration

## dv.listings 4.0.0

Package was renamed to dv.listings.
