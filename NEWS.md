# dv.listings 4.1.0-1

- Prototype of listing annotation feature.

# dv.listings 4.1.0

- Add jumping feature
- The module allows now to 
  - select all columns
  - unselect all columns
  - reset to default columns
  - reset filters
- Remove support for data dispatchers
- Provide early feedback of module misconfiguration

# dv.listings 4.0.0

Package was renamed to dv.listings.

# dv.table 3.0.1

The module allows now to 
- switch between listings
- select/hide columns
- order columns
- search the listing on a free-text basis
- filter columns
- display labels instead of variable/dataset names in column headers and dropdown menues
- export listings to .xlsx and .pdf

The module is now consistent to DaVinci parameter standards, thus `mod_table()`, `table_UI()`, `table_server()` 
introduce the following changes:
  - parameters reordered
  - `id` replaced by `module_id`
  - `data_list` replaced by `dataset_list`
  - `default_cols` replaced by `default_vars`
  - `dataset_names` added
  - `dataset_disp` is now an optional parameter

Additionally, a bug was fixed where fast column selection caused a reactive loop.

# dv.table 2.0.0

- module now uses functional dispatchers from module manager instead of expressions

# dv.table 1.0.0

- initial version of dv.table
- dv.table is a module mainly focused on validation
