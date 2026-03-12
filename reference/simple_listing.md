# A module that displays a single dataset in a table form

This module shows a given dataset in a table form.

## Usage

``` r
simple_listing_UI(module_id)

simple_listing_server(module_id, dataset)

mod_simple_listing(dataset_name, module_id)
```

## Arguments

- module_id:

  `[character(1)]` Unique module_id identifier. It can only contain
  alphanumeric characters and underscores.

- dataset:

  a data.framish dataset that will be shown as a table

- dataset_name:

  `[character(1)]`

## Functions

- `simple_listing_UI()`: UI

- `simple_listing_server()`: server

- `mod_simple_listing()`: module
