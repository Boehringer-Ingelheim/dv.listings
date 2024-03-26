
# dv.listings

DaVinci's *dv.listings* module displays arbitrary datasets as listings. 
Users can select the dataset to be shown, and specify
which columns of the dataset should be displayed as well as their order.
The displayed columns can be sorted and filtered. Moreover, the
table can be filtered to show only entries containing a keyword entered by the user. 
The module is not limited to usage of one data source,
most notably, it can handle data from ADaM or SDTM.

<img src="man/figures/table_main_view.PNG" width="100%" />

The module is prepared to be used in combination with *dv.manager* and
supports its bookmarking functionality.

## Installation

Feel free to copy the following code chunk to install the latest version of *dv.listings*.

``` r
if (!require("remotes")) install.packages("remotes")
remotes::install_github("Boehringer-Ingelheim/dv.listings")
```

Since the *dv.listings* module is intended to be used within an application
created by means of DaVinci’s *dv.manager* package, make sure that you
have installed *dv.manager* with a version number equally to or higher than 2.1.0

## Data requirements

As stated above, *dv.listings* can display data from various data sources,
such as (but not limited to) SDTM and ADaM. However, in order for the
column filters to work, columns need to be converted to appropriate
types, e.g. categorial data should be stored as factors, numbers as
numeric, etc. The easiest way to convert the data is by using
`convert_data()` which comes along the package. An example can be found
in the section below.

Note that `dv.listings` drops row names. In case your dataset is equipped
with informative row names that are required to be displayed, you have
to include them manually in the scope of preprocessing. For example:

``` r
my_data <- datasets::mtcars
my_data[["index"]] <- rownames(my_data)
attributes(my_data$index)$label <- "Former row names"
```

## Example

To define an app containing *dv.listings* using *dv.manager*, you need to

1.  load data,
2.  make sure that the provided data comply with the requirements of
    *dv.listings*,
3.  define a list of modules,
4.  and launch the app via `run_app()` from *dv.manager*.

The following example contains the listed steps. For data protection
purposes, the example uses dummy data from the *pharmaverseadam* package.

``` r
library(dv.listing)

# 1. Create a data list with example data
data_list <- list(
  adsl  = pharmaverseadam::adsl,
  adae  = pharmaverseadam::adae,
  adtte = pharmaverseadam::adtte_onco
)

# 2. Preprocessing
# Convert data to appropriate types
data_list$adsl <- convert_data(data_list$adsl)
data_list$adae <- convert_data(data_list$adae)
data_list$adtte <- convert_data(data_list$adtte)

# Assign meaningful labels to data domain names
attributes(data_list$adsl)$label <- "Subject Level"
attributes(data_list$adae)$label <- "Adverse Events"
attributes(data_list$adtte)$label <- "Time-to-Event"

# Specify default variables
default_vars <- list(
  adsl = c("STUDYID", "USUBJID", "SITEID", "ARM"),
  adae = c("STUDYID", "ASTDY", "AENDT", "AESER")
)

# 3. Module list
module_list <- list(
  "Exemplary Table" = mod_listings(
    module_id = "mod1",
    dataset_names = c("adsl", "adae", "adtte"),
    default_vars = default_vars
  )
)

# 4. Launch the app
dv.manager::run_app(
  data = list("MyData" = data_list),
  module_list = module_list,
  filter_data = "adsl"
)
```
## Export functionality 

The *dv.listings* module allows users to export listings. Users have the option to either download the currently displayed listing or all available listings.

For downloading only the currently active listing, the listing will be saved as it is displayed, either in .xlsx or .pdf format. In case any filters have been applied, the downloaded file will reflect this and only include the filtered data.

For users who wish to download all listings, the module allows saving in .xlsx format exclusively. This process disregards any local filters, and each listing is saved in a separate worksheet within the file 

Please be aware that the PDF download feature is implemented using an RMarkdown file that is rendered into a PDF through LaTeX. As such, it is important to note that a LaTeX installation, along with the necessary packages, is required to use this feature.
