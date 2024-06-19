# validation (S)
vdoc <- local({
  #                      ##########
  # package_name is used # INSIDE # the sourced file below
  #                      ##########
  package_name <- read.dcf("../../DESCRIPTION")[, "Package"]
  utils_file_path <- system.file("validation", "utils-validation.R", package = package_name, mustWork = TRUE)
  source(utils_file_path, local = TRUE)[["value"]]
})
specs <- vdoc[["specs"]]
#  validation (F)
