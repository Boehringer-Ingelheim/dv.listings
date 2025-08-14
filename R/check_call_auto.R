# Automatically generated module API check functions. Think twice before editing them manually.
({
# styler: off

# dv.listings::mod_listings
check_mod_listings_auto <- function(afmm, datasets, module_id, dataset_names, default_vars, pagination,
    intended_use_label, subjid_var, receiver_id, review, warn, err) {
    OK <- logical(0)
    used_dataset_names <- new.env(parent = emptyenv())
    OK[["module_id"]] <- CM$check_module_id("module_id", module_id, warn, err)
    flags <- list(one_or_more = TRUE)
    OK[["dataset_names"]] <- CM$check_dataset_name("dataset_names", dataset_names, flags, datasets, used_dataset_names,
        warn, err)
    "NOTE: default_vars (group) tagged as \"manual_check\""
    "      The expectation is that it either does not require automated checks or that"
    "      the caller of this function has written manual checks near the call site."
    "NOTE: pagination (logical) tagged as \"manual_check\""
    "      The expectation is that it either does not require automated checks or that"
    "      the caller of this function has written manual checks near the call site."
    "NOTE: intended_use_label (character) tagged as \"manual_check\""
    "      The expectation is that it either does not require automated checks or that"
    "      the caller of this function has written manual checks near the call site."
    "NOTE: subjid_var (character) tagged as \"manual_check\""
    "      The expectation is that it either does not require automated checks or that"
    "      the caller of this function has written manual checks near the call site."
    "NOTE: receiver_id (character) tagged as \"manual_check\""
    "      The expectation is that it either does not require automated checks or that"
    "      the caller of this function has written manual checks near the call site."
    "NOTE: review (group) tagged as \"manual_check\""
    "      The expectation is that it either does not require automated checks or that"
    "      the caller of this function has written manual checks near the call site."
    return(OK)
}

})
# styler: on
