use_load_all <- (isTRUE(as.logical(Sys.getenv("TEST_LOCAL"))))

if (use_load_all) {
  pkg_path <- "."
  prev_path <- ""
  while (!length(list.files(pkg_path, pattern = "^DESCRIPTION$")) == 1) {
    if (normalizePath(pkg_path) == prev_path) rlang::abort("root folder reached and no DESCRIPTION file found")
    prev_path <- normalizePath(pkg_path)
    pkg_path <- file.path("..", pkg_path)
  }
  devtools::load_all(pkg_path, quiet = TRUE)
}

dv.listings:::mock_simple_listing_mm()
