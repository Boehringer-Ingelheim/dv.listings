test_that("mod_simple_listing displays a listing", {
  app <- shinytest2::AppDriver$new(app_dir = "apps/simple_listing", name = "test_dropdown_labels")
  app$wait_for_idle()
  expect_snapshot(app$get_html(".datatables"))
})
