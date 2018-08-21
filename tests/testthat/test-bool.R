test_that("bool diagnostics", {
  expect_error(diagnostics(radf_dta, mc, bool = TRUE), regexp = NA)
  ds <- c(1,1,1,0,1); names(ds) <- col_names(radf_dta)
  expect_equal(diagnostics(radf_dta, mc, bool = TRUE), ds)
})

test_that("bool diagnostics panel", {
  expect_error(diagnostics(radf_dta, mc, bool = TRUE, panel = TRUE), NA)
  ds <- 1; names(ds) <- "Panel"
  expect_equal(diagnostics(radf_dta, mc, bool = TRUE, panel = TRUE), ds)
})


test_that("bool datestamp", {
  expect_error(datestamp(radf_dta, mc, bool = TRUE), regexp = NA)
  expect_error(datestamp(radf_dta, mc, bool = TRUE, panel = TRUE), regexp = NA)
})
