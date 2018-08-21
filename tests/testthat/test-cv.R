context("cv")

test_that("nboot positive integer",{
  expect_error(panel_cv(dta, nboot = 0),
               "Argument 'nboot' should be a positive integer")
  expect_error(panel_cv(dta, nboot = -2),
               "Argument 'nboot' should be a positive integer")
  expect_error(wb_cv(dta, nboot = 0),
               "Argument 'nboot' should be a positive integer")
})

test_that("minw positive integer", {
  expect_error(
    mc_cv(100, minw = -1), "Argument 'minw' should be a positive integer")
  expect_error(
    mc_cv(100, minw = 0), "Argument 'minw' should be a positive integer")
  expect_error(
    wb_cv(dta, minw = -1), "Argument 'minw' should be a positive integer")
  expect_error(
    wb_cv(dta, minw = 0), "Argument 'minw' should be a positive integer")
  expect_error(
    panel_cv(dta, minw = -1), "Argument 'minw' should be a positive integer")
  expect_error(
    panel_cv(dta, minw = 0), "Argument 'minw' should be a positive integer")
})

test_that("minw too small", {
  expect_error(mc_cv(100, minw = 2), "Argument 'minw' is too small")
  expect_error(wb_cv(dta, minw = 2), "Argument 'minw' is too small")
  expect_error(panel_cv(dta, minw = 2), "Argument 'minw' is too small")
})

test_that("NA handling",{
  expect_error(
    wb_cv(dta_na),"Recursive least square estimation cannot handle NA")
  expect_error(
    panel_cv(dta_na),"Recursive least square estimation cannot handle NA")
})

test_that("parallel-ncores arguements",{
  expect_error(
    mc_cv(100, parallel = FALSE, ncores = 3),
    "Argument 'ncores' is redundant when 'parallel' is set to 'FALSE'")
  expect_error(
    wb_cv(dta, parallel = FALSE, ncores = 3),
    "Argument 'ncores' is redundant when 'parallel' is set to 'FALSE'")
  expect_error(
    panel_cv(dta, parallel = FALSE, ncores = 3),
    "Argument 'ncores' is redundant when 'parallel' is set to 'FALSE'")
})

test_that("distribution_rad works", {
  expect_error(invisible(capture.output(
    wb_cv(dta, 10, dist_rad = TRUE))), regexp = NA)
})

# with_parallel <- function(code) {
#   skip_on_cran()
#   doParallel::registerDoParallel(cores = 2)
#   on.exit(doParallel::stopImplicitCluster())
#   code
# }

# test_that("parallel works", {
#   skip_on_travis()
#   skip_on_cran()
#   with_parallel({
#   expect_error(invisible(capture.output(mc_cv(100, 12, parallel = TRUE))),
#     regexp = NA)
#   expect_error(invisible(capture.output(wb_cv(dta, 12, parallel = TRUE))),
#     regexp = NA)
#   expect_error(
#     invisible(capture.output(
#       wb_cv(dta, 12, parallel = TRUE, dist_rad = TRUE))), regexp = NA)
#   })
# })
