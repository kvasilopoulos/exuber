context("cv")

test_that("nboot positive integer",{
<<<<<<< HEAD
  expect_error(panel_cv(dta, nboot = 0),
               "Argument 'nboot' should be a positive integer")
  expect_error(panel_cv(dta, nboot = -2),
=======
  expect_error(sb_cv(dta, nboot = 0),
               "Argument 'nboot' should be a positive integer")
  expect_error(sb_cv(dta, nboot = -2),
>>>>>>> pipe friendly version: confrom better with ggplot and S3 methods
               "Argument 'nboot' should be a positive integer")
  expect_error(wb_cv(dta, nboot = 0),
               "Argument 'nboot' should be a positive integer")
})

test_that("minw positive integer", {
<<<<<<< HEAD
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
=======
  expect_error(
    mc_cv(100, minw = -1), "Argument 'minw' should be a positive integer")
  expect_error(
    mc_cv(100, minw = 0), "Argument 'minw' should be a positive integer")
  expect_error(
    wb_cv(dta, minw = -1), "Argument 'minw' should be a positive integer")
  expect_error(
    wb_cv(dta, minw = 0), "Argument 'minw' should be a positive integer")
  expect_error(
    sb_cv(dta, minw = -1), "Argument 'minw' should be a positive integer")
  expect_error(
    sb_cv(dta, minw = 0), "Argument 'minw' should be a positive integer")
})

test_that("minw too small", {
  msg <- "Argument 'minw' should be greater than '2'"
  expect_error(mc_cv(100, minw = 2),msg)
  expect_error(wb_cv(dta, minw = 2), msg)
  expect_error(sb_cv(dta, minw = 2), msg)
>>>>>>> pipe friendly version: confrom better with ggplot and S3 methods
})

test_that("NA handling",{
  expect_error(
    wb_cv(dta_na),"Recursive least square estimation cannot handle NA")
  expect_error(
<<<<<<< HEAD
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

=======
    sb_cv(dta_na),"Recursive least square estimation cannot handle NA")
})

test_that("parallel-ncores arguements",{
  expect_warning(
    invisible(capture.output(
      mc_cv(100, nrep = 10, parallel = FALSE, ncores = 3))),
    "Argument 'ncores' is redundant")
  expect_warning(
    invisible(capture.output(
      wb_cv(dta, nboot = 10, parallel = FALSE, ncores = 3))),
    "Argument 'ncores' is redundant")
  expect_warning(
    invisible(capture.output(
      wb_cv(dta, nboot = 10, parallel = FALSE, ncores = 3))),
    "Argument 'ncores' is redundant")
})

test_that("distribution_rad works", {
  expect_error(invisible(capture.output(
    wb_cv(dta, nboot = 10, dist_rad = TRUE))), regexp = NA)
})

>>>>>>> pipe friendly version: confrom better with ggplot and S3 methods
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
