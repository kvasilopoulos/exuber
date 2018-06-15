context("cv")

# with_parallel <- function(code) {
#   skip_on_cran()
#   doParallel::registerDoParallel(cores = 2)
#   on.exit(doParallel::stopImplicitCluster())
#   code
# }

test_that("minw check cv", {
  expect_error(
    mc_cv(100, minw = -1),
    "Argument 'minw' should be a positive integer"
  )
  expect_error(
    mc_cv(100, minw = 0),
    "Argument 'minw' should be a positive integer"
  )
  expect_error(mc_cv(100, minw = 2), "Argument 'minw' is too small")
  expect_error(
    wb_cv(dta, minw = -1),
    "Argument 'minw' should be a positive integer"
  )
  expect_error(
    wb_cv(dta, minw = 0),
    "Argument 'minw' should be a positive integer"
  )
  expect_error(wb_cv(dta, minw = 2), "Argument 'minw' is too small")
})

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

test_that("distribution_rad works", {
  expect_error(invisible(capture.output(
    wb_cv(dta, 10, dist_rad = TRUE))), regexp = NA)
})
