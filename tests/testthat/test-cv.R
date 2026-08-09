context("cv")

test_that("extended critical values: graceful fallback when unreachable", {
  expect_null(fetch_crit_bucket(700, lag = 0, base_url = "http://127.0.0.1:1/crit2"))
})

test_that("extended critical values: not-yet-simulated combo returns NULL, not an error", {
  skip_on_cran()
  expect_null(fetch_crit_bucket(4999, lag = 3))
})

test_that("extended critical values: fetched from the live store", {
  skip_on_cran()
  cv <- fetch_crit_bucket(601, lag = 0)
  skip_if(is.null(cv), "critical-value store unreachable")
  expect_setequal(
    names(cv),
    c("adf_cv", "sadf_cv", "gsadf_cv", "badf_cv", "bsadf_cv")
  )
  expect_equal(attr(cv, "n"), 601)
  expect_equal(attr(cv, "lag"), 0)
  unlink(crit_cache_path(601, 0))
})

test_that("data instead of n", {
  expect_message(radf_mc_cv(dta), "Did you use")
})

test_that("n positive integer", {
  msg <- "Argument 'n' should be a positive integer"
  expect_error(radf_mc_cv(0, minw = 0), msg)
  expect_error(radf_mc_cv(-1, minw = 0), msg)
})

test_that("nboot positive integer", {
  msg <- "Argument 'nboot' should be a positive integer"
  expect_error(radf_sb_cv(dta, nboot = 0), msg)
  expect_error(radf_sb_cv(dta, nboot = -2), msg)
  expect_error(radf_wb_cv(dta, nboot = 0), msg)
  expect_error(radf_wb_cv(dta, nboot = -2), msg)
})

test_that("minw positive integer", {
  msg <- "Argument 'minw' should be a positive integer"
  expect_error(radf_mc_cv(100, minw = -1), msg)
  expect_error(radf_mc_cv(100, minw = 0), msg)

  expect_error(radf_wb_cv(dta, minw = -1), msg)
  expect_error(radf_wb_cv(dta, minw = 0), msg)
  expect_error(radf_sb_cv(dta, minw = -1), msg)
  expect_error(radf_sb_cv(dta, minw = 0), msg)
})

test_that("n/nboot/minw too small", {
  msg_n <- "Argument 'n' should be greater than '5'"
  msg_minw <- "Argument 'minw' should be greater than '2'"
  msg_nboot <- "Argument 'nboot' should be greater than '2'"

  expect_error(radf_mc_cv(2), msg_n)

  expect_error(radf_mc_cv(100, minw = 2), msg_minw)
  expect_error(radf_wb_cv(dta, minw = 2), msg_minw)
  expect_error(radf_sb_cv(dta, minw = 2), msg_minw)

  expect_error(radf_wb_cv(dta, nboot = 2), msg_nboot)
  expect_error(radf_sb_cv(dta, nboot = 2), msg_nboot)
})

test_that("minw too small", {
  msg <- "Argument 'minw' should be greater than '2'"
  expect_error(radf_mc_cv(100, minw = 2), msg)
  expect_error(radf_wb_cv(dta, minw = 2), msg)
  expect_error(radf_sb_cv(dta, minw = 2), msg)
})

test_that("NA handling", {
  msg <- "rls estimation cannot handle NA"
  expect_error(radf_wb_cv(dta_na), msg)
  expect_error(radf_sb_cv(dta_na), msg)
})

test_that("distribution_rad works", {
  expect_error(invisible(capture.output(
    radf_wb_cv(dta, nboot = 10, dist_rad = TRUE)
  )), regexp = NA)
})


# test_that("show_progress", {
#   options(exuber.show_progress = TRUE)
#   expect_error(capture.output(mc_cv(100, nrep = 10)), NA)
#   expect_error(capture.output(wb_cv(dta, nboot = 10)), NA)
#   expect_error(capture.output(sb_cv(dta, nboot = 10)), NA)
#   options(exuber.show_progress = FALSE)
# })

# test_that("parallel-ncores arguments",{
#   msg <- "Argument 'ncores' is redundant"
#   expect_warning(
#     invisible(capture.output(
#       mc_cv(100, nrep = 10, parallel = FALSE, ncores = 3))), msg)
#   expect_warning(
#     invisible(capture.output(
#       wb_cv(dta, nboot = 10, parallel = FALSE, ncores = 3))), msg)
#   expect_warning(
#     invisible(capture.output(
#       wb_cv(dta, nboot = 10, parallel = FALSE, ncores = 3))), msg)
# })


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
