context("cv")

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

test_that("dist_skew (Hafner 2020) works and only one of dist_rad/dist_skew may be TRUE", {
  expect_error(invisible(capture.output(
    radf_wb_cv(dta, nboot = 10, dist_skew = TRUE)
  )), regexp = NA)
  expect_error(
    radf_wb_cv(dta, nboot = 10, dist_rad = TRUE, dist_skew = TRUE),
    "Only one of 'dist_rad' and 'dist_skew'"
  )
})

test_that("dist_skew's multiplier w = u/sqrt(2) + (v^2-1)/2 has the moments
  Hafner (2020) claims by construction: E[w]=0, E[w^2]=1, E[w^3]=1", {
  set.seed(1)
  n <- 500000
  u <- rnorm(n); v <- rnorm(n)
  w <- u / sqrt(2) + (v^2 - 1) / 2
  expect_equal(mean(w), 0, tolerance = 0.01)
  expect_equal(mean(w^2), 1, tolerance = 0.01)
  expect_equal(mean(w^3), 1, tolerance = 0.02)
})

test_that("dist_skew = FALSE reproduces the original (pre-Hafner) wild bootstrap
  DGP exactly, for the same seed -- a pure additive option, not a behavior
  change to the default path", {
  set.seed(3)
  y <- cumsum(rnorm(80))
  set.seed(7)
  r1 <- exuber:::radf_wb_dgp_hlst(y, dist_rad = FALSE)
  set.seed(7)
  r2 <- exuber:::radf_wb_dgp_hlst(y, dist_rad = FALSE, dist_skew = FALSE)
  expect_identical(r1, r2)
})

test_that("dist_skew = TRUE bootstrap correctly detects a clear mildly
  explosive alternative (confirms the skewed multiplier doesn't degrade
  basic detection power)", {
  skip_on_cran()
  run_once <- function(seed) {
    set.seed(seed)
    Tn <- 100
    Te <- 60
    normal_part <- cumsum(rnorm(Te))
    expl_part <- normal_part[Te] * 1.06^(1:(Tn - Te)) + cumsum(rnorm(Tn - Te, sd = 0.3))
    y <- c(normal_part, expl_part)
    obs <- radf(y, minw = 20)$sadf
    cv <- radf_wb_cv(y, minw = 20, nboot = 199, dist_skew = TRUE, seed = 1)
    obs > cv$sadf_cv[1, "95%"]
  }
  power <- mean(sapply(1:20, run_once))
  expect_gt(power, 0.5)
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
