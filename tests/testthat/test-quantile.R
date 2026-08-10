context("radf_quantile")

test_that("quantile_adf_tstat matches radf()'s own single-shot adf t-stat
  bit-for-bit -- the critical-value functional Q reuses this, not a new
  computation", {
  set.seed(1)
  y <- cumsum(rnorm(100))
  full <- radf(y, minw = 90)
  q_manual <- exuber:::quantile_adf_tstat(y)
  expect_equal(unname(full$adf), q_manual, tolerance = 1e-8)
})

test_that("radf_quantile runs end to end and returns a well-formed object", {
  set.seed(1)
  y <- cumsum(rnorm(150))
  out <- radf_quantile(y, nrep = 100, seed = 1)

  expect_s3_class(out, "radf_quantile_obj")
  expect_true(is.numeric(out$tstat))
  expect_true(out$tau[["series1"]] >= 0.2 && out$tau[["series1"]] <= 0.8)
  expect_true(out$delta[["series1"]] >= -1 && out$delta[["series1"]] <= 1)
  expect_output(print(out), "radf_quantile")
})

test_that("radf_quantile runs with a fixed tau", {
  set.seed(1)
  y <- cumsum(rnorm(100))
  out <- radf_quantile(y, tau = 0.5, nrep = 100, seed = 1)
  expect_equal(unname(out$tau[["series1"]]), 0.5)
})

test_that("radf_quantile rejects a tau outside (0, 1)", {
  y <- cumsum(rnorm(100))
  expect_error(radf_quantile(y, tau = 1.5, nrep = 100))
})

test_that("radf_quantile rejects a non-tabulated significance level", {
  y <- cumsum(rnorm(100))
  expect_error(radf_quantile(y, level = 93, nrep = 100))
})

test_that("radf_quantile detects a genuine explosive series far more
  often than a pure random walk", {
  skip_on_cran()
  run_explosive <- function(seed) {
    set.seed(seed)
    n1 <- 60
    y <- 100 * 1.03^(1:n1) + cumsum(rnorm(n1, sd = 1))
    radf_quantile(y, nrep = 100, seed = 1)$detected[["series1"]]
  }
  run_null <- function(seed) {
    set.seed(seed)
    y <- cumsum(rnorm(100))
    radf_quantile(y, nrep = 100, seed = 1)$detected[["series1"]]
  }
  rate_explosive <- mean(sapply(1:25, run_explosive))
  rate_null <- mean(sapply(1:25, run_null))
  expect_gt(rate_explosive, rate_null)
  expect_gt(rate_explosive, 0.5)
})

test_that("radf_quantile false-detection rate under pure H0 is in a
  plausible range around the nominal level (loose Monte Carlo bound)", {
  skip_on_cran()
  run_null <- function(seed) {
    set.seed(seed)
    y <- cumsum(rnorm(100))
    radf_quantile(y, nrep = 100, seed = 1)$detected[["series1"]]
  }
  rate <- mean(sapply(1:40, run_null))
  expect_true(rate <= 0.25)
})
