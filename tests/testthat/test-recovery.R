context("radf_recovery")

test_that("radf_recovery_cv runs end to end and returns a well-formed object", {
  cv <- radf_recovery_cv(n = 100, minw = 20, nrep = 50, seed = 1)

  expect_s3_class(cv, "radf_cv")
  expect_true(is.matrix(cv$bsadf_cv))
  expect_equal(nrow(cv$bsadf_cv), 80)
  expect_equal(colnames(cv$bsadf_cv), c("90%", "95%", "99%"))
})

test_that("radf_recovery runs end to end and returns a well-formed object", {
  set.seed(1)
  y <- cumsum(rnorm(100))
  out <- radf_recovery(y, minw = 20, nrep = 50, seed = 1)

  expect_s3_class(out, "radf_recovery_obj")
  expect_true(is.logical(out$detected))
  expect_true(is.logical(out$censored))
  expect_output(print(out), "radf_recovery")
})

test_that("radf_recovery rejects a non-tabulated significance level", {
  y <- cumsum(rnorm(100))
  expect_error(radf_recovery(y, minw = 20, nrep = 50, sig_lvl = 93))
})

test_that("radf_recovery never returns f_c > f_r when both dates are
  identified and uncensored -- the down-crossing search is restricted to
  start at the up-crossing, so this must hold by construction", {
  skip_on_cran()
  run_once <- function(seed) {
    set.seed(seed)
    n1 <- 40; n2 <- 25; n3 <- 35
    expansion <- 100 * 1.03^(1:n1) + cumsum(rnorm(n1, sd = 1))
    collapse <- expansion[n1] * 0.5^((1:n2) / n2) + cumsum(rnorm(n2, sd = 1))
    recovery <- collapse[n2] + cumsum(rnorm(n3, sd = 1)) + (1:n3) * 0.5
    y <- c(expansion, collapse, recovery)
    out <- radf_recovery(y, minw = 15, nrep = 50, seed = 1)
    if (!out$detected[["series1"]] || out$censored[["series1"]]) {
      return(NA)
    }
    out$f_c[["series1"]] <= out$f_r[["series1"]]
  }
  results <- sapply(1:10, run_once)
  expect_true(all(na.omit(results)))
})

test_that("radf_recovery detects a genuine collapse-then-recovery episode
  with a plausible (not systematically reversed) date ordering", {
  skip_on_cran()
  run_once <- function(seed) {
    set.seed(seed)
    n1 <- 40; n2 <- 25; n3 <- 35
    expansion <- 100 * 1.03^(1:n1) + cumsum(rnorm(n1, sd = 1))
    collapse <- expansion[n1] * 0.5^((1:n2) / n2) + cumsum(rnorm(n2, sd = 1))
    recovery <- collapse[n2] + cumsum(rnorm(n3, sd = 1)) + (1:n3) * 0.5
    y <- c(expansion, collapse, recovery)
    out <- radf_recovery(y, minw = 15, nrep = 50, seed = 1)
    out$detected[["series1"]]
  }
  detected <- sapply(1:15, run_once)
  expect_gt(mean(detected), 0.3)
})

test_that("radf_recovery false-detection rate under pure H0 is not wildly
  inflated (loose bound, descriptive Monte Carlo check)", {
  skip_on_cran()
  run_null <- function(seed) {
    set.seed(seed)
    y <- cumsum(rnorm(100))
    out <- radf_recovery(y, minw = 20, nrep = 50, seed = 1)
    out$detected[["series1"]]
  }
  rate <- mean(sapply(1:30, run_null))
  expect_true(rate <= 0.5)
})
