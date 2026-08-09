context("radf_monitor")

test_that("radf_monitor runs end to end and returns a well-formed object", {
  set.seed(1)
  n <- 100
  y <- cumsum(rnorm(n))
  out <- radf_monitor(y, r_star = 0.5, minw = 20, nboot = 99, seed = 1)

  expect_s3_class(out, "radf_monitor_obj")
  expect_true(is.matrix(out$bsadf))
  expect_true(is.numeric(out$boundary))
  expect_equal(out$T_star, 50)
  expect_output(print(out), "radf_monitor")
})

test_that("'r_star' must leave room for both a training window and at
  least one monitoring observation", {
  y <- cumsum(rnorm(60))
  expect_error(radf_monitor(y, r_star = 5, minw = 20, nboot = 20))
  expect_error(radf_monitor(y, r_star = 60, minw = 20, nboot = 20))
})

test_that("an alarm, when raised, always falls strictly after the
  training window T_star -- monitoring must never fire on training data", {
  skip_on_cran()
  run_once <- function(seed) {
    set.seed(seed)
    n1 <- 75; n2 <- 40
    normal_part <- cumsum(rnorm(n1))
    expl_part <- normal_part[n1] * 1.05^(1:n2) + cumsum(rnorm(n2, sd = 0.3))
    y <- c(normal_part, expl_part)
    out <- radf_monitor(y, r_star = n1 / length(y), minw = 20, nboot = 99, seed = 1)
    alarm <- unname(out$alarm)
    if (is.na(alarm)) NA else alarm > out$T_star
  }
  results <- sapply(1:10, run_once)
  expect_true(all(na.omit(results)))
})

test_that("radf_monitor detects a clear bubble starting strictly after the
  training window, with a positive (not negative or absurdly large) alarm
  delay relative to the true origination date", {
  skip_on_cran()
  run_once <- function(seed) {
    set.seed(seed)
    n1 <- 75; n2 <- 40
    normal_part <- cumsum(rnorm(n1))
    expl_part <- normal_part[n1] * 1.05^(1:n2) + cumsum(rnorm(n2, sd = 0.3))
    y <- c(normal_part, expl_part)
    out <- radf_monitor(y, r_star = n1 / length(y), minw = 20, nboot = 99, seed = 1)
    c(alarm = unname(out$alarm), true_origination = n1)
  }
  res <- t(sapply(1:15, run_once))
  detected <- !is.na(res[, "alarm"])
  expect_gt(mean(detected), 0.5)
  delay <- res[detected, "alarm"] - res[detected, "true_origination"]
  expect_true(all(delay >= 0))
  expect_true(all(delay < 40))
})
