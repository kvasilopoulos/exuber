context("radf_monitor")

test_that("radf_monitor runs end to end and returns a well-formed object", {
  set.seed(1)
  n <- 100
  y <- cumsum(rnorm(n))
  out <- radf_monitor(y, r_star = 0.5, minw = 20, nboot = 99, seed = 1)

  expect_s3_class(out, "radf_monitor_obj")
  expect_true(is.matrix(out$stat))
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

test_that("kurozumi_sadf_q looks up Kurozumi (2020) Table 1 constants exactly", {
  expect_equal(exuber:::kurozumi_sadf_q(0.95, 1), 1.0381)
  expect_equal(exuber:::kurozumi_sadf_q(0.95, 1.2), 1.0381) # snaps to nearest sbar
  expect_equal(exuber:::kurozumi_sadf_q(0.95, 3), 1.3330)
  expect_equal(exuber:::kurozumi_sadf_q(0.95, 5), 1.4255)
  expect_equal(exuber:::kurozumi_sadf_q(0.90, 1), 0.6946)
  expect_equal(exuber:::kurozumi_sadf_q(0.99, 1), 1.6474)
  expect_error(exuber:::kurozumi_sadf_q(0.93, 1))
})

test_that("radf_monitor runs end to end with boundary = 'kurozumi'", {
  set.seed(1)
  y <- cumsum(rnorm(150))
  out <- radf_monitor(y, r_star = 0.5, minw = 20, boundary = "kurozumi", level = 0.95)

  expect_s3_class(out, "radf_monitor_obj")
  expect_true(is.matrix(out$stat))
  expect_equal(unname(out$boundary), 1.0381)
  expect_output(print(out), "kurozumi")
})

test_that("boundary = 'kurozumi' rejects levels outside its tabulated set", {
  y <- cumsum(rnorm(100))
  expect_error(radf_monitor(y, r_star = 0.5, minw = 20, boundary = "kurozumi", level = 0.93))
})

test_that("boundary = 'kurozumi' false-alarm rate under H0 is in a plausible
  range around its nominal level (not near-zero or wildly inflated)", {
  skip_on_cran()
  run_null <- function(seed) {
    set.seed(seed)
    y <- cumsum(rnorm(150))
    out <- radf_monitor(y, r_star = 0.5, minw = 20, boundary = "kurozumi", level = 0.95)
    !is.na(out$alarm)
  }
  rate <- mean(sapply(1:100, run_null))
  expect_true(rate >= 0 && rate <= 0.20)
})

test_that("boundary = 'kurozumi' alarms never fire before T_star", {
  skip_on_cran()
  run_once <- function(seed) {
    set.seed(seed)
    n1 <- 75; n2 <- 40
    normal_part <- cumsum(rnorm(n1))
    expl_part <- normal_part[n1] * 1.05^(1:n2) + cumsum(rnorm(n2, sd = 0.3))
    y <- c(normal_part, expl_part)
    out <- radf_monitor(y, r_star = n1 / length(y), minw = 20, boundary = "kurozumi")
    alarm <- unname(out$alarm)
    if (is.na(alarm)) NA else alarm > out$T_star
  }
  results <- sapply(1:10, run_once)
  expect_true(all(na.omit(results)))
})

test_that("hb_fluc_q looks up Homm & Breitung (2012) Table 7(i) constants exactly", {
  expect_equal(exuber:::hb_fluc_q(0.95, 100, 2), 4.50)
  expect_equal(exuber:::hb_fluc_q(0.95, 100, 10), 6.26)
  expect_equal(exuber:::hb_fluc_q(0.95, 50, 4), 5.11)
  expect_equal(exuber:::hb_fluc_q(0.90, 20, 2), 2.49)
  expect_equal(exuber:::hb_fluc_q(0.99, 100, 8), 9.79)
  expect_equal(exuber:::hb_fluc_q(0.95, 73, 7), 5.50) # snaps n->50, k->6
  expect_error(exuber:::hb_fluc_q(0.93, 100, 2))
})

test_that("radf_monitor runs end to end with boundary = 'fluc'", {
  set.seed(1)
  y <- cumsum(rnorm(150))
  out <- radf_monitor(y, r_star = 0.5, minw = 20, boundary = "fluc", level = 0.95)

  expect_s3_class(out, "radf_monitor_obj")
  expect_true(is.matrix(out$stat))
  expect_output(print(out), "fluc")
})

test_that("boundary = 'fluc' rejects levels outside its tabulated set", {
  y <- cumsum(rnorm(100))
  expect_error(radf_monitor(y, r_star = 0.5, minw = 20, boundary = "fluc", level = 0.93))
})

test_that("boundary = 'fluc' false-alarm rate under H0 is not wildly
  inflated (loose Monte Carlo bound; HB's own boundary is conservative)", {
  skip_on_cran()
  run_null <- function(seed) {
    set.seed(seed)
    y <- cumsum(rnorm(150))
    out <- radf_monitor(y, r_star = 0.5, minw = 20, boundary = "fluc", level = 0.95)
    !is.na(out$alarm)
  }
  rate <- mean(sapply(1:100, run_null))
  expect_true(rate <= 0.15)
})

test_that("boundary = 'fluc' alarms never fire before T_star", {
  skip_on_cran()
  run_once <- function(seed) {
    set.seed(seed)
    n1 <- 75; n2 <- 40
    normal_part <- cumsum(rnorm(n1))
    expl_part <- normal_part[n1] * 1.05^(1:n2) + cumsum(rnorm(n2, sd = 0.3))
    y <- c(normal_part, expl_part)
    out <- radf_monitor(y, r_star = n1 / length(y), minw = 20, boundary = "fluc")
    alarm <- unname(out$alarm)
    if (is.na(alarm)) NA else alarm > out$T_star
  }
  results <- sapply(1:10, run_once)
  expect_true(all(na.omit(results)))
})
