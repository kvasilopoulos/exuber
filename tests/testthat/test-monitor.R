context("monitor_radf")

test_that("monitor_radf runs end to end and returns a well-formed object", {
  set.seed(1)
  n <- 100
  y <- cumsum(rnorm(n))
  out <- monitor_radf(y, r_star = 0.5, minw = 20, nboot = 99, seed = 1)

  expect_s3_class(out, "monitor_radf_obj")
  expect_true(is.matrix(out$stat))
  expect_true(is.numeric(out$boundary))
  expect_equal(out$T_star, 50)
  expect_output(print(out), "monitor_radf")
})

test_that("'r_star' must leave room for both a training window and at
  least one monitoring observation", {
  y <- cumsum(rnorm(60))
  expect_error(monitor_radf(y, r_star = 5, minw = 20, nboot = 20))
  expect_error(monitor_radf(y, r_star = 60, minw = 20, nboot = 20))
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
    out <- monitor_radf(y, r_star = n1 / length(y), minw = 20, nboot = 99, seed = 1)
    alarm <- unname(out$alarm)
    if (is.na(alarm)) NA else alarm > out$T_star
  }
  results <- sapply(1:10, run_once)
  expect_true(all(na.omit(results)))
})

test_that("monitor_radf detects a clear bubble starting strictly after the
  training window, with a positive (not negative or absurdly large) alarm
  delay relative to the true origination date", {
  skip_on_cran()
  run_once <- function(seed) {
    set.seed(seed)
    n1 <- 75; n2 <- 40
    normal_part <- cumsum(rnorm(n1))
    expl_part <- normal_part[n1] * 1.05^(1:n2) + cumsum(rnorm(n2, sd = 0.3))
    y <- c(normal_part, expl_part)
    out <- monitor_radf(y, r_star = n1 / length(y), minw = 20, nboot = 99, seed = 1)
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

test_that("monitor_radf runs end to end with boundary = 'kurozumi'", {
  set.seed(1)
  y <- cumsum(rnorm(150))
  out <- monitor_radf(y, r_star = 0.5, minw = 20, boundary = "kurozumi", level = 0.95)

  expect_s3_class(out, "monitor_radf_obj")
  expect_true(is.matrix(out$stat))
  expect_equal(unname(out$boundary), 1.0381)
  expect_output(print(out), "kurozumi")
})

test_that("boundary = 'kurozumi' rejects levels outside its tabulated set", {
  y <- cumsum(rnorm(100))
  expect_error(monitor_radf(y, r_star = 0.5, minw = 20, boundary = "kurozumi", level = 0.93))
})

test_that("boundary = 'kurozumi' false-alarm rate under H0 is in a plausible
  range around its nominal level (not near-zero or wildly inflated)", {
  skip_on_cran()
  run_null <- function(seed) {
    set.seed(seed)
    y <- cumsum(rnorm(150))
    out <- monitor_radf(y, r_star = 0.5, minw = 20, boundary = "kurozumi", level = 0.95)
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
    out <- monitor_radf(y, r_star = n1 / length(y), minw = 20, boundary = "kurozumi")
    alarm <- unname(out$alarm)
    if (is.na(alarm)) NA else alarm > out$T_star
  }
  results <- sapply(1:10, run_once)
  expect_true(all(na.omit(results)))
})

test_that("kurozumi_gsadf_q looks up Kurozumi (2020) Table 1's GSADF_{s0}
  columns (q04_df/q08_df) exactly, snapping s0 to the nearest of {0.4, 0.8}", {
  expect_equal(exuber:::kurozumi_gsadf_q(0.90, 1, 0.4), 1.3969)
  expect_equal(exuber:::kurozumi_gsadf_q(0.95, 1, 0.4), 1.8081)
  expect_equal(exuber:::kurozumi_gsadf_q(0.99, 1, 0.4), 2.5927)
  expect_equal(exuber:::kurozumi_gsadf_q(0.95, 1, 0.8), 2.3330)
  expect_equal(exuber:::kurozumi_gsadf_q(0.95, 3, 0.4), 2.0737)
  expect_equal(exuber:::kurozumi_gsadf_q(0.95, 1, 0.6), 1.8081) # tie snaps to first (0.4)
  expect_error(exuber:::kurozumi_gsadf_q(0.93, 1, 0.4))
})

test_that("kurozumi_gsadf_stat's closed-form (with-intercept) ADF t-statistic
  band matches radf()$badf exactly at k1_max = 1 (s0 -> 0 limit)", {
  set.seed(1)
  y <- cumsum(rnorm(80))
  minw <- 20
  badf <- radf(y, minw = minw, lag = 0)$badf[, 1]
  # s0 tiny enough that floor(T_star * s0) == 1, T_star = minw so k1_max = 1
  stat <- exuber:::kurozumi_gsadf_stat(y, T_star = minw, s0 = 1 / minw)
  expect_equal(unname(stat), badf, tolerance = 1e-8)
})

test_that("kurozumi_gsadf_stat matches a brute-force lm() search over the
  restricted window-start band exactly", {
  set.seed(2)
  y <- cumsum(rnorm(150))
  T_star <- 75
  s0 <- 0.4
  k1_max <- floor(T_star * s0)
  stat <- exuber:::kurozumi_gsadf_stat(y, T_star, s0)
  for (k_check in c(5, 30, 60)) {
    t_check <- T_star + k_check
    brute <- sapply(seq_len(k1_max), function(k1) {
      yy <- y[k1:t_check]
      fit <- lm(diff(yy) ~ yy[-length(yy)])
      summary(fit)$coefficients[2, 3]
    })
    expect_equal(stat[k_check], max(brute), tolerance = 1e-8, ignore_attr = TRUE)
  }
})

test_that("monitor_radf(boundary = 'kurozumi', s0 = 0.4/0.8) runs end to end,
  matches the published GSADF boundary constant, and alarms never fire
  before T_star", {
  set.seed(1)
  y <- cumsum(rnorm(150))
  out <- monitor_radf(y, r_star = 0.5, boundary = "kurozumi", s0 = 0.4, level = 0.95)

  expect_s3_class(out, "monitor_radf_obj")
  expect_true(is.matrix(out$stat))
  expect_equal(nrow(out$stat), length(out$boundary))
  expect_equal(attr(out, "q"), 1.8081)
  expect_equal(attr(out, "s0"), 0.4)
  expect_output(print(out), "kurozumi")
  expect_true(is.na(out$alarm) || out$alarm > out$T_star)
})

test_that("monitor_radf(boundary = 'kurozumi', s0 = 0) is unchanged (s0 = 0
  is the default and reproduces the original SADF-only behavior)", {
  set.seed(1)
  y <- cumsum(rnorm(150))
  out_default <- monitor_radf(y, r_star = 0.5, minw = 20, boundary = "kurozumi", level = 0.95)
  out_explicit <- monitor_radf(y, r_star = 0.5, minw = 20, boundary = "kurozumi", s0 = 0, level = 0.95)
  expect_equal(out_default$stat, out_explicit$stat)
  expect_equal(out_default$boundary, out_explicit$boundary)
})

test_that("boundary = 'kurozumi', s0 = 0.4 false-alarm rate under H0 is close
  to nominal, and detection power is comparable to the s0 = 0 (SADF) case", {
  skip_on_cran()
  set.seed(10)
  nrep <- 100
  n <- 150
  T_star <- 75
  fa <- mean(vapply(seq_len(nrep), function(i) {
    set.seed(1000 + i)
    y <- cumsum(rnorm(n))
    !is.na(monitor_radf(y, r_star = T_star, boundary = "kurozumi", s0 = 0.4, level = 0.95)$alarm)
  }, logical(1)))
  expect_lt(fa, 0.15)

  make_bubble_series <- function(n, T_star, bstart, rho = 1.03) {
    y <- numeric(n)
    y[seq_len(T_star)] <- cumsum(rnorm(T_star))
    for (t in (T_star + 1):n) {
      y[t] <- if (t < bstart) y[t - 1] + rnorm(1) else rho * y[t - 1] + rnorm(1)
    }
    y
  }
  run <- function(seed) {
    set.seed(seed)
    y <- make_bubble_series(n, T_star, bstart = 130)
    c(
      sadf = !is.na(unname(monitor_radf(y, r_star = T_star, boundary = "kurozumi", s0 = 0)$alarm)),
      gsadf = !is.na(unname(monitor_radf(y, r_star = T_star, boundary = "kurozumi", s0 = 0.4)$alarm))
    )
  }
  res <- rowMeans(sapply(1:40, run))
  expect_gt(res[["gsadf"]], 0.15)
  expect_lt(abs(res[["gsadf"]] - res[["sadf"]]), 0.35)
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

test_that("monitor_radf runs end to end with boundary = 'fluc'", {
  set.seed(1)
  y <- cumsum(rnorm(150))
  out <- monitor_radf(y, r_star = 0.5, minw = 20, boundary = "fluc", level = 0.95)

  expect_s3_class(out, "monitor_radf_obj")
  expect_true(is.matrix(out$stat))
  expect_output(print(out), "fluc")
})

test_that("boundary = 'fluc' rejects levels outside its tabulated set", {
  y <- cumsum(rnorm(100))
  expect_error(monitor_radf(y, r_star = 0.5, minw = 20, boundary = "fluc", level = 0.93))
})

test_that("boundary = 'fluc' false-alarm rate under H0 is not wildly
  inflated (loose Monte Carlo bound; HB's own boundary is conservative)", {
  skip_on_cran()
  run_null <- function(seed) {
    set.seed(seed)
    y <- cumsum(rnorm(150))
    out <- monitor_radf(y, r_star = 0.5, minw = 20, boundary = "fluc", level = 0.95)
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
    out <- monitor_radf(y, r_star = n1 / length(y), minw = 20, boundary = "fluc")
    alarm <- unname(out$alarm)
    if (is.na(alarm)) NA else alarm > out$T_star
  }
  results <- sapply(1:10, run_once)
  expect_true(all(na.omit(results)))
})
