context("radf_lbi")

test_that("Breitung & Diegel's eq. 4 telescoping identity holds exactly
  (2*sum(Delta y_t * y_{t-1}) = y_T^2 - T*sigma_tilde^2, y_1 = 0 case)", {
  set.seed(1)
  T <- 100
  y <- c(0, cumsum(rnorm(T)))
  dy <- diff(y)
  ylag <- y[1:T]
  lhs <- 2 * sum(dy * ylag)
  sigma2_tilde <- mean(dy^2)
  rhs <- y[T + 1]^2 - T * sigma2_tilde
  expect_equal(lhs, rhs, tolerance = 1e-8)
})

test_that("radf_lbi runs end to end and returns a well-formed object", {
  set.seed(1)
  y <- cumsum(rnorm(100))
  out <- lbi_test(y)

  expect_s3_class(out, "lbi_test_obj")
  expect_true(is.numeric(out$stat[["series1"]]))
  expect_equal(out$crit, qnorm(0.95))
  expect_output(print(out), "lbi_test")
})

test_that("radf_lbi's statistic follows a standard normal distribution
  under H0, matching Breitung & Diegel's own claimed null distribution
  (not just an approximately-sized test)", {
  skip_on_cran()
  run_stat <- function(seed) {
    set.seed(seed)
    y <- cumsum(rnorm(100))
    lbi_test(y)$stat[["series1"]]
  }
  stats <- sapply(1:300, run_stat)
  expect_true(abs(mean(stats)) < 0.15)
  expect_true(abs(sd(stats) - 1) < 0.15)
  expect_gt(ks.test(stats, "pnorm")$p.value, 0.01)
})

test_that("radf_lbi detects a genuine explosive series with power
  comparable to a standard SADF test on the same DGP", {
  skip_on_cran()
  run_lbi <- function(seed) {
    set.seed(seed)
    n1 <- 60
    y <- 100 * 1.03^(1:n1) + cumsum(rnorm(n1, sd = 1))
    lbi_test(y)$detected[["series1"]]
  }
  rate <- mean(sapply(1:30, run_lbi))
  expect_gt(rate, 0.8)
})

test_that("bd_cusum_weights sums of squares equal 1 (exact at c_bar = 0,
  approximately for c_bar > 0 -- eq. 12's own continuum-limit
  normalization)", {
  w0 <- exuber:::bd_cusum_weights(500, 0)
  expect_equal(sum(w0^2), 1, tolerance = 1e-12)
  for (cb in c(1, 2, 5)) {
    w <- exuber:::bd_cusum_weights(500, cb)
    expect_equal(sum(w^2), 1, tolerance = 0.02)
  }
})

test_that("bd_cusum_q looks up Breitung & Diegel's Table 1 exactly and
  errors on an untabulated level", {
  expect_equal(exuber:::bd_cusum_q(0.90), 1.64)
  expect_equal(exuber:::bd_cusum_q(0.95), 1.95)
  expect_equal(exuber:::bd_cusum_q(0.995), 2.80)
  expect_error(exuber:::bd_cusum_q(0.80), "must be one of")
})

test_that("radf_lbi_monitor's mCUSUM (c_bar = 0) final-point statistic is
  formula-exact against a manual telescoped computation using
  training-window sigma_tilde", {
  set.seed(1)
  n <- 300
  T_star <- 150
  y <- cumsum(rnorm(n))
  out <- monitor_lbi(y, r_star = T_star, c_bar = 0, level = 0.95)
  dy <- diff(y)
  sigma2_tilde <- mean(dy[seq_len(T_star - 1L)]^2)
  manual <- (y[n] - y[T_star]) / sqrt(sigma2_tilde * (n - T_star))
  expect_equal(unname(out$stat[nrow(out$stat), 1]), manual, tolerance = 1e-10)
})

test_that("radf_lbi_monitor runs end to end and returns a well-formed
  object; alarms never fire before T_star + 1", {
  set.seed(1)
  y <- cumsum(rnorm(200))
  out <- monitor_lbi(y, r_star = 100, c_bar = 0, level = 0.95)

  expect_s3_class(out, "monitor_lbi_obj")
  expect_equal(out$boundary, 1.95)
  expect_equal(out$T_star, 100)
  expect_true(is.na(out$alarm[["series1"]]) || out$alarm[["series1"]] > 100)
  expect_output(print(out), "monitor_lbi")
})

test_that("radf_lbi_monitor errors on an untabulated level and on too-short
  training/monitoring windows", {
  y <- cumsum(rnorm(200))
  expect_error(monitor_lbi(y, level = 0.80), "must be one of")
  expect_error(monitor_lbi(y, r_star = 2), "too short")
  expect_error(monitor_lbi(y, r_star = 200), "leave at least one")
})

test_that("radf_lbi_monitor's mCUSUM/wCUSUM false-alarm rate under H0 is
  close to (and not above) the nominal level, matching Breitung & Diegel's
  own claimed asymptotic size", {
  skip_on_cran()
  set.seed(2)
  nrep <- 300
  n <- 200
  T_star <- 100
  fires <- function(c_bar) {
    mean(vapply(seq_len(nrep), function(i) {
      set.seed(1000 + i)
      y <- cumsum(rnorm(n))
      !is.na(monitor_lbi(y, r_star = T_star, c_bar = c_bar, level = 0.95)$alarm[["series1"]])
    }, logical(1)))
  }
  expect_lt(fires(0), 0.10)
  expect_lt(fires(2), 0.10)
})

test_that("radf_lbi_monitor detects a genuine post-training bubble with
  power exceeding monitor_cusum(type = 'standard') on the same DGP, and
  wCUSUM (c_bar = 2) is at least as powerful as mCUSUM (c_bar = 0)", {
  skip_on_cran()
  make_bubble_series <- function(n, T_star, bstart, rho = 1.03) {
    y <- numeric(n)
    y[seq_len(T_star)] <- cumsum(rnorm(T_star))
    for (t in (T_star + 1):n) {
      y[t] <- if (t < bstart) y[t - 1] + rnorm(1) else rho * y[t - 1] + rnorm(1)
    }
    y
  }
  nrep <- 60
  n <- 200
  T_star <- 100
  run <- function(seed) {
    set.seed(seed)
    y <- make_bubble_series(n, T_star, bstart = 165)
    c(
      mcusum = !is.na(monitor_lbi(y, r_star = T_star, c_bar = 0)$alarm[["series1"]]),
      wcusum = !is.na(monitor_lbi(y, r_star = T_star, c_bar = 2)$alarm[["series1"]]),
      cusum_std = !is.na(monitor_cusum(y, r_star = T_star / n, b_alpha = 4.6)$alarm[["series1"]])
    )
  }
  res <- rowMeans(sapply(1:nrep, run))
  expect_gt(res[["mcusum"]], res[["cusum_std"]])
  expect_gte(res[["wcusum"]], res[["mcusum"]] - 0.05)
})
