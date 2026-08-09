context("radf_cusum")

test_that("cusum_stat_path() matches an independent brute-force loop
  recomputation of Homm & Breitung's eq. 26-29", {
  set.seed(1)
  n <- 80
  T_star <- 40
  y <- cumsum(rnorm(n))
  b_alpha <- 4.6
  res <- exuber:::cusum_stat_path(y, T_star, b_alpha)

  S_brute <- numeric(n - T_star)
  bnd_brute <- numeric(n - T_star)
  for (k in seq_len(n - T_star)) {
    t <- T_star + k
    dy <- diff(y[1:t])
    sigma2_t <- sum(dy^2) / (t - 1)
    S_brute[k] <- (y[t] - y[T_star]) / sqrt(sigma2_t)
    c_t <- sqrt(b_alpha + log(t / T_star))
    bnd_brute[k] <- c_t * sqrt(t)
  }
  expect_equal(res$S, S_brute, tolerance = 1e-10)
  expect_equal(res$boundary, bnd_brute, tolerance = 1e-10)
})

test_that("radf_cusum runs end to end and returns a well-formed object", {
  set.seed(1)
  y <- cumsum(rnorm(100))
  out <- radf_cusum(y, r_star = 0.5)
  expect_s3_class(out, "radf_cusum_obj")
  expect_equal(out$T_star, 50)
  expect_output(print(out), "radf_cusum")
})

test_that("an alarm, when raised, always falls strictly after T_star", {
  skip_on_cran()
  run_once <- function(seed) {
    set.seed(seed)
    n1 <- 75; n2 <- 40
    normal_part <- cumsum(rnorm(n1))
    expl_part <- normal_part[n1] * 1.05^(1:n2) + cumsum(rnorm(n2, sd = 0.3))
    y <- c(normal_part, expl_part)
    out <- radf_cusum(y, r_star = n1 / length(y))
    alarm <- unname(out$alarm)
    if (is.na(alarm)) NA else alarm > out$T_star
  }
  results <- sapply(1:10, run_once)
  expect_true(all(na.omit(results)))
})

test_that("empirical cumulative false-alarm rate under H0 stays well within
  Homm & Breitung's own conservative asymptotic bound -- their eq. 28 is
  an upper bound (via Chu, Stinchcombe & White 1996), not an exact size,
  so a rate comfortably below the nominal level is expected, not just
  tolerated", {
  skip_on_cran()
  run_null <- function(seed) {
    set.seed(seed)
    y <- cumsum(rnorm(150))
    out <- radf_cusum(y, r_star = 0.5, b_alpha = 4.6)
    !is.na(out$alarm)
  }
  rate <- mean(sapply(1:60, run_null))
  expect_lt(rate, 0.15)
})

test_that("radf_cusum detects at least some genuine post-training bubbles,
  with a positive alarm delay -- power is expected to be genuinely lower
  than the ADF-family radf_monitor() for this kind of (mid-sample)
  bubble, consistent with the literature's own finding (Kurozumi 2020/
  2021, cited in monitoring.md) that CUSUM-type detectors underperform
  ADF-type ones for middle-to-late bubbles; this is not asserted as high
  power, only as non-degenerate", {
  skip_on_cran()
  run_once <- function(seed) {
    set.seed(seed)
    n1 <- 75; n2 <- 40
    normal_part <- cumsum(rnorm(n1))
    expl_part <- normal_part[n1] * 1.05^(1:n2) + cumsum(rnorm(n2, sd = 0.3))
    y <- c(normal_part, expl_part)
    out <- radf_cusum(y, r_star = n1 / length(y))
    c(alarm = unname(out$alarm), true_origination = n1)
  }
  res <- t(sapply(1:30, run_once))
  detected <- !is.na(res[, "alarm"])
  expect_gt(mean(detected), 0.1)
  delay <- res[detected, "alarm"] - res[detected, "true_origination"]
  expect_true(all(delay > 0))
})
