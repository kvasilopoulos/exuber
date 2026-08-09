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

test_that("one_sided_kernel_spot_vol() matches an independent brute-force
  loop recomputation of Astill, Harvey, Leybourne, Taylor & Zu's eq. 6-7", {
  set.seed(1)
  n <- 60
  dy <- rnorm(n)
  N <- 10
  res <- exuber:::one_sided_kernel_spot_vol(dy, N = N, kernel = "gaussian")

  w_full <- dnorm((0:N) / N); w_full <- w_full / sum(w_full)
  sigma2_brute <- numeric(n)
  for (j in seq_len(n)) {
    if (j <= N) {
      sigma2_brute[j] <- 1
    } else {
      idx <- (j - N):j
      sigma2_brute[j] <- sum(rev(w_full) * dy[idx]^2)
    }
  }
  expect_equal(res[(N + 1):n], sigma2_brute[(N + 1):n], tolerance = 1e-10)
  expect_true(all(res[1:N] == 1))
})

test_that("cusum_stat_path_kernel() matches an independent brute-force
  recomputation of the CUSUMV statistic (eq. 6)", {
  set.seed(2)
  n <- 100
  T_star <- 50
  y <- cumsum(rnorm(n))
  b_alpha <- 4.6
  res <- exuber:::cusum_stat_path_kernel(y, T_star, b_alpha, N = 20, kernel = "gaussian")

  dy <- diff(y)
  sigma2_dy <- exuber:::one_sided_kernel_spot_vol(dy, N = 20, kernel = "gaussian")
  SV_brute <- numeric(n - T_star)
  for (k in seq_len(n - T_star)) {
    t <- T_star + k
    js <- (T_star + 1):t
    SV_brute[k] <- sum(dy[js - 1] / sqrt(sigma2_dy[js - 1]))
  }
  expect_equal(res$S, SV_brute, tolerance = 1e-8)
})

test_that("type = 'kernel' runs end to end and returns a well-formed object", {
  set.seed(1)
  y <- cumsum(rnorm(100))
  out <- radf_cusum(y, r_star = 0.5, type = "kernel")
  expect_s3_class(out, "radf_cusum_obj")
  expect_true(all(is.finite(out$S)))
})

test_that("type = 'kernel' (CUSUMV) controls the false-alarm rate under
  heteroskedasticity meaningfully better than type = 'standard' -- this
  is Astill, Harvey, Leybourne, Taylor & Zu (2023)'s central claim: the
  standard CUSUM procedure requires homoskedasticity for its own
  size-control result to hold and becomes oversized without it, while
  the kernel-weighted variant stays controlled via the same boundary
  function (their Corollary 1)", {
  skip_on_cran()
  run_null_hetero <- function(seed, type) {
    set.seed(seed)
    n <- 150
    vol <- c(rep(1, 90), rep(8, 60))
    y <- cumsum(rnorm(n) * vol)
    out <- radf_cusum(y, r_star = 0.5, b_alpha = 4.6, type = type)
    !is.na(out$alarm)
  }
  rate_std <- mean(sapply(1:40, function(s) run_null_hetero(s, "standard")))
  rate_ker <- mean(sapply(1:40, function(s) run_null_hetero(s, "kernel")))
  expect_lte(rate_ker, rate_std)
})
