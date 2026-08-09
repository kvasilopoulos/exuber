context("radf_cobubble")

test_that("coexplosive_stat() matches an independent brute-force computation
  (separate lm() call, manual cumulative-sum loop instead of vectorized
  cumsum())", {
  set.seed(1)
  Tn <- 100
  x <- rnorm(Tn)
  y <- 2 + 0.5 * x + rnorm(Tn)
  lag <- 2L
  res <- exuber:::coexplosive_stat(y, x, lag)

  lo <- max(lag, 0L) + 1L
  hi <- Tn + min(lag, 0L)
  yy <- y[lo:hi]
  xx <- x[(lo:hi) - lag]
  e_brute <- residuals(lm(yy ~ xx))
  n_brute <- length(e_brute)
  sigma2_brute <- sum(e_brute^2) / n_brute
  running <- 0
  ss <- 0
  for (t in seq_along(e_brute)) {
    running <- running + e_brute[t]
    ss <- ss + running^2
  }
  S_brute <- ss / (sigma2_brute * n_brute^2)

  expect_equal(unname(res$S), unname(S_brute), tolerance = 1e-8)
})

test_that("'y' and 'x' must be the same length", {
  expect_error(radf_cobubble(rnorm(50), rnorm(51)))
})

test_that("wild bootstrap controls empirical size near the nominal level
  under H0 (co-explosive) with HOMOSKEDASTIC errors", {
  skip_on_cran()
  run_once <- function(seed) {
    set.seed(seed)
    Tn <- 150
    Te <- 90
    ex <- cumsum(rnorm(Te))
    expl <- ex[Te] * 1.05^(1:(Tn - Te)) + cumsum(rnorm(Tn - Te, sd = 0.3))
    x <- c(ex, expl)
    y <- 1 + 0.8 * x + rnorm(Tn, sd = 1)
    radf_cobubble(y, x, lag = 0L, nboot = 199L, seed = 1)$reject
  }
  size <- mean(sapply(1:100, run_once))
  # loose band around the nominal 5% -- 100 MC reps has real sampling noise
  expect_lt(size, 0.15)
})

test_that("wild bootstrap controls empirical size near the nominal level
  under H0 (co-explosive) with HETEROSKEDASTIC errors -- this is the
  paper's own central claim (Theorem 2): a fixed/homoskedastic critical
  value would NOT be valid here, only the wild bootstrap is", {
  skip_on_cran()
  run_once <- function(seed) {
    set.seed(seed)
    Tn <- 150
    Te <- 90
    ex <- cumsum(rnorm(Te))
    expl <- ex[Te] * 1.05^(1:(Tn - Te)) + cumsum(rnorm(Tn - Te, sd = 0.3))
    x <- c(ex, expl)
    sd_pattern <- c(rep(1, Tn %/% 2), rep(4, Tn - Tn %/% 2))
    y <- 1 + 0.8 * x + rnorm(Tn, sd = sd_pattern)
    radf_cobubble(y, x, lag = 0L, nboot = 199L, seed = 1)$reject
  }
  size <- mean(sapply(1:100, run_once))
  expect_lt(size, 0.15)
})

test_that("test has high power under H1: y and x contain independent
  (unrelated) explosive episodes, so co-explosivity should be rejected", {
  skip_on_cran()
  run_once <- function(seed) {
    set.seed(seed)
    Tn <- 150
    Te <- 90
    ex <- cumsum(rnorm(Te))
    expl_x <- ex[Te] * 1.05^(1:(Tn - Te)) + cumsum(rnorm(Tn - Te, sd = 0.3))
    x <- c(ex, expl_x)
    ey <- cumsum(rnorm(Te))
    expl_y <- ey[Te] * 1.05^(1:(Tn - Te)) + cumsum(rnorm(Tn - Te, sd = 0.3))
    y <- c(ey, expl_y)
    radf_cobubble(y, x, lag = 0L, nboot = 199L, seed = 1)$reject
  }
  power <- mean(sapply(1:40, run_once))
  expect_gt(power, 0.8)
})

test_that("coexplosive_select_lag() recovers a known true lag", {
  set.seed(1)
  Tn <- 200
  Te <- 120
  true_lag <- 3L
  ex <- cumsum(rnorm(Te))
  expl <- ex[Te] * 1.06^(1:(Tn - Te)) + cumsum(rnorm(Tn - Te, sd = 0.3))
  x <- c(ex, expl)
  y <- rep(NA_real_, Tn)
  for (t in (true_lag + 1):Tn) y[t] <- 1 + 0.8 * x[t - true_lag] + rnorm(1, sd = 0.5)
  y[1:true_lag] <- x[1:true_lag] + rnorm(true_lag, sd = 0.5)

  est_lag <- exuber:::coexplosive_select_lag(y, x, lags = -6:6)
  expect_equal(est_lag, true_lag)
})

test_that("radf_cobubble() runs end to end and returns a well-formed object", {
  set.seed(42)
  Tn <- 120
  Te <- 70
  ex <- cumsum(rnorm(Te))
  expl <- ex[Te] * 1.05^(1:(Tn - Te)) + cumsum(rnorm(Tn - Te, sd = 0.3))
  x <- c(ex, expl)
  y <- 1 + 0.8 * x + rnorm(Tn)

  out <- radf_cobubble(y, x, nboot = 99L, seed = 1)
  expect_s3_class(out, "radf_cobubble")
  expect_true(is.numeric(out$S))
  expect_true(is.numeric(out$cv))
  expect_true(out$lag %in% (-6:6))
  expect_true(is.logical(out$reject))
  expect_output(print(out), "radf_cobubble")
})
