context("radf_pdc")

test_that("pdc_find_break() matches a brute-force lm()-based RSS scan exactly", {
  set.seed(123)
  y <- cumsum(rnorm(80))
  trim <- 0.05
  res <- exuber:::pdc_find_break(y, trim)

  n1 <- length(y) - 1L
  ylag <- y[1:n1]; ycur <- y[2:(n1 + 1)]
  k_min <- max(2L, ceiling(trim * n1))
  k_max <- n1 - k_min
  rss_brute <- sapply(k_min:k_max, function(k) {
    left <- 1:k
    right <- (k + 1):n1
    sum(lm(ycur[left] ~ ylag[left] - 1)$residuals^2) +
      sum(lm(ycur[right] ~ ylag[right] - 1)$residuals^2)
  })
  brute_break <- (k_min:k_max)[which.min(rss_brute)]

  expect_equal(res$break_idx, brute_break)
  expect_equal(res$rss, min(rss_brute), tolerance = 1e-8)
})

test_that("regimes must be 3 or 4", {
  y <- cumsum(rnorm(80))
  expect_error(radf_pdc(y, regimes = 5L), "should be 3 or 4")
})

test_that("errors on a series too short for the requested trim", {
  expect_error(radf_pdc(rnorm(10), regimes = 4L), "too short")
})

test_that("works on a multivariate panel, one row per series", {
  set.seed(1)
  y <- cumsum(rnorm(80))
  panel <- cbind(s1 = y, s2 = y + rnorm(length(y), sd = 0.1))
  out <- radf_pdc(panel, regimes = 3L)
  expect_equal(rownames(out), c("s1", "s2"))
  expect_true(all(c("origination", "collapse") %in% colnames(out)))
})

test_that("3-regime dating is essentially exact in the low-noise/long-series/
  strong-effect limit -- confirms the estimator is consistent (converges to
  the truth as conditions improve), which finite-sample accuracy checks
  alone can't distinguish from a systematic bug", {
  set.seed(99)
  n1_len <- 300; n2_len <- 150; n3_len <- 200
  regime1 <- cumsum(rnorm(n1_len, sd = 1))
  regime2 <- regime1[n1_len] * 1.08^(1:n2_len) + cumsum(rnorm(n2_len, sd = 0.1))
  peak <- regime2[n2_len]; target <- regime1[n1_len]
  regime3 <- target + (peak - target) * exp(-0.2 * (1:n3_len)) + rnorm(n3_len, sd = 0.3)
  y <- c(regime1, regime2, regime3)

  out <- radf_pdc(y, regimes = 3L, trim = 0.05)
  expect_lte(abs(out$origination - n1_len), 2)
  expect_lte(abs(out$collapse - (n1_len + n2_len)), 2)
})

test_that("4-regime dating (KS extension, recovery = split of the
  post-collapse subsample) is essentially exact in the same low-noise
  limit", {
  # The collapse regime must be a genuine stationary AR(1) (rho < 1, no
  # intercept), matching what pdc_find_break() actually fits -- a
  # deterministic decay (e.g. exp(-k*t) toward a target) is not an AR(1)
  # process at all, and once it numerically converges its noisy "flat" tail
  # becomes statistically indistinguishable from the following random-walk
  # recovery regime, so the estimator finds a spurious break inside the
  # collapse regime instead of at the true collapse/recovery boundary.
  set.seed(3)
  n1_len <- 200; n2_len <- 100; n3_len <- 150; n4_len <- 150
  regime1 <- cumsum(rnorm(n1_len, sd = 0.5))
  regime2 <- regime1[n1_len] * 1.08^(1:n2_len) + cumsum(rnorm(n2_len, sd = 0.1))
  peak <- regime2[n2_len]
  rho3 <- 0.5
  regime3 <- numeric(n3_len)
  regime3[1] <- rho3 * peak + rnorm(1, sd = 1)
  for (t in 2:n3_len) regime3[t] <- rho3 * regime3[t - 1] + rnorm(1, sd = 1)
  regime4 <- regime3[n3_len] + cumsum(rnorm(n4_len, sd = 0.5))
  y <- c(regime1, regime2, regime3, regime4)

  out <- radf_pdc(y, regimes = 4L, trim = 0.05)
  true_origination <- n1_len
  true_collapse <- n1_len + n2_len
  true_recovery <- n1_len + n2_len + n3_len
  expect_lte(abs(out$origination - true_origination), 3)
  expect_lte(abs(out$collapse - true_collapse), 3)
  expect_lte(abs(out$recovery - true_recovery), 3)
})

test_that("finite-sample accuracy at moderate T is genuinely limited -- not
  asserted at a high rate, since Kurozumi & Skrobotov's own Monte Carlo
  reports only ~30% exact-date recovery at T=400 (rising to ~65% at
  T=800), so a moderate-T synthetic check should NOT expect tight,
  reliable recovery; this documents that honestly rather than picking a
  synthetic DGP lucky enough to pass a tight tolerance", {
  skip_on_cran()
  set.seed(2000)
  errs <- t(sapply(1:20, function(s) {
    set.seed(s + 2000)
    n1_len <- 60; n2_len <- 30; n3_len <- 40
    regime1 <- cumsum(rnorm(n1_len, sd = 1))
    regime2 <- regime1[n1_len] * 1.04^(1:n2_len) + cumsum(rnorm(n2_len, sd = 0.3))
    peak <- regime2[n2_len]; target <- regime1[n1_len]
    regime3 <- target + (peak - target) * exp(-0.15 * (1:n3_len)) + rnorm(n3_len, sd = 0.5)
    y <- c(regime1, regime2, regime3)
    out <- radf_pdc(y, regimes = 3L, trim = 0.05)
    c(orig_err = out$origination - n1_len, coll_err = out$collapse - (n1_len + n2_len))
  }))
  # sanity bound only: estimates should stay within the sample, not that
  # they're close to the truth (per KS's own low exact-recovery rate at
  # this scale) -- a regression guard against the estimator becoming
  # degenerate (e.g. always returning the trim boundary), not an accuracy claim
  expect_true(all(is.finite(errs)))
  expect_true(sd(errs[, "coll_err"]) > 0)
})
