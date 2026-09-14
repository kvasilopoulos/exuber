context("radf_sb_cv")

test_that("radf_sb_cv(type = 'fixed') (the pre-existing default) still runs
  and matches passing `lag` directly, unaffected by the new type/max_lag
  arguments", {
  set.seed(5)
  y <- cumsum(rnorm(80))
  a <- radf_sb_cv(y, lag = 1, nboot = 50, seed = 9)
  b <- radf_sb_cv(y, lag = 1, type = "fixed", nboot = 50, seed = 9)
  expect_equal(a$gsadf_panel_cv, b$gsadf_panel_cv)
})

test_that("radf_sb_cv(type = 'aic'/'bic') selects a lag via lag_select()
  (Pedersen & Schütte 2020's fix for size distortion under a fixed lag)
  instead of using the `lag` argument, and returns a usable radf_cv object", {
  set.seed(6)
  # AR(2)-autocorrelated innovations, so a lag > 0 should actually get picked
  n <- 150
  e <- arima.sim(list(ar = c(0.4, 0.2)), n = n)
  y <- cumsum(as.numeric(e))

  sb_bic <- radf_sb_cv(y, type = "bic", max_lag = 4, nboot = 50, seed = 9)
  expect_s3_class(sb_bic, "radf_cv")
  expect_true(attr(sb_bic, "lag") >= 0)

  sb_aic <- radf_sb_cv(y, type = "aic", max_lag = 4, nboot = 50, seed = 9)
  expect_s3_class(sb_aic, "radf_cv")
})

test_that("radf_sb_cv(type = 'bic') mostly selects a small lag on
  white-noise-differenced data (a random walk in levels), since there's no
  real autocorrelation to pick up -- checked via the modal choice across
  several independent draws rather than asserting exactly 0 on one draw,
  since BIC can pick a nonzero lag by chance in any single finite sample", {
  skip_on_cran()
  lags <- vapply(1:8, function(s) {
    set.seed(s)
    y <- cumsum(rnorm(100))
    attr(radf_sb_cv(y, type = "bic", max_lag = 6, nboot = 20, seed = 9), "lag")
  }, integer(1))
  expect_equal(as.integer(names(sort(table(lags), decreasing = TRUE))[1]), 0L)
})

test_that("the bootstrap panel BSADF is the cross-sectional mean of the series' BSADF paths", {
  # Regression test: the bootstrap loop used to overwrite the per-series
  # BSADF instead of summing it, returning (last series' BSADF) / nc. A
  # panel of identical copies shares one bootstrap path per replication
  # (same resampling index for every series), so its panel distribution
  # must equal the single-series one exactly; the bug divided it by nc.
  set.seed(5)
  y <- cumsum(rnorm(80))
  single <- radf_sb_distr(y, nboot = 30, seed = 9)
  triple <- radf_sb_distr(cbind(a = y, b = y, c = y), nboot = 30, seed = 9)
  expect_equal(as.numeric(triple), as.numeric(single))
  cv1 <- radf_sb_cv(y, nboot = 30, seed = 9)
  cv3 <- radf_sb_cv(cbind(a = y, b = y, c = y), nboot = 30, seed = 9)
  expect_equal(cv3$gsadf_panel_cv, cv1$gsadf_panel_cv)
  expect_equal(cv3$bsadf_panel_cv, cv1$bsadf_panel_cv)
})

test_that("the panel sieve-bootstrap test is correctly sized under H0", {
  skip_on_cran()
  set.seed(4); n <- 100; nc <- 5
  rej <- replicate(100, {
    p <- replicate(nc, cumsum(rnorm(n)))
    radf(p)$gsadf_panel > radf_sb_cv(p, nboot = 99)$gsadf_panel_cv["95%"]
  })
  expect_lt(mean(rej), 0.12) # nominal 5%; the buggy version gave 0% for nc = 10 and 8% for nc = 2
  expect_gt(mean(rej), 0.005)
})
