context("radf_kp")

test_that("radf_kp runs on sim_data and returns a standard radf_obj", {
  res <- radf_kp(dta)
  expect_s3_class(res, "radf_obj")
  expect_true(all(is.finite(res$adf)))
  expect_true(all(is.finite(res$gsadf)))
})

test_that("kernel_purge reduces to (approximately) the original series under
  constant volatility, since dividing by a near-constant sigma_hat and
  cumulating first differences approximately recovers the series' own path", {
  set.seed(11)
  y <- cumsum(rnorm(200))
  x <- exuber:::kernel_purge(y)
  # not identical (sigma_hat is estimated, not exactly 1), but should be
  # highly correlated with the (demeaned) original series' increments
  expect_gt(cor(diff(x), diff(y)[-1]), 0.9)
})

test_that("radf_kp's null GSADF distribution is in the published ballpark
  (Harvey, Leybourne, Taylor & Zu 2024, Table I, T = 400, with-intercept
  PSY_sigma: 90/95/99% = 1.712/1.935/2.296) and close to exuber's own
  radf_mc_cv() at a comparable n, per Remark 3.2's claim that the two
  distributions coincide asymptotically", {
  skip_on_cran()
  options(exuber.parallel = FALSE, exuber.show_progress = FALSE)

  set.seed(2)
  n <- 300
  gsadf_kp <- replicate(500, max(radf_kp(cumsum(rnorm(n)))$gsadf))
  kp_cv <- quantile(gsadf_kp, c(0.9, 0.95, 0.99))

  mc <- radf_mc_cv(n, seed = 2, nrep = 500)

  # Generous tolerance: radf_kp adds kernel-estimation noise on top of the
  # same finite-T Monte Carlo error that radf_mc_cv already has, so this is
  # necessarily a looser check than the closed-form STADF/SBZ ones.
  expect_equal(unname(kp_cv), unname(mc$gsadf_cv), tolerance = 0.3)

  published_T400 <- c(1.712, 1.935, 2.296)
  expect_equal(unname(kp_cv), published_T400, tolerance = 0.3)
})
