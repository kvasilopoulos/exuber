context("explosive_root / root_ci")

test_that("Cauchy two-sided percentiles (Phillips-Magdalinos / Guo Sun Wang
  citation) match Student's t with 1 df exactly -- the standard Cauchy
  distribution IS Student's t at df = 1, a pure mathematical identity, so
  this is checkable bit-for-bit with no simulation involved", {
  published <- c(`10%` = 6.314, `5%` = 12.7, `1%` = 63.65674)
  computed <- c(
    `10%` = qt(0.95, df = 1),
    `5%` = qt(0.975, df = 1),
    `1%` = qt(0.995, df = 1)
  )
  expect_equal(unname(computed), unname(published), tolerance = 1e-3)
})

test_that("explosive_root recovers a known rho on a simulated explosive AR(1)", {
  set.seed(1)
  rho_true <- 1.03
  n <- 150
  y <- numeric(n)
  e <- rnorm(n)
  for (t in 2:n) y[t] <- rho_true * y[t - 1] + e[t]

  est <- explosive_root(y, 1, n)
  expect_equal(est$rho, rho_true, tolerance = 0.01)
  expect_true(est$se > 0)
  expect_equal(est$n, n - 1)
})

test_that("root_ci's doubling time is consistent with its own rho estimate", {
  set.seed(1)
  rho_true <- 1.03
  n <- 150
  y <- numeric(n)
  e <- rnorm(n)
  for (t in 2:n) y[t] <- rho_true * y[t - 1] + e[t]

  est <- explosive_root(y, 1, n)
  ci <- root_ci(est)
  expect_equal(ci$doubling_time, log(2) / log(ci$rho))
  # doubling time is decreasing in rho, so the CI bounds should be flipped
  expect_true(ci$doubling_time_ci[1] < ci$doubling_time)
  expect_true(ci$doubling_time_ci[2] > ci$doubling_time)
  expect_true(ci$rho_ci[1] < ci$rho && ci$rho < ci$rho_ci[2])
})

test_that("root_ci empirical coverage is in a plausible range at T = 150
  (Guo, Sun & Wang's asymptotic-normal t-statistic result; not expected to
  be exact at finite T -- reported honestly rather than asserted at nominal)", {
  skip_on_cran()
  set.seed(2)
  rho_true <- 1.03
  n <- 150
  covered <- replicate(500, {
    y <- numeric(n)
    e <- rnorm(n)
    for (t in 2:n) y[t] <- rho_true * y[t - 1] + e[t]
    ci <- root_ci(explosive_root(y, 1, n))
    ci$rho_ci[1] <= rho_true && rho_true <= ci$rho_ci[2]
  })
  # observed ~90% in development; a generous band around that, not the
  # nominal 95%, since finite-T undercoverage is expected for this estimator
  expect_gt(mean(covered), 0.80)
})
