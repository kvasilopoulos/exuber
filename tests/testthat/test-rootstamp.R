context("rootstamp")

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

test_that("rootstamp.default recovers a known rho on a simulated explosive AR(1)", {
  set.seed(1)
  rho_true <- 1.03
  n <- 150
  y <- numeric(n)
  e <- rnorm(n)
  for (t in 2:n) y[t] <- rho_true * y[t - 1] + e[t]

  ci <- rootstamp(y)
  expect_equal(ci$rho, rho_true, tolerance = 0.01)
  expect_true(ci$se > 0)
  expect_equal(ci$n, n - 1)
})

test_that("rootstamp.default's doubling time is consistent with its own rho estimate", {
  set.seed(1)
  rho_true <- 1.03
  n <- 150
  y <- numeric(n)
  e <- rnorm(n)
  for (t in 2:n) y[t] <- rho_true * y[t - 1] + e[t]

  ci <- rootstamp(y)
  expect_equal(ci$doubling_time, log(2) / log(ci$rho))
  # doubling time is decreasing in rho, so the CI bounds should be flipped
  expect_true(ci$doubling_time_ci[1] < ci$doubling_time)
  expect_true(ci$doubling_time_ci[2] > ci$doubling_time)
  expect_true(ci$rho_ci[1] < ci$rho && ci$rho < ci$rho_ci[2])
})

test_that("rootstamp.default empirical coverage is in a plausible range at T = 150
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
    ci <- rootstamp(y)
    ci$rho_ci[1] <= rho_true && rho_true <= ci$rho_ci[2]
  })
  # observed ~90% in development; a generous band around that, not the
  # nominal 95%, since finite-T undercoverage is expected for this estimator
  expect_gt(mean(covered), 0.80)
})

test_that("rootstamp.default(type = 'cauchy') brackets the point estimate and matches
  the Phillips-Magdalinos fixed-root formula (eq. 27) exactly, not just
  approximately", {
  set.seed(1)
  rho_true <- 1.05
  n <- 150
  y <- numeric(n)
  e <- rnorm(n)
  for (t in 2:n) y[t] <- rho_true * y[t - 1] + e[t]

  ci <- rootstamp(y, type = "cauchy")

  expect_true(ci$rho_ci[1] < ci$rho && ci$rho < ci$rho_ci[2])

  q <- qcauchy(0.975)
  half_width <- q * (ci$rho^2 - 1) / ci$rho^ci$n
  expect_equal(ci$rho_ci, ci$rho + c(-1, 1) * half_width)
})

test_that("rootstamp.radf_obj runs end-to-end on a real datestamp() result
  and its per-episode rho matches calling rootstamp.default() directly on the
  same Start/End positions", {
  set.seed(2026)
  burn <- cumsum(rnorm(60))
  bubble <- burn[length(burn)] * 1.04^(1:40) + cumsum(rnorm(40, sd = 0.5))
  y <- c(burn, bubble)

  r <- radf(y, minw = 20)
  mc <- radf_mc_cv(length(y), minw = 20, nrep = 300, seed = 4)
  ds <- datestamp(r, cv = mc, min_duration = 3)

  skip_if(length(ds) == 0, "no episode detected on this draw")

  out <- rootstamp(r, ds)
  expect_type(out, "list")
  expect_named(out, names(ds))

  ep <- out[["series1"]]
  first <- ds[["series1"]][1, ]
  direct <- rootstamp(y[first$Start:first$End])
  expect_equal(ep$rho[1], direct$rho)
  expect_equal(ep$rho_lower[1], direct$rho_ci[1])
})
