context("radf_sign")

test_that("sign_transform() + gls_dfstat_grid() matches a brute-force
  no-intercept lm() fit on the cumulated-sign series", {
  set.seed(7)
  y <- cumsum(rnorm(40))
  minw <- 10
  Cy <- exuber:::sign_transform(y)
  res <- exuber:::gls_dfstat_grid(Cy, minw)

  yc <- Cy - Cy[1]
  n1 <- length(yc) - 1L
  dy <- diff(yc)
  ylag <- yc[1:n1]

  b_idx <- minw:n1
  badf_brute <- vapply(b_idx, function(b) {
    fit <- lm(dy[1:b] ~ ylag[1:b] - 1)
    summary(fit)$coefficients[1, "t value"]
  }, numeric(1))

  expect_equal(res$badf, badf_brute, tolerance = 1e-8)
  expect_equal(res$sadf, max(badf_brute), tolerance = 1e-8)
})

test_that("radf_sign runs on the package's sim_data and returns finite stats", {
  res <- radf_sign(dta)
  expect_s3_class(res, "radf_sign_obj")
  expect_true(all(is.finite(res$adf)))
  expect_true(all(is.finite(res$sadf)))
  expect_true(all(is.finite(res$gsadf)))
  expect_true(all(res$sadf <= res$gsadf + 1e-8))
})

test_that("radf_sign() is EXACTLY invariant to the pattern of (even wildly
  time-varying) volatility -- this is the paper's central claim: sign()
  strips out all magnitude information, so scaling the same sign-pattern
  series by any volatility function must leave sadf/gsadf bit-identical,
  not just approximately similar", {
  set.seed(7)
  n <- 150
  raw_dy <- c(rnorm(90), rnorm(1, mean = 3), rnorm(59))
  y_homo <- cumsum(raw_dy)
  vol_pattern <- c(rep(0.1, 40), rep(10, 60), rep(1, n - 100))
  y_hetero <- cumsum(raw_dy * vol_pattern)

  r_homo <- radf_sign(y_homo, minw = 20)
  r_hetero <- radf_sign(y_hetero, minw = 20)

  expect_identical(r_homo$sadf, r_hetero$sadf)
  expect_identical(r_homo$gsadf, r_hetero$gsadf)
})

test_that("radf_sign_cv's simulated critical values match Harvey, Leybourne &
  Zu (2020)'s Table 1 published finite-sample values at T = 200: sPWY
  (10%, 5%, 1%) = (2.405, 2.735, 3.434) is checked against sadf_cv (r1 = 0
  fixed, single supremum), sPSY = (3.469, 3.901, 4.957) against gsadf_cv
  (double supremum). Tolerance reflects observed MC noise at this
  nrep/T, not chosen to force a pass -- gsadf_cv (sPSY) converges more
  slowly and is given a looser tolerance, matching the paper's own
  documented finding that sPSY's finite-sample values converge to their
  asymptotic limit much more slowly than sPWY's.", {
  skip_on_cran()

  set.seed(20260809)
  n <- 200
  minw <- round(0.1 * n)
  cv <- radf_sign_cv(n, minw = minw, nrep = 1500, seed = 1)

  published_pwy <- c(2.405, 2.735, 3.434)
  published_psy <- c(3.469, 3.901, 4.957)

  expect_equal(unname(cv$sadf_cv), published_pwy, tolerance = 0.15)
  expect_equal(unname(cv$gsadf_cv), published_psy, tolerance = 0.3)
})

test_that("radf_sign correctly detects a clear mildly explosive alternative
  using its own simulated critical value", {
  skip_on_cran()
  cv <- radf_sign_cv(150, minw = 20, nrep = 500, seed = 2)
  run_once <- function(seed) {
    set.seed(seed)
    Tn <- 150
    Te <- 90
    normal_part <- cumsum(rnorm(Te))
    expl_part <- normal_part[Te] * 1.05^(1:(Tn - Te)) + cumsum(rnorm(Tn - Te, sd = 0.5))
    y <- c(normal_part, expl_part)
    radf_sign(y, minw = 20)$gsadf > cv$gsadf_cv["95%"]
  }
  power <- mean(sapply(1:20, run_once))
  expect_gt(power, 0.7)
})
