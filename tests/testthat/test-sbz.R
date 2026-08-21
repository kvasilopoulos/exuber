context("radf_sbz")

test_that("wls_dfstat_grid matches a brute-force weighted no-intercept fit", {
  set.seed(3)
  y <- cumsum(rnorm(30))
  sigma2 <- runif(length(y) - 1, 0.5, 2)
  minw <- 8
  res <- exuber:::wls_dfstat_grid(y, sigma2, minw)

  yc <- y - y[1]
  n1 <- length(yc) - 1L
  dy <- diff(yc)
  ylag <- yc[1:n1]
  w <- 1 / sigma2

  b_idx <- minw:n1
  badf_brute <- vapply(b_idx, function(b) {
    idx <- 1:b
    sum(w[idx] * ylag[idx] * dy[idx]) / sqrt(sum(w[idx] * ylag[idx]^2))
  }, numeric(1))

  expect_equal(res$badf, badf_brute, tolerance = 1e-10)
  expect_equal(res$sadf, max(badf_brute), tolerance = 1e-10)
})

test_that("kernel_spot_vol recovers roughly the right scale under a volatility shift", {
  set.seed(5)
  n <- 300
  sigma_true <- c(rep(1, n * 0.3), rep(4, n * 0.7))
  y <- cumsum(rnorm(n, sd = sigma_true))
  vol <- exuber:::kernel_spot_vol(y)
  early <- mean(vol$sigma2[1:(n * 0.2)])
  late <- mean(vol$sigma2[(n * 0.8):(n - 1)])
  # true variance ratio is 16 (4^2); just check the estimator picks up the
  # right *direction* and order of magnitude, not an exact match
  expect_true(late > 4 * early)
})

test_that("radf_sbz_union runs on sim_data and gives internally consistent output", {
  skip_on_cran()
  options(exuber.parallel = FALSE, exuber.show_progress = FALSE)
  set.seed(1)
  res <- radf_sbz_union(dta[, 1], minw = 20, nboot = 100, seed = 1)
  expect_s3_class(res, "radf_sbz_union")
  expect_true(is.finite(res$supDF))
  expect_true(is.finite(res$supBZ))
  expect_true(is.finite(res$U))
  expect_true(res$p_supDF >= 0 && res$p_supDF <= 1)
  expect_true(res$p_supBZ >= 0 && res$p_supBZ <= 1)
  expect_true(res$p_U >= 0 && res$p_U <= 1)
  # U's critical values should be at least as large as supDF's alone
  # (union procedure trades a higher hurdle for combined coverage)
  expect_true(all(res$U_cv >= res$supDF_cv - 1e-8))
})

test_that("autoplot.radf_sbz_union runs without error", {
  skip_on_cran()
  options(exuber.parallel = FALSE, exuber.show_progress = FALSE)
  res <- radf_sbz_union(dta[, 1], minw = 20, nboot = 50, seed = 1)
  p <- autoplot(res)
  expect_s3_class(p, "ggplot")
})

test_that("radf_sbz/radf_sbz_cv match wls_dfstat_grid exactly (formula-exact, no re-derivation)", {
  set.seed(11)
  y <- cumsum(rnorm(60))
  minw <- 15
  res <- radf_sbz(y, minw = minw)

  vol <- exuber:::kernel_spot_vol(as.numeric(y))
  brute <- exuber:::wls_dfstat_grid(as.numeric(y), vol$sigma2, minw)

  expect_equal(unname(res$badf[, 1]), brute$badf, tolerance = 1e-10)
  expect_equal(unname(res$bsadf[, 1]), brute$bsadf, tolerance = 1e-10)
  expect_equal(unname(res$adf), brute$adf, tolerance = 1e-10)
  expect_equal(unname(res$sadf), brute$sadf, tolerance = 1e-10)
  expect_equal(unname(res$gsadf), brute$gsadf, tolerance = 1e-10)
})

test_that("radf_sbz_cv computes badf_cv/bsadf_cv with the right shape and a hard identity", {
  skip_on_cran()
  options(exuber.parallel = FALSE, exuber.show_progress = FALSE)
  n <- 80
  minw <- 20
  n_minw <- n - minw
  set.seed(1)
  y <- cumsum(rnorm(n))
  cv <- radf_sbz_cv(y, minw = minw, nboot = 100, seed = 1)

  expect_equal(dim(cv$badf_cv), c(n_minw, 3L, 1L))
  expect_equal(dim(cv$bsadf_cv), c(n_minw, 3L, 1L))
  expect_equal(dimnames(cv$badf_cv)[[2]], c("90%", "95%", "99%"))
  # badf's last point IS adf by construction (wls_dfstat_grid(): adf <-
  # badf[length(badf)]), per replicate -- so their quantiles must match
  # exactly, not just approximately.
  expect_equal(unname(cv$badf_cv[n_minw, , 1]), unname(cv$adf_cv[1, ]))
})

test_that("radf_sbz's full Analysis/Tidying/Plotting pipeline works, not just print()", {
  skip_on_cran()
  options(exuber.parallel = FALSE, exuber.show_progress = FALSE)
  # supBZ's kernel-volatility weighting trades power for heteroskedasticity
  # robustness (same trade-off already accepted for radf_sbz_union's supBZ
  # leg) -- sim_data's bubbles are too mild for supBZ to reject at nboot=100,
  # even though they reject easily for the classic supDF/wb_cv. Use a blatant
  # deterministic explosive path instead so this test only exercises the
  # rejection code path in summary/tidy/datestamp/autoplot, not statistical
  # power (power is out of scope here; see the formula-exact test above).
  set.seed(7)
  n <- 120
  te <- 70
  y <- cumsum(rnorm(n))
  y[(te + 1):n] <- y[te] * 1.15^(seq_len(n - te))

  res <- radf_sbz(y, minw = 20)
  cv <- radf_sbz_cv(y, minw = 20, nboot = 100, seed = 1)

  expect_no_error(summary(res, cv = cv))
  expect_no_error(tidy(res, cv = cv))
  expect_no_error(datestamp(res, cv = cv))
  expect_no_error(datestamp(res, cv = cv, option = "sadf"))
  expect_no_error(autoplot(res, cv = cv))
})
