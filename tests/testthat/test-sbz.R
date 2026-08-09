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

test_that("radf_sbz_cv runs on sim_data and gives internally consistent output", {
  skip_on_cran()
  options(exuber.parallel = FALSE, exuber.show_progress = FALSE)
  set.seed(1)
  res <- radf_sbz_cv(dta[, 1], minw = 20, nboot = 100, seed = 1)
  expect_s3_class(res, "radf_sbz")
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
