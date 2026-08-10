context("radf_hls")

test_that("hls_segment_ssr matches a brute-force lm() SSR for fixed segments", {
  set.seed(1)
  y <- cumsum(rnorm(40))
  ps <- exuber:::hls_prefix_sums(y)
  n1 <- length(y) - 1L
  x_all <- y[1:n1]; z_all <- y[2:(n1 + 1)] - y[1:n1]

  for (seg in list(c(1, 10), c(11, 25), c(5, 39))) {
    lo <- seg[1] - 1; hi <- seg[2]
    manual <- exuber:::hls_segment_ssr(ps, lo, hi, TRUE)
    idx <- (lo + 1):hi
    brute <- sum(resid(lm(z_all[idx] ~ x_all[idx]))^2)
    expect_equal(manual, brute, tolerance = 1e-8)
  }
  expect_equal(exuber:::hls_segment_ssr(ps, 0, 10, FALSE), sum(z_all[1:10]^2), tolerance = 1e-8)
})

test_that("hls_model1's grid search matches a brute-force nested lm() search", {
  set.seed(2)
  n <- 30
  y <- cumsum(rnorm(n))
  ps <- exuber:::hls_prefix_sums(y)
  m1 <- exuber:::hls_model1(y, ps, trim = 0.1)

  n1 <- n - 1L
  x <- y[1:n1]; z <- y[2:(n1 + 1)] - y[1:n1]
  k_min <- max(2L, ceiling(0.1 * n1))
  best <- list(ssr = Inf)
  for (tau1 in k_min:(n1 - k_min)) {
    idx_right <- (tau1 + 1):n1
    ssr <- sum(z[1:tau1]^2) + sum(resid(lm(z[idx_right] ~ x[idx_right]))^2)
    if (ssr < best$ssr) best <- list(tau1 = tau1, ssr = ssr)
  }
  expect_equal(m1$tau1, best$tau1)
  expect_equal(m1$ssr, best$ssr, tolerance = 1e-6)
})

test_that("hls_model4's joint 3-breakpoint grid search matches a brute-force
  nested lm() search", {
  skip_on_cran()
  set.seed(5)
  n <- 24
  y <- cumsum(rnorm(n))
  ps <- exuber:::hls_prefix_sums(y)
  m4 <- exuber:::hls_model4(y, ps, trim = 0.1)

  n1 <- n - 1L
  x <- y[1:n1]; z <- y[2:(n1 + 1)] - y[1:n1]
  k_min <- max(2L, ceiling(0.1 * n1))
  best <- list(ssr = Inf)
  for (tau1 in k_min:(n1 - 3 * k_min)) {
    for (tau2 in (tau1 + k_min):(n1 - 2 * k_min)) {
      if (y[tau2 + 1] <= y[tau1 + 1]) next
      for (tau3 in (tau2 + k_min):(n1 - k_min)) {
        ssr <- sum(z[1:tau1]^2) +
          sum(resid(lm(z[(tau1 + 1):tau2] ~ x[(tau1 + 1):tau2]))^2) +
          sum(resid(lm(z[(tau2 + 1):tau3] ~ x[(tau2 + 1):tau3]))^2) +
          sum(z[(tau3 + 1):n1]^2)
        if (ssr < best$ssr) best <- list(tau1 = tau1, tau2 = tau2, tau3 = tau3, ssr = ssr)
      }
    }
  }
  expect_equal(m4$tau1, best$tau1)
  expect_equal(m4$tau2, best$tau2)
  expect_equal(m4$tau3, best$tau3)
  expect_equal(m4$ssr, best$ssr, tolerance = 1e-6)
})

test_that("radf_hls runs end to end and returns a well-formed object", {
  set.seed(1)
  y <- cumsum(rnorm(100))
  out <- radf_hls(y, trim = 0.05)

  expect_s3_class(out, "radf_hls_obj")
  expect_true(out$model[["series1"]] %in% 1:4)
  expect_true(is.matrix(out$bic))
  expect_equal(ncol(out$bic), 4)
  expect_output(print(out), "radf_hls")
})

test_that("radf_hls's selected model always has NA for the breakpoints it
  doesn't have (model 1 has no collapse/recovery, model 2/3 have no
  recovery)", {
  set.seed(1)
  y <- cumsum(rnorm(100))
  out <- radf_hls(y, trim = 0.05)
  m <- out$model[["series1"]]
  if (m == 1) {
    expect_true(is.na(out$collapse[["series1"]]))
    expect_true(is.na(out$recovery[["series1"]]))
  } else if (m %in% c(2, 3)) {
    expect_true(is.na(out$recovery[["series1"]]))
  }
  expect_false(is.na(out$origination[["series1"]]))
})

test_that("radf_hls recovers a genuine 4-regime bubble episode with
  plausible (not wildly biased) breakpoint dates, and correctly favors
  Model 3/4 (a distinct collapse regime) over the more parsimonious
  Model 1/2 when the collapse is genuinely a different regime", {
  skip_on_cran()
  sim_model4 <- function(seed, n1 = 60, n2 = 25, n3 = 25, n4 = 40,
                          base = 100, c_bubble = 1.05) {
    set.seed(seed)
    unit1 <- base + cumsum(rnorm(n1))
    bubble <- unit1[n1] * c_bubble^(1:n2) + cumsum(rnorm(n2))
    target <- bubble[n2] * 0.5
    collapse <- numeric(n3)
    collapse[1] <- bubble[n2] + rnorm(1)
    for (k in 2:n3) collapse[k] <- target + 0.85 * (collapse[k - 1] - target) + rnorm(1)
    recovery <- collapse[n3] + cumsum(rnorm(n4))
    list(y = c(unit1, bubble, collapse, recovery), true_tau1 = n1, true_tau2 = n1 + n2)
  }
  run_once <- function(seed) {
    sim <- sim_model4(seed)
    out <- radf_hls(sim$y, trim = 0.05)
    list(
      model = out$model[["series1"]],
      orig_bias = as.numeric(out$origination[["series1"]]) - sim$true_tau1
    )
  }
  res <- lapply(1:15, run_once)
  models <- sapply(res, `[[`, "model")
  bias <- sapply(res, `[[`, "orig_bias")
  expect_true(mean(models %in% c(3, 4)) > 0.5)
  expect_true(mean(abs(bias)) < 15)
})

test_that("radf_hls does not spuriously prefer complex models on a
  pure random-walk null with no bubble at all", {
  skip_on_cran()
  run_h0 <- function(seed) {
    set.seed(seed)
    y <- 100 + cumsum(rnorm(150))
    radf_hls(y, trim = 0.05)$model[["series1"]]
  }
  models <- sapply(1:20, run_h0)
  expect_true(mean(models == 4) < 0.3)
})
