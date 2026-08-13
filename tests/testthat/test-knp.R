context("radf_knp")

test_that("knp_find_break(omit = FALSE) matches a brute-force nested lm() search", {
  set.seed(3)
  n <- 26
  y <- cumsum(rnorm(n))
  fit <- exuber:::knp_find_break(y, trim = 0.1, omit = FALSE)

  n1 <- n - 1L
  x <- y[1:n1]; z <- y[2:(n1 + 1)] - y[1:n1]
  k_min <- max(2L, ceiling(0.1 * n1))
  best <- list(ssr = Inf)
  for (tau1 in k_min:(n1 - 2 * k_min)) {
    for (tau2 in (tau1 + k_min):(n1 - k_min)) {
      idx_mid <- (tau1 + 1):tau2
      ssr <- sum(z[1:tau1]^2) +
        sum(resid(lm(z[idx_mid] ~ x[idx_mid]))^2) +
        sum(z[(tau2 + 1):n1]^2)
      if (ssr < best$ssr) best <- list(tau1 = tau1, tau2 = tau2, ssr = ssr)
    }
  }
  expect_equal(fit$tau1, best$tau1)
  expect_equal(fit$tau2, best$tau2)
  expect_equal(fit$ssr, best$ssr, tolerance = 1e-6)
})

test_that("knp_find_break(omit = TRUE) matches a brute-force search with
  the single collapse-date residual subtracted", {
  set.seed(3)
  n <- 26
  y <- cumsum(rnorm(n))
  fit <- exuber:::knp_find_break(y, trim = 0.1, omit = TRUE)

  n1 <- n - 1L
  x <- y[1:n1]; z <- y[2:(n1 + 1)] - y[1:n1]
  k_min <- max(2L, ceiling(0.1 * n1))
  best <- list(ssr = Inf)
  for (tau1 in k_min:(n1 - 2 * k_min)) {
    for (tau2 in (tau1 + k_min):(n1 - k_min)) {
      idx_mid <- (tau1 + 1):tau2
      ssr <- sum(z[1:tau1]^2) +
        sum(resid(lm(z[idx_mid] ~ x[idx_mid]))^2) +
        sum(z[(tau2 + 1):n1]^2) - z[tau2 + 1]^2
      if (ssr < best$ssr) best <- list(tau1 = tau1, tau2 = tau2, ssr = ssr)
    }
  }
  expect_equal(fit$tau1, best$tau1)
  expect_equal(fit$tau2, best$tau2)
  expect_equal(fit$ssr, best$ssr, tolerance = 1e-6)
})

test_that("radf_knp runs end to end and returns a well-formed object", {
  set.seed(1)
  y <- cumsum(rnorm(150))
  out <- dating_knp(y, trim = 0.05)

  expect_s3_class(out, "dating_knp_obj")
  expect_true(is.character(out$origination[["series1"]]))
  expect_true(is.character(out$collapse[["series1"]]))
  expect_true(is.numeric(out$delta[["series1"]]))
  expect_output(print(out), "dating_knp")
})

test_that("radf_knp's omission correction reproduces Kejriwal, Nguyen &
  Perron's own central finding: the naive (omit = FALSE) estimator's
  origination date is badly biased toward the true COLLAPSE date, while
  the omission-corrected estimator is materially more accurate for the
  origination date", {
  skip_on_cran()
  sim_knp <- function(seed, T1 = 50, T2 = 90, T = 200, delta = 1.05) {
    set.seed(seed)
    y <- numeric(T)
    y[1] <- 0
    for (t in 2:T1) y[t] <- y[t - 1] + rnorm(1)
    for (t in (T1 + 1):T2) y[t] <- delta * y[t - 1] + rnorm(1)
    y[T2 + 1] <- y[T1] + rnorm(1)
    if (T2 + 2 <= T) for (t in (T2 + 2):T) y[t] <- y[t - 1] + rnorm(1)
    list(y = y, T1 = T1, T2 = T2)
  }
  run <- function(seed, omit) {
    sim <- sim_knp(seed)
    fit <- exuber:::knp_find_break(sim$y, trim = 0.05, omit = omit)
    c(tau1 = fit$tau1, T1 = sim$T1, T2 = sim$T2)
  }
  res_naive <- t(sapply(1:20, run, omit = FALSE))
  res_om <- t(sapply(1:20, run, omit = TRUE))

  bias_naive_T1 <- mean(abs(res_naive[, "tau1"] - res_naive[, "T1"]))
  bias_naive_T2 <- mean(abs(res_naive[, "tau1"] - res_naive[, "T2"]))
  bias_om_T1 <- mean(abs(res_om[, "tau1"] - res_om[, "T1"]))

  # the naive estimator's tau1 should land much closer to the true
  # COLLAPSE date than to the true origination date (Theorem 1)
  expect_true(bias_naive_T2 < bias_naive_T1)
  # the omission correction should substantially reduce the origination
  # date's bias relative to the naive estimator (Theorem 2)
  expect_true(bias_om_T1 < bias_naive_T1 / 2)
})
