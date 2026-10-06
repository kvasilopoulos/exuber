context("dating_knp")

test_that("knp_find_break(omit = FALSE) matches a brute-force nested lm() search", {
  set.seed(3)
  n <- 26
  y <- cumsum(rnorm(n))
  fit <- exuber:::knp_find_break(y, trim = 0.1, omit = FALSE)

  n1 <- n - 1L
  x <- y[1:n1]
  z <- y[2:(n1 + 1)] - y[1:n1]
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
  x <- y[1:n1]
  z <- y[2:(n1 + 1)] - y[1:n1]
  k_min <- max(2L, ceiling(0.1 * n1))
  best <- list(ssr = Inf)
  for (tau1 in k_min:(n1 - 2 * k_min)) {
    for (tau2 in (tau1 + k_min):(n1 - k_min)) {
      idx_mid <- (tau1 + 1):tau2
      ssr <- sum(z[1:tau1]^2) +
        sum(resid(lm(z[idx_mid] ~ x[idx_mid]))^2) +
        sum(z[(tau2 + 1):n1]^2) -
        z[tau2 + 1]^2
      if (ssr < best$ssr) best <- list(tau1 = tau1, tau2 = tau2, ssr = ssr)
    }
  }
  expect_equal(fit$tau1, best$tau1)
  expect_equal(fit$tau2, best$tau2)
  expect_equal(fit$ssr, best$ssr, tolerance = 1e-6)
})

test_that("dating_knp runs end to end and returns a well-formed object", {
  set.seed(1)
  y <- cumsum(rnorm(150))
  out <- dating_knp(y, trim = 0.05)

  expect_s3_class(out, "dating_knp_obj")
  expect_true(is.character(out$origination[["series1"]]))
  expect_true(is.character(out$collapse[["series1"]]))
  expect_true(is.numeric(out$delta[["series1"]]))
  expect_output(print(out), "dating_knp")
})

test_that("dating_knp's omission correction reproduces Kejriwal, Nguyen &
  Perron's own central finding: the naive (omit = FALSE) estimator's
  origination date is badly biased toward the true COLLAPSE date, while
  the omission-corrected estimator is materially more accurate for the
  origination date", {
  skip_on_cran()
  sim_knp <- function(seed, T1 = 50, T2 = 90, n_obs = 200, delta = 1.05) {
    set.seed(seed)
    y <- numeric(n_obs)
    y[1] <- 0
    for (t in 2:T1) {
      y[t] <- y[t - 1] + rnorm(1)
    }
    for (t in (T1 + 1):T2) {
      y[t] <- delta * y[t - 1] + rnorm(1)
    }
    y[T2 + 1] <- y[T1] + rnorm(1)
    if (T2 + 2 <= n_obs) {
      for (t in (T2 + 2):n_obs) {
        y[t] <- y[t - 1] + rnorm(1)
      }
    }
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

# Brute-force KNP objective over every admissible partition (regimes
# alternate unit root / explosive, each >= k_min pairs; a unit-root regime
# after a collapse drops its first residual when omit = TRUE).
knp_brute <- function(y, breaks, trim, omit) {
  n1 <- length(y) - 1L
  x <- y[1:n1]
  z <- diff(y)
  k_min <- max(2L, ceiling(trim * n1))
  seg <- function(j, lo, hi) {
    k <- (lo + 1):hi
    if (j %% 2 == 0) {
      return(sum(resid(lm(z[k] ~ x[k]))^2))
    }
    sum(z[k]^2) - if (omit && j > 1) z[lo + 1]^2 else 0
  }
  best <- list(ssr = Inf)
  rec <- function(taus) {
    j <- length(taus)
    if (j == breaks) {
      ends <- c(0, taus, n1)
      ssr <- sum(vapply(seq_len(breaks + 1), function(r) seg(r, ends[r], ends[r + 1]), 0))
      if (ssr < best$ssr) {
        best <<- list(tau = taus, ssr = ssr)
      }
      return(invisible())
    }
    from <- (if (j == 0) 0 else taus[j]) + k_min
    to <- n1 - (breaks - j) * k_min
    if (from <= to) {
      for (t in from:to) {
        rec(c(taus, t))
      }
    }
  }
  rec(integer(0))
  best
}

test_that("knp_dp(breaks = 2) reproduces the single-bubble exhaustive search", {
  for (omit in c(TRUE, FALSE)) {
    set.seed(11)
    y <- cumsum(rnorm(80))
    dp <- exuber:::knp_dp(y, 2, trim = 0.05, omit = omit)
    fb <- exuber:::knp_find_break(y, trim = 0.05, omit = omit)
    expect_equal(dp$tau, c(fb$tau1, fb$tau2))
    expect_equal(dp$ssr, fb$ssr, tolerance = 1e-10)
  }
})

test_that("knp_dp matches a brute-force search over every partition for
  3 and 4 breaks", {
  set.seed(12)
  y <- cumsum(rnorm(28))
  for (breaks in 3:4) {
    for (omit in c(TRUE, FALSE)) {
      dp <- exuber:::knp_dp(y, breaks, trim = 0.1, omit = omit)
      b <- knp_brute(y, breaks, trim = 0.1, omit = omit)
      expect_equal(dp$tau, b$tau, info = paste(breaks, omit))
      expect_equal(dp$ssr, b$ssr, tolerance = 1e-8, info = paste(breaks, omit))
    }
  }
})

test_that("dating_knp(breaks = 4) dates two well-separated bubbles", {
  set.seed(13)
  e <- rnorm(200)
  y <- numeric(200)
  y[1] <- 10
  for (t in 2:200) {
    rho <- if ((t > 40 && t <= 70) || (t > 120 && t <= 150)) 1.06 else 1
    y[t] <- rho * y[t - 1] + e[t]
    if (t == 71 || t == 151) y[t] <- 10 + e[t] # instantaneous collapse
  }
  out <- dating_knp(y, breaks = 4)
  expect_true(is.matrix(out$origination))
  expect_equal(dim(out$origination), c(2L, 1L))
  expect_lt(max(abs(as.numeric(out$origination) - c(41, 121))), 10)
  expect_lt(max(abs(as.numeric(out$collapse) - c(70, 150))), 3)
  expect_output(print(out), "breaks = 4")
  ongoing <- dating_knp(y[1:140], breaks = 3)
  expect_true(is.na(ongoing$collapse[2, 1]))
})
