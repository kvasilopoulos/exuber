context("radf_ssu")

test_that("ssu_stat_path (t^{omega,c}) matches a brute-force computation
  from separately fitted lm() regressions and manual residual
  cross-moments, at several window sizes", {
  set.seed(2)
  n <- 150
  y <- cumsum(rnorm(n))
  ps <- exuber:::ssu_prefix_sums(y)

  brute_force_stat <- function(hi) {
    win <- 1:hi
    x1 <- y[win]
    d1 <- y[win + 1] - y[win]
    x2 <- x1^2
    d2 <- d1^2

    fit6 <- lm(d1 ~ x1)
    fit7 <- lm(d2 ~ x2)
    eps_hat <- residuals(fit6)
    eta_hat <- residuals(fit7)
    L <- length(win)
    sigma2_eps <- sum(eps_hat^2) / (L - 2)
    sigma2_eta <- sum(eta_hat^2) / (L - 2)
    sigma2_epseta <- sum(eps_hat * eta_hat) / (L - 1)
    sigma_eps <- sqrt(sigma2_eps)
    sigma_eta <- sqrt(sigma2_eta)
    psi_hat <- sigma2_epseta / (sigma_eps * sigma_eta)

    omega_hat <- unname(coef(fit7)[2])
    Sxx2_c <- sum((x2 - mean(x2))^2)
    t_omega <- omega_hat / sqrt(sigma2_eta / Sxx2_c)

    num_corr <- sum((x2 - mean(x2)) * d1)
    den_corr <- sqrt(Sxx2_c)
    correction <- (psi_hat / sigma_eps) * num_corr / den_corr
    (t_omega - correction) / sqrt(1 - psi_hat^2)
  }

  for (hi_check in c(50, 80, 120, 149)) {
    fast <- exuber:::ssu_stat_path(ps, hi_check)
    manual <- brute_force_stat(hi_check)
    expect_equal(unname(fast), manual, tolerance = 1e-8)
  }
})

test_that("ssu_q looks up Kurozumi & Nishi (2025) Table I exactly and
  errors on an untabulated level", {
  expect_equal(exuber:::ssu_q(0.90), 2.90)
  expect_equal(exuber:::ssu_q(0.95), 3.30)
  expect_equal(exuber:::ssu_q(0.99), 4.20)
  expect_error(exuber:::ssu_q(0.93), "must be one of")
})

test_that("radf_ssu runs end to end and returns a well-formed object", {
  set.seed(1)
  y <- cumsum(rnorm(100))
  out <- ssu_test(y, level = 0.95)

  expect_s3_class(out, "ssu_test_obj")
  expect_true(is.matrix(out$stat))
  expect_equal(unname(out$crit), 3.30)
  expect_equal(unname(out$sadf), max(out$stat[, 1]))
  expect_output(print(out), "ssu_test")
})

test_that("radf_ssu's minw matches psy_minw() by default (SSU's own
  r0 = 0.01 + 1.8/sqrt(T) is exactly exuber's existing convention)", {
  set.seed(1)
  y <- cumsum(rnorm(120))
  out <- ssu_test(y)
  expect_equal(attr(out, "minw"), psy_minw(120))
})

test_that("radf_ssu rejects an untabulated level", {
  y <- cumsum(rnorm(60))
  expect_error(ssu_test(y, level = 0.80))
})

test_that("radf_ssu's empirical false-alarm rate under H0 is close to
  nominal at all three tabulated levels", {
  skip_on_cran()
  set.seed(1)
  nrep <- 100
  n <- 150
  run <- function(level) {
    mean(vapply(seq_len(nrep), function(i) {
      set.seed(1000 + i)
      y <- cumsum(rnorm(n))
      unname(ssu_test(y, level = level)$detected)
    }, logical(1)))
  }
  expect_lt(run(0.90), 0.25)
  expect_lt(run(0.95), 0.20)
  expect_lt(run(0.99), 0.10)
})

test_that("radf_ssu has non-trivial detection power on a stochastic
  -explosive-coefficient DGP (the alternative it's designed for)", {
  skip_on_cran()
  set.seed(2)
  nrep <- 40
  n <- 150
  make_stochastic_bubble <- function(n, te_frac = 0.5, c1 = 3, a = 4) {
    y <- numeric(n)
    y[1] <- rnorm(1)
    Te <- round(te_frac * n)
    for (t in 2:n) {
      if (t <= Te) {
        y[t] <- y[t - 1] + rnorm(1)
      } else {
        rho_t <- 1 + c1 / n + a * rnorm(1) / sqrt(n)
        y[t] <- rho_t * y[t - 1] + rnorm(1)
      }
    }
    y
  }
  rate <- mean(vapply(seq_len(nrep), function(i) {
    set.seed(2000 + i)
    y <- make_stochastic_bubble(n)
    unname(ssu_test(y, level = 0.95)$detected)
  }, logical(1)))
  expect_gt(rate, 0.3)
})
