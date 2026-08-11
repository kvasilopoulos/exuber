context("sim-dgp")

# sim_psy1 extensions -------------------------------------------------------

test_that("sim_psy1 e = NULL reproduces the original DGP exactly (formula check)", {
  n <- 80
  set.seed(42)
  delta <- 1 + 1 * n ^ (-0.6)
  te <- 0.4 * n
  tf <- 0.15 * n + te
  y <- 100
  for (t in 2:n) {
    if (t < te) {
      y[t] <- y[t - 1] + rnorm(1, sd = 6.79)
    } else if (t >= te & t <= tf) {
      y[t] <- delta * y[t - 1] + rnorm(1, sd = 6.79)
    } else if (t == tf + 1) {
      y[t] <- y[te] + rnorm(1, sd = 6.79)
    } else {
      y[t] <- y[t - 1] + rnorm(1, sd = 6.79)
    }
  }
  x <- sim_psy1(n, seed = 42)
  expect_equal(as.numeric(x), y)
})

test_that("sim_psy1 'e' overrides the default innovations and validates length", {
  n <- 30
  zero_shocks <- rep(0, n - 1)
  x <- sim_psy1(n, seed = 1, e = zero_shocks)
  # with zero noise, pre-bubble/post-bubble regimes are flat, bubble regime is geometric
  expect_equal(as.numeric(x)[1:2], c(100, 100))
  expect_error(sim_psy1(n, seed = 1, e = rep(0, n)), "length n - 1")
})

test_that("sim_psy1 'shifts' adds one-period jumps that carry forward additively
          within the (random-walk) pre-bubble regime", {
  # te defaults to 0.4*n = 16, so keep both shift dates and the comparison
  # window strictly before te -- once a shift enters the bubble's
  # *multiplicative* regime it gets amplified by delta each step, so a flat
  # superposition would no longer hold there (a DGP property, not a bug).
  n <- 40
  x0 <- sim_psy1(n, seed = 7)
  x1 <- sim_psy1(n, seed = 7, shifts = list(date = c(5, 12), size = c(30, -15)))
  diff <- as.numeric(x1) - as.numeric(x0)
  expect_equal(diff[2:4], rep(0, 3))
  expect_equal(diff[5:11], rep(30, 7))
  expect_equal(diff[12:15], rep(15, 4))
})

test_that("sim_psy1 'coef_noise' of all zeros is equivalent to fixed delta", {
  n <- 50
  x0 <- sim_psy1(n, seed = 3)
  x1 <- sim_psy1(n, seed = 3, coef_noise = rep(0, n - 1), coef_a = 5)
  expect_equal(x0, x1)
  expect_error(sim_psy1(n, seed = 3, coef_noise = rep(0, n)), "length n - 1")
})

# innovation / volatility generators -----------------------------------------

sample_skewness <- function(x) {
  m <- mean(x)
  mean((x - m) ^ 3) / sd(x) ^ 3
}

test_that("sim_innov standardizes each distribution to mean 0 / sd sigma", {
  n <- 200000
  sigma <- 2.5
  for (dist in c("normal", "t")) {
    z <- sim_innov(n, dist = dist, sigma = sigma, df = 6, seed = 1)
    expect_equal(mean(z), 0, tolerance = 0.05)
    expect_equal(sd(z), sigma, tolerance = 0.05)
  }
  z_skew <- sim_innov(n, dist = "skew_t", sigma = sigma, df = 6, xi = -0.75, seed = 1)
  expect_equal(mean(z_skew), 0, tolerance = 0.05)
  expect_equal(sd(z_skew), sigma, tolerance = 0.05)
  expect_true(sample_skewness(z_skew) < 0) # left-skewed for xi < 0
})

test_that("sim_vol_garch reproduces the GARCH(1,1) recursion exactly (formula check)", {
  set.seed(11)
  n <- 15
  eps <- rnorm(n)
  h <- numeric(n); z <- numeric(n)
  h_prev <- 0; z_prev <- 0
  for (t in seq_len(n)) {
    h[t] <- 0.1 + 0.1 * z_prev ^ 2 + 0.8 * h_prev
    z[t] <- sqrt(h[t]) * eps[t]
    h_prev <- h[t]; z_prev <- z[t]
  }
  set.seed(11)
  out <- sim_vol_garch(n, seed = NULL)
  expect_equal(as.numeric(out), z)
})

test_that("sim_vol_garch TGARCH branch reacts asymmetrically to sign", {
  z_up <- sim_vol_garch(5000, omega = 0.5, alpha = 0, beta = 0.5, gamma = 0.4, seed = 1)
  z_flat <- sim_vol_garch(5000, omega = 0.5, alpha = 0, beta = 0.5, gamma = 0, seed = 1)
  # adding a leverage term (gamma > 0) with everything else equal must increase
  # unconditional variance, since it only ever adds a non-negative term to h_t
  expect_gt(var(z_up), var(z_flat))
})

test_that("sim_vol_cir and sim_vol_sv produce finite, reproducible series", {
  a <- sim_vol_cir(100, seed = 5)
  b <- sim_vol_cir(100, seed = 5)
  expect_equal(a, b)
  expect_true(all(is.finite(a)))

  c1 <- sim_vol_sv(100, seed = 5)
  c2 <- sim_vol_sv(100, seed = 5)
  expect_equal(c1, c2)
  expect_true(all(is.finite(c1)))
})

test_that("sim_fi has no missing values and shows long memory for larger d", {
  v <- sim_fi(3000, d = 0.3, seed = 1)
  expect_false(anyNA(v))
  expect_equal(length(v), 3000)

  acf1 <- function(x) cor(x[-1], x[-length(x)])
  strong <- acf1(sim_fi(3000, d = 0.45, seed = 2))
  weak <- acf1(sim_fi(3000, d = 0.01, seed = 2))
  expect_gt(strong, weak)
})

# sim_blan: rotermann_wilfling ------------------------------------------------

test_that("sim_blan(type = 'rotermann_wilfling') stays strictly positive", {
  b <- sim_blan(300, type = "rotermann_wilfling", delta = 0.984, seed = 9)
  expect_true(all(b > 0))
})

test_that("sim_blan default type is unaffected by the new arguments", {
  x0 <- sim_blan(100, seed = 4)
  x1 <- sim_blan(100, seed = 4, delta = 0.5, rw_sigma = 0.9)
  expect_equal(x0, x1)
})

# sim_tree --------------------------------------------------------------------

test_that("sim_tree never drops below its price floor (Corollary 1)", {
  y <- sim_tree(500, a = 0.95, eta = 1, seed = 1)
  floor <- 1 / (1 - 0.95)
  expect_true(all(y >= floor - 1e-8))
})

# sim_mar -----------------------------------------------------------------

test_that("sim_mar is reproducible and finite", {
  a <- sim_mar(50, seed = 6)
  b <- sim_mar(50, seed = 6)
  expect_equal(a, b)
  expect_true(all(is.finite(a)))
  expect_equal(length(a), 50)
})

# sim_common --------------------------------------------------------------

test_that("sim_common series share more comovement than pure idiosyncratic noise", {
  x <- sim_common(4, 200, sigma_e = 0.1, seed = 8)
  expect_equal(dim(x), c(200, 4))
  avg_cor <- mean(cor(x)[lower.tri(cor(x))])
  expect_gt(avg_cor, 0.9) # dominated by the shared factor, sigma_e small
})

# sim_coexplosive -----------------------------------------------------------

test_that("sim_coexplosive recovers the true lag", {
  set.seed(21)
  d <- sim_coexplosive(300, lag = 5, phi_x = 1, sigma_y = 1, seed = 21)
  lags <- -10:10
  ccf_vals <- vapply(lags, function(k) {
    idx <- seq_len(nrow(d)) - k
    valid <- idx >= 1 & idx <= nrow(d)
    suppressWarnings(cor(d$y[valid], d$x[idx[valid]], use = "complete.obs"))
  }, numeric(1))
  expect_equal(lags[which.max(abs(ccf_vals))], 5)
})

# sim_msbubble --------------------------------------------------------------

test_that("sim_msbubble regime path only takes values 1/2 and matches nominal persistence", {
  b <- sim_msbubble(5000, p11 = 0.95, p22 = 0.9, seed = 15)
  s <- attr(b, "regime")
  expect_true(all(s %in% c(1L, 2L)))
  # empirical P(stay in regime 1) close to p11
  in1 <- which(s[-length(s)] == 1L)
  emp_p11 <- mean(s[in1 + 1] == 1L)
  expect_equal(emp_p11, 0.95, tolerance = 0.03)
})

# sim_falsebubble -------------------------------------------------------------

test_that("sim_falsebubble reduces to the plain dividend fundamental when amplitude = 0", {
  n <- 100
  set.seed(2)
  eta <- rnorm(n - 1, sd = 0.05)
  mu <- 0.02; r <- 0.05
  d <- numeric(n); d[1] <- 0
  for (t in 2:n) d[t] <- d[t - 1] + mu + eta[t - 1]
  pf <- mu * (1 + r) * r ^ (-2) + d / r

  set.seed(2)
  p <- sim_falsebubble(n, amplitude = 0, mu = mu, sigma_d = 0.05, r = r, seed = NULL)
  expect_equal(as.numeric(p), pf)
})

test_that("sim_falsebubble's technology term is zero outside [t1, t2] and positive at its peak", {
  p <- sim_falsebubble(200, t1 = 50, t2 = 150, kappa = 40, seed = 3)
  tau <- attr(p, "technology")
  expect_true(all(tau[1:49] == 0))
  expect_true(all(tau[151:200] == 0))
  expect_gt(tau[90], 0)
})
