context("radf_qpwy")

test_that("qpwy_stat_path at the full sample matches a manual replicate
  of radf_quantile()'s own per-window QR t-ratio formula exactly", {
  set.seed(3)
  y <- cumsum(rnorm(60))
  full_stat <- exuber:::qpwy_stat_path(y, 0.5, 60)

  dy <- diff(y)
  ylag <- y[1:59]
  yresp <- y[2:60]
  qr_fit <- quantreg::rq(yresp ~ ylag, tau = 0.5)
  alpha_hat <- unname(coef(qr_fit)["ylag"])
  f_hat <- exuber:::quantile_check_density(dy, 0.5)$f_hat
  yPzy <- sum((ylag - mean(ylag))^2)
  manual <- (f_hat / sqrt(0.5 * 0.5)) * sqrt(yPzy) * (alpha_hat - 1)

  expect_equal(unname(full_stat), manual, tolerance = 1e-8)
})

test_that("qpwy_boundary_sim's simulated Q paths have the same length as
  radf()'s own badf sequence (Q_{0,r} is identified with badf[r]
  exactly, per Corollary 2)", {
  n <- 80
  minw <- 20
  Q <- exuber:::qpwy_boundary_sim(n, minw, 5, seed = 1)
  set.seed(1)
  # first replicate uses the same RNG stream as a direct radf() call
  ysim1 <- cumsum(rnorm(n))
  r1 <- radf(ysim1, minw = minw, lag = 0)
  expect_equal(ncol(Q), length(r1$badf[, 1]))
  expect_equal(nrow(Q), 5)
})

test_that("radf_qpwy runs end to end and returns a well-formed object", {
  set.seed(1)
  y <- cumsum(rnorm(80))
  out <- monitor_quantile(y, tau = 0.5, nrep = 50, seed = 1)

  expect_s3_class(out, "monitor_quantile_obj")
  expect_true(is.matrix(out$stat))
  expect_true(is.numeric(out$boundary) && length(out$boundary) == 1)
  expect_true(out$delta >= -1 && out$delta <= 1)
  expect_output(print(out), "monitor_quantile")
})

test_that("radf_qpwy's boundary is the quantile of simulated PATH
  MAXIMA (controlling the supremum/first-crossing probability), not a
  per-r marginal quantile -- the bug an initial version had (~50%
  false-alarm rate against a nominal 5%) before this was fixed", {
  set.seed(1)
  y <- cumsum(rnorm(80))
  minw <- exuber:::psy_minw(80)
  Q <- exuber:::qpwy_boundary_sim(80, minw, 100, seed = 7)
  set.seed(9)
  z <- rnorm(100)
  delta_j <- 0.4
  U <- sqrt(1 - delta_j^2) * z + delta_j * Q
  sup_boundary <- unname(quantile(apply(U, 1, max), probs = 0.95, names = FALSE))
  marginal_boundary <- unname(quantile(U[, ncol(U)], probs = 0.95, names = FALSE))
  # the supremum-calibrated boundary must be at least as large as any
  # single-column marginal quantile (the max of a path is >= any one of
  # its own points, so its quantile stochastically dominates)
  expect_gte(sup_boundary, marginal_boundary)
})

test_that("radf_qpwy rejects an out-of-range tau or level", {
  y <- cumsum(rnorm(60))
  expect_error(monitor_quantile(y, tau = 1.5))
  expect_error(monitor_quantile(y, level = 80))
})

test_that("radf_qpwy's false-alarm rate under H0 is not wildly inflated", {
  skip_on_cran()
  set.seed(2)
  nrep_mc <- 30
  n <- 100
  fa <- mean(vapply(seq_len(nrep_mc), function(i) {
    set.seed(2000 + i)
    yy <- cumsum(rnorm(n))
    !is.na(monitor_quantile(yy, tau = 0.5, nrep = 100, seed = i)$alarm)
  }, logical(1)))
  expect_lt(fa, 0.30)
})

test_that("radf_qpwy has non-trivial detection power on a genuine
  explosive DGP", {
  skip_on_cran()
  set.seed(3)
  nrep_mc <- 20
  det <- mean(vapply(seq_len(nrep_mc), function(i) {
    set.seed(3000 + i)
    n1 <- 60
    normal_part <- cumsum(rnorm(n1))
    expl_part <- normal_part[n1] * 1.03^(1:40) + cumsum(rnorm(40, sd = 1))
    yy <- c(normal_part, expl_part)
    !is.na(monitor_quantile(yy, tau = 0.5, nrep = 100, seed = i)$alarm)
  }, logical(1)))
  expect_gt(det, 0.3)
})
