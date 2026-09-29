context("monitor_quantile")

test_that("qpwy_stat_path at the full sample matches a manual replicate
  of quantile_test()'s own per-window QR t-ratio formula exactly", {
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

test_that("quantile_boundary_sim matches a brute-force per-window
  computation of Q and Z (sup of delta*Q + sqrt(1-delta^2)*Z)", {
  n <- 30
  minw <- 8
  delta <- c(0.3, 0.9)
  for (type in "qpwy") {
    sim <- exuber:::quantile_boundary_sim(n, minw, 2, delta, seed = 11)
    set.seed(11)
    for (i in 1:2) {
      e <- rnorm(n - 1)
      v <- rnorm(n - 1)
      x <- c(0, cumsum(e))[1:(n - 1)]
      U <- NULL
      for (hi in minw:(n - 1)) {
        los <- if (type == "qpwy") 0 else 0:(hi - minw)
        for (lo in los) {
          k <- (lo + 1):hi
          xb <- x[k] - mean(x[k])
          q <- sum(xb * e[k]) / sqrt(sum(xb^2))
          z <- sum(xb * v[k]) / sqrt(sum(xb^2))
          U <- rbind(U, delta * q + sqrt(1 - delta^2) * z)
        }
      }
      expect_equal(sim[i, ], apply(U, 2, max), tolerance = 1e-8)
    }
  }
})

test_that("the boundary treats Z as a process over windows, not one z
  per replicate (the single-z boundary oversized the test)", {
  # delta = 0 leaves only Z; with one shared z per path, sup_r Z would be
  # exactly N(0,1), so its 95% quantile would be ~1.645
  sim <- exuber:::quantile_boundary_sim(150, 20, 400, 0, seed = 3)
  expect_gt(quantile(sim[, 1], 0.95), 2)
})

test_that("monitor_quantile runs end to end and returns a well-formed object", {
  set.seed(1)
  y <- cumsum(rnorm(80))
  out <- monitor_quantile(y, tau = 0.5, nrep = 50, seed = 1)

  expect_s3_class(out, "monitor_quantile_obj")
  expect_true(is.matrix(out$stat))
  expect_true(is.numeric(out$boundary) && length(out$boundary) == 1)
  expect_true(out$delta >= -1 && out$delta <= 1)
  expect_output(print(out), "monitor_quantile")
})

test_that("monitor_quantile's boundary is the quantile of simulated PATH
  MAXIMA (controlling the supremum/first-crossing probability), not a
  per-r marginal quantile -- the bug an initial version had (~50%
  false-alarm rate against a nominal 5%) before this was fixed", {
  set.seed(1)
  y <- cumsum(rnorm(80))
  out <- monitor_quantile(y, tau = 0.5, nrep = 200, seed = 7)
  sup_U <- exuber:::quantile_boundary_sim(80, attr(out, "minw"), 200, out$delta, seed = 7)
  expect_equal(unname(out$boundary), unname(quantile(sup_U[, 1], 0.95)))
})

test_that("monitor_quantile rejects an out-of-range tau or level", {
  y <- cumsum(rnorm(60))
  expect_error(monitor_quantile(y, tau = 1.5))
  expect_error(monitor_quantile(y, sig_lvl = 80))
})

test_that("monitor_quantile's false-alarm rate under H0 is not wildly inflated", {
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

test_that("monitor_quantile has non-trivial detection power on a genuine
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
