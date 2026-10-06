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
  for (type in c("qpwy", "qpsy")) {
    sim <- exuber:::quantile_boundary_sim(n, minw, 2, delta, type = type, seed = 11)
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

test_that("the QPSY grid contains the QPWY path, so its suprema dominate", {
  wy <- exuber:::quantile_boundary_sim(60, 12, 20, c(0.2, 0.8), type = "qpwy", seed = 5)
  sy <- exuber:::quantile_boundary_sim(60, 12, 20, c(0.2, 0.8), type = "qpsy", seed = 5)
  expect_true(all(sy >= wy - 1e-12))
})

test_that("qpsy_stat_path is the sup over window starts of quantreg::rq()
  per-window t-ratios", {
  set.seed(4)
  y <- cumsum(rnorm(40))
  minw <- 10
  r_idx <- (minw + 1):40
  path <- exuber:::qpsy_stat_path(y, 0.7, r_idx, minw)
  brute <- vapply(
    r_idx,
    function(r) {
      max(vapply(
        1:(r - minw),
        function(r1) {
          yy <- y[r1:r]
          m <- length(yy)
          ylag <- yy[1:(m - 1)]
          yresp <- yy[2:m]
          a <- unname(coef(quantreg::rq(yresp ~ ylag, tau = 0.7))["ylag"])
          f <- exuber:::quantile_check_density(yresp - ylag, 0.7)$f_hat
          (f / sqrt(0.7 * 0.3)) * sqrt(sum((ylag - mean(ylag))^2)) * (a - 1)
        },
        numeric(1)
      ))
    },
    numeric(1)
  )
  expect_equal(path, brute, tolerance = 1e-8)
  expect_equal(path[1], exuber:::qpwy_stat_path(y, 0.7, minw + 1), tolerance = 1e-12)
})

test_that("monitor_quantile(type = 'qpsy') runs end to end", {
  set.seed(1)
  y <- cumsum(rnorm(50))
  out <- monitor_quantile(y, tau = 0.5, nrep = 30, seed = 1, type = "qpsy")
  expect_s3_class(out, "monitor_quantile_obj")
  expect_equal(attr(out, "type"), "qpsy")
  expect_output(print(out), "QPSY")
  expect_null(attr(out, "caveat"))
  expect_message(
    off <- monitor_quantile(y, tau = 0.8, nrep = 30, seed = 1, type = "qpsy"),
    "oversized"
  )
  expect_output(print(off), "oversized")
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
  fa <- mean(vapply(
    seq_len(nrep_mc),
    function(i) {
      set.seed(2000 + i)
      yy <- cumsum(rnorm(n))
      !is.na(monitor_quantile(yy, tau = 0.5, nrep = 100, seed = i)$alarm)
    },
    logical(1)
  ))
  expect_lt(fa, 0.30)
})

test_that("monitor_quantile has non-trivial detection power on a genuine
  explosive DGP", {
  skip_on_cran()
  set.seed(3)
  nrep_mc <- 20
  det <- mean(vapply(
    seq_len(nrep_mc),
    function(i) {
      set.seed(3000 + i)
      n1 <- 60
      normal_part <- cumsum(rnorm(n1))
      expl_part <- normal_part[n1] * 1.03^(1:40) + cumsum(rnorm(40, sd = 1))
      yy <- c(normal_part, expl_part)
      !is.na(monitor_quantile(yy, tau = 0.5, nrep = 100, seed = i)$alarm)
    },
    logical(1)
  ))
  expect_gt(det, 0.3)
})

test_that("quantile_boot_max is the path maximum of quantreg::rq() window
  t-ratios on the resampled, cumulated first differences", {
  set.seed(8)
  y <- cumsum(rt(30, df = 3))
  u <- diff(y)
  u <- u - mean(u)
  minw <- 8
  rq_stat <- function(yy, tau) {
    m <- length(yy)
    ylag <- yy[1:(m - 1)]
    yresp <- yy[2:m]
    a <- unname(coef(quantreg::rq(yresp ~ ylag, tau = tau))["ylag"])
    f <- exuber:::quantile_check_density(yresp - ylag, tau)$f_hat
    (f / sqrt(tau * (1 - tau))) * sqrt(sum((ylag - mean(ylag))^2)) * (a - 1)
  }
  for (type in c("qpwy", "qpsy")) {
    set.seed(12)
    got <- exuber:::quantile_boot_max(u, 0.7, minw, type)
    set.seed(12)
    ys <- cumsum(c(0, sample(u, 29, replace = TRUE)))
    want <- max(vapply(
      (minw + 1L):30,
      function(r) {
        if (type == "qpwy") {
          rq_stat(ys[1:r], 0.7)
        } else {
          max(vapply(1:(r - minw), function(r1) rq_stat(ys[r1:r], 0.7), numeric(1)))
        }
      },
      numeric(1)
    ))
    expect_equal(got, want, tolerance = 1e-8)
  }
})

test_that("the bootstrap boundary is reproducible, does not depend on the
  level of the series, and is the quantile of the bootstrap path maxima", {
  set.seed(6)
  y <- cumsum(rt(40, df = 3))
  a <- exuber:::quantile_boundary_boot(y, 0.8, 10, 12, seed = 4)
  b <- exuber:::quantile_boundary_boot(y + 25, 0.8, 10, 12, seed = 4)
  expect_equal(a, b, tolerance = 1e-8)
  expect_equal(a, exuber:::quantile_boundary_boot(y, 0.8, 10, 12, seed = 4))

  res <- monitor_quantile(y, tau = 0.8, minw = 10, nrep = 12, seed = 4, boundary = "bootstrap")
  expect_equal(unname(res$boundary), unname(quantile(a, 0.95)), tolerance = 1e-8)
  expect_identical(attr(res, "boundary_type"), "bootstrap")
  expect_identical(
    attr(monitor_quantile(y, minw = 10, nrep = 20, seed = 1), "boundary_type"),
    "asymptotic"
  )
})

test_that("QPSY with the bootstrap boundary does not emit the asymptotic
  caveat, and QPSY with the asymptotic boundary still does", {
  set.seed(7)
  y <- cumsum(rnorm(30))
  expect_message(
    res <- monitor_quantile(y, tau = 0.8, minw = 10, nrep = 5, seed = 1, type = "qpsy"),
    "oversized"
  )
  expect_false(is.null(attr(res, "caveat")))
  expect_no_message(
    res_b <- monitor_quantile(
      y,
      tau = 0.8,
      minw = 10,
      nrep = 5,
      seed = 1,
      type = "qpsy",
      boundary = "bootstrap"
    )
  )
  expect_null(attr(res_b, "caveat"))
})
