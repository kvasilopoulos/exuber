context("radf_contagion")

test_that("contagion_fixed_window_beta matches a brute-force lm() fit
  exactly, at several window-end dates (S levels per window, S-1
  regression pairs, per Greenaway-McGrevy & Phillips's own definition)", {
  set.seed(1)
  n <- 150
  S <- 50
  core <- cumsum(rnorm(n))
  beta_core <- exuber:::contagion_fixed_window_beta(core, S)

  for (t_check in c(60, 80, 100, 130, 150)) {
    win <- (t_check - S + 1):t_check
    fit <- lm(core[win[-1]] ~ core[win[-length(win)]])
    expect_equal(
      unname(beta_core[as.character(t_check)]),
      unname(coef(fit)[2]),
      tolerance = 1e-8
    )
  }
})

test_that("contagion_nw_delta2 (eq. 6) matches a manual Gaussian-kernel
  weighted-least-squares ratio exactly", {
  set.seed(1)
  n <- 150
  S <- 50
  core <- cumsum(rnorm(n))
  y <- 0.5 * core + cumsum(rnorm(n, sd = 0.5))
  bc <- exuber:::contagion_fixed_window_beta(core, S)
  bj <- exuber:::contagion_fixed_window_beta(y, S)

  d <- 2
  r_test <- 0.5
  h_test <- 0.2
  fast <- exuber:::contagion_nw_delta2(bc, bj, n, r_test, h_test, d)

  s <- as.integer(names(bj))
  core_shift <- (bc - mean(bc))[as.character(s - d)]
  valid <- !is.na(core_shift)
  s2 <- s[valid]
  bjc <- (bj - mean(bj))[valid]
  csh <- core_shift[valid]
  w <- dnorm((s2 / n - r_test) / h_test) / h_test
  manual <- sum(w * bjc * csh) / sum(w * csh^2)

  expect_equal(unname(fast), manual, tolerance = 1e-10)
})

test_that("contagion_loocv_sse (eq. 7) matches a manual double loop
  (leave-one-out kernel regression) exactly", {
  set.seed(1)
  n <- 150
  S <- 50
  core <- cumsum(rnorm(n))
  y <- 0.5 * core + cumsum(rnorm(n, sd = 0.5))
  bc <- exuber:::contagion_fixed_window_beta(core, S)
  bj <- exuber:::contagion_fixed_window_beta(y, S)
  d <- 2
  h_test <- 0.25

  fast_sse <- exuber:::contagion_loocv_sse(h_test, bc, bj, n, d)

  bj_c <- bj - mean(bj)
  bc_c <- bc - mean(bc)
  s <- as.integer(names(bj))
  core_shift <- bc_c[as.character(s - d)]
  valid <- !is.na(core_shift)
  s2 <- s[valid]
  bjc2 <- bj_c[valid]
  csh2 <- core_shift[valid]
  m <- length(s2)
  manual_sse <- 0
  for (i in seq_len(m)) {
    r_i <- s2[i] / m
    num <- 0
    den <- 0
    for (p in seq_len(m)) {
      if (p == i) next
      w <- dnorm((s2[p] / n - r_i) / h_test) / h_test
      num <- num + w * bjc2[p] * csh2[p]
      den <- den + w * csh2[p]^2
    }
    pred <- (num / den) * csh2[i]
    manual_sse <- manual_sse + (bjc2[i] - pred)^2
  }
  expect_equal(as.numeric(fast_sse), as.numeric(manual_sse), tolerance = 1e-8)
})

test_that("contagion_bandwidth_cv picks a bandwidth inside eq. 7's own
  H_T interval and achieves LOOCV SSE no worse than either endpoint", {
  set.seed(1)
  n <- 150
  S <- 50
  core <- cumsum(rnorm(n))
  y <- 0.5 * core + cumsum(rnorm(n, sd = 0.5))
  bc <- exuber:::contagion_fixed_window_beta(core, S)
  bj <- exuber:::contagion_fixed_window_beta(y, S)
  d <- 2

  h_opt <- exuber:::contagion_bandwidth_cv(bc, bj, n, d)
  m <- length(bj)
  H_T <- c(m^(-1 / 2), m^(-1 / 10))

  expect_gte(h_opt, H_T[1])
  expect_lte(h_opt, H_T[2])
  sse_opt <- exuber:::contagion_loocv_sse(h_opt, bc, bj, n, d)
  sse_lo <- exuber:::contagion_loocv_sse(H_T[1], bc, bj, n, d)
  sse_hi <- exuber:::contagion_loocv_sse(H_T[2], bc, bj, n, d)
  expect_lte(sse_opt, sse_lo + 1e-6)
  expect_lte(sse_opt, sse_hi + 1e-6)
})

test_that("radf_contagion runs end to end and returns a well-formed
  object", {
  set.seed(1)
  n <- 150
  core <- cumsum(rnorm(n))
  y <- 0.5 * core + cumsum(rnorm(n, sd = 0.5))
  out <- contagion_reg(y, core, S = 50, d = 2)

  expect_s3_class(out, "contagion_reg_obj")
  expect_length(out$beta_core, length(out$beta_j))
  expect_length(out$delta2, length(out$r_grid))
  expect_true(is.numeric(out$h) && out$h > 0)
  expect_output(print(out), "contagion_reg")
})

test_that("radf_contagion accepts a user-supplied bandwidth, skipping CV", {
  set.seed(1)
  n <- 150
  core <- cumsum(rnorm(n))
  y <- 0.5 * core + cumsum(rnorm(n, sd = 0.5))
  out <- contagion_reg(y, core, S = 50, d = 1, h = 0.3)
  expect_equal(out$h, 0.3)
})

test_that("radf_contagion rejects mismatched series lengths", {
  expect_error(contagion_reg(rnorm(10), rnorm(11)))
})

test_that("radf_contagion's estimated delta2(r) varies more over r for a
  series with a genuine time-varying relationship to the core than for
  one independent of it", {
  skip_on_cran()
  n <- 150
  S <- 50
  nrep <- 15
  planted_range <- indep_range <- numeric(nrep)
  for (i in seq_len(nrep)) {
    set.seed(1000 + i)
    core_i <- cumsum(rnorm(n))
    y_planted <- numeric(n)
    y_planted[1:10] <- rnorm(10)
    for (t in 11:n) {
      local_rho <- 0.5 + 0.4 * tanh((core_i[max(t - 3, 1)] - core_i[max(t - 13, 1)]) / 5)
      y_planted[t] <- local_rho * y_planted[t - 1] + rnorm(1)
    }
    y_indep <- cumsum(rnorm(n))

    out_planted <- contagion_reg(y_planted, core_i, S = S, d = 3, h = 0.3)
    out_indep <- contagion_reg(y_indep, core_i, S = S, d = 3, h = 0.3)
    planted_range[i] <- diff(range(out_planted$delta2))
    indep_range[i] <- diff(range(out_indep$delta2))
  }
  expect_gt(mean(planted_range), mean(indep_range))
})
