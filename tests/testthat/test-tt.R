context("radf_tt")

test_that("gls_dfstat_grid matches a brute-force no-intercept lm() fit", {
  set.seed(7)
  y <- cumsum(rnorm(40))
  minw <- 10
  res <- exuber:::gls_dfstat_grid(y, minw)

  yc <- y - y[1]
  n1 <- length(yc) - 1L
  dy <- diff(yc)
  ylag <- yc[1:n1]

  b_idx <- minw:n1
  badf_brute <- vapply(b_idx, function(b) {
    fit <- lm(dy[1:b] ~ ylag[1:b] - 1)
    summary(fit)$coefficients[1, "t value"]
  }, numeric(1))

  expect_equal(res$badf, badf_brute, tolerance = 1e-8)
  expect_equal(res$adf, badf_brute[length(badf_brute)], tolerance = 1e-8)
  expect_equal(res$sadf, max(badf_brute), tolerance = 1e-8)
})

test_that("radf_tt runs on the package's sim_data and returns finite stats", {
  res <- radf_tt(dta)
  expect_s3_class(res, "radf_tt_obj")
  expect_true(all(is.finite(res$adf)))
  expect_true(all(is.finite(res$sadf)))
  expect_true(all(is.finite(res$gsadf)))
  expect_true(all(res$sadf <= res$gsadf + 1e-8))
})

test_that("radf_tt_cv computes badf_cv/bsadf_cv with the right shape and a hard identity", {
  set.seed(1)
  n <- 100
  minw <- 20
  cv <- radf_tt_cv(n = n, minw = minw, nrep = 300, seed = 1)

  n_minw <- n - minw
  expect_equal(dim(cv$badf_cv), c(n_minw, 3L))
  expect_equal(dim(cv$bsadf_cv), c(n_minw, 3L))
  expect_equal(colnames(cv$badf_cv), c("90%", "95%", "99%"))
  expect_equal(colnames(cv$bsadf_cv), c("90%", "95%", "99%"))

  # badf's last point IS adf by construction (gls_dfstat_grid(): adf <-
  # badf[length(badf)]), per replicate -- so their quantiles across
  # replicates must match exactly, not just approximately.
  expect_equal(
    unname(cv$badf_cv[n_minw, ]),
    as.vector(cv$adf_cv)
  )
})

test_that("radf_tt's full Analysis/Tidying/Plotting pipeline works, not just summary()/tidy()", {
  skip_on_cran()
  res <- radf_tt(sim_data, minw = 20)
  cv <- radf_tt_cv(n = 100, minw = 20, nrep = 300, seed = 1)

  expect_no_error(summary(res, cv = cv))
  expect_no_error(tidy(res, cv = cv))
  expect_no_error(datestamp(res, cv = cv))
  expect_no_error(datestamp(res, cv = cv, option = "sadf")) # exercises badf_cv specifically
  expect_no_error(autoplot(res, cv = cv))
})

test_that("radf_tt_cv's asymptotic STADF critical values match Whitehouse (2019)
  as reported in Kurozumi, Skrobotov & Tsarev (2024), footnote 4: for r0 = 0.1,
  (10%, 5%, 1%) = (2.319, 2.626, 3.223). This is a Monte Carlo approximation to
  a T -> Inf limit, so an exact match isn't expected -- tolerance is set from
  the observed MC/finite-T error at this nrep/n, not chosen to force a pass.", {
  skip_on_cran()

  set.seed(20260808)
  n <- 300
  minw <- round(0.1 * n)
  results <- replicate(1500, exuber:::gls_dfstat_grid(cumsum(rnorm(n)), minw), simplify = FALSE)
  sadf <- vapply(results, `[[`, numeric(1), "sadf")
  sadf_cv <- quantile(sadf, probs = c(0.9, 0.95, 0.99))

  published <- c(2.319, 2.626, 3.223)
  expect_equal(unname(sadf_cv), published, tolerance = 0.15)
})
