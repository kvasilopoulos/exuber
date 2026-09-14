context("radf")

test_that("Right output", {
  expect_s3_class(radf_dta, class = "radf_obj")
  nm <- c("adf", "badf", "sadf", "bsadf", "gsadf", "bsadf_panel", "gsadf_panel")
  expect_output(str(radf_dta), "List of 7")
  expect_equal(names(radf_dta), nm)
  expect_output(str(attributes(radf_dta)), "List of 9")
  expect_equal(
    names(attributes(radf_dta)),
    c("names", "mat", "index", "series_names", "minw", "lag", "n", "valid_range", "class")
  )
})

test_that("lag check", {
  expect_error(
    radf(dta, lag = -1), "Argument 'lag' should be a non-negative integer"
  )
  expect_equal(get_lag(radf_dta), 0)
  expect_equal(get_lag(radf_dta_lag1), 1)
})

test_that("minw check radf", {
  msg_minw <- "Argument 'minw' should be a positive integer"
  expect_error(radf(dta, minw = -1), msg_minw)
  expect_error(radf(dta, minw = 0), msg_minw)
  msg <- "Argument 'minw' should be greater than '2'"
  expect_error(radf(dta, minw = 1), msg)
  expect_equal(
    get_minw(radf_dta),
    floor( (0.01 + 1.8 / sqrt(NROW(dta))) * NROW(dta))
  )
})

test_that("class check", {
  expect_error(radf(as.list(dta)), "Unsupported input class")
  expect_error(radf(NULL), "`data` is NULL")
})

test_that("NA handling", {
  # dta_na has a single NA at row 1 (leading position) -- an uneven panel,
  # not an error: radf() drops the panel statistic (with a warning) and
  # NA-pads the affected series' badf/bsadf, but still computes it.
  expect_warning(radf_na <- radf(dta_na), "panel statistic")
  expect_true(all(is.na(radf_na$bsadf_panel)))
  expect_true(is.na(radf_na$gsadf_panel))
  padded_col <- colnames(dta_na)[3]
  expect_true(is.na(radf_na$badf[1, padded_col]))
  expect_false(anyNA(radf_na$badf[, setdiff(colnames(dta_na), padded_col)]))

  # an interior NA (a gap in the middle of a series) is still rejected
  dta_na_interior <- dta
  dta_na_interior[50, 3] <- NA
  expect_error(radf(dta_na_interior), "interior NA")
})

test_that("radf() accepts a named numeric vector (e.g. a prcomp() score column)", {
  y <- setNames(sim_data$psy1, seq_along(sim_data$psy1))
  expect_equal(radf(y)$gsadf, radf(unname(y))$gsadf)
})

# formula-exact check ------------------------------------------------------

# Brute-force recursive ADF t-statistic (intercept, no trend) via lm(), the
# same regression rls_gsadf() solves recursively: for every window [r1, r2]
# regress dy on (1, y_{t-1}, dy_{t-1}, ..., dy_{t-lag}) and take beta's t-value.
adf_t_lm <- function(y, r1, r2, lag) {
  yw <- y[r1:r2]
  dy <- diff(yw)
  ylag <- yw[-length(yw)]
  X <- cbind(1, ylag)
  if (lag > 0) {
    for (j in seq_len(lag)) X <- cbind(X, c(rep(NA, j), dy[seq_len(length(dy) - j)]))
  }
  keep <- (lag + 1):length(dy)
  fit <- lm.fit(X[keep, , drop = FALSE], dy[keep])
  s2 <- sum(fit$residuals^2) / (length(keep) - ncol(X))
  se <- sqrt(s2 * chol2inv(chol(crossprod(X[keep, , drop = FALSE])))[2, 2])
  unname(fit$coefficients[2] / se)
}

test_that("radf() statistics match a brute-force lm() recursion (lag 0 and 1)", {
  set.seed(42)
  y <- cumsum(rnorm(60))
  minw <- 10
  for (lag in 0:1) {
    res <- radf(y, minw = minw, lag = lag)
    ends <- (minw + lag + 1):60
    badf <- vapply(ends, function(r2) adf_t_lm(y, 1, r2, lag), numeric(1))
    bsadf <- vapply(ends, function(r2) {
      max(vapply(1:(r2 - minw - lag), function(r1) adf_t_lm(y, r1, r2, lag), numeric(1)))
    }, numeric(1))
    expect_equal(unname(res$badf[, 1]), badf, tolerance = 1e-8)
    expect_equal(unname(res$bsadf[, 1]), bsadf, tolerance = 1e-8)
    expect_equal(unname(res$adf), adf_t_lm(y, 1, 60, lag), tolerance = 1e-8)
    expect_equal(unname(res$sadf), max(badf), tolerance = 1e-8)
    expect_equal(unname(res$gsadf), max(bsadf), tolerance = 1e-8)
  }
})
