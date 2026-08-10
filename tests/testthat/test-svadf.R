context("radf_svadf")

test_that("svadf_threshold matches the paper's own closed-form formulas
  exactly", {
  expect_equal(exuber:::svadf_threshold(100, "origination"), log(100) / 10)
  expect_equal(exuber:::svadf_threshold(100, "collapse"), log(100) / 2)
})

test_that("radf_svadf reuses radf()'s own badf sequence bit-for-bit
  (SV-ADF's point statistic is exactly the existing recursive ADF
  t-statistic, per the paper's own proof appendix)", {
  set.seed(1)
  y <- cumsum(rnorm(150))
  out <- radf_svadf(y)
  r <- radf(y, minw = attr(out, "minw"), lag = 0)
  expect_equal(out$badf[, 1], r$badf[, 1])
})

test_that("radf_svadf runs end to end and returns a well-formed object", {
  set.seed(1)
  y <- cumsum(rnorm(150))
  out <- radf_svadf(y)

  expect_s3_class(out, "radf_svadf_obj")
  expect_true(is.matrix(out$badf))
  expect_length(out$origination_threshold, nrow(out$badf))
  expect_length(out$collapse_threshold, nrow(out$badf))
  expect_output(print(out), "radf_svadf")
})

test_that("radf_svadf never dates a collapse before the origination date", {
  skip_on_cran()
  set.seed(3)
  ok <- TRUE
  for (i in 1:20) {
    yy <- cumsum(rnorm(150))
    oo <- radf_svadf(yy)
    if (!is.na(oo$origination) && !is.na(oo$collapse) && oo$collapse <= oo$origination) {
      ok <- FALSE
    }
  }
  expect_true(ok)
})

test_that("radf_svadf detects a genuine bubble+collapse episode with
  reasonable dating accuracy", {
  skip_on_cran()
  set.seed(100)
  n1 <- 60
  y <- 100 + cumsum(rnorm(n1))
  n2 <- 40
  bubble <- y[n1] * 1.04^(1:n2) + cumsum(rnorm(n2, sd = 1))
  n3 <- 40
  collapse <- bubble[n2] - cumsum(abs(rnorm(n3, mean = 3, sd = 1)))
  yy <- c(y, bubble, collapse)

  out <- radf_svadf(yy)
  expect_false(is.na(out$origination))
  expect_lt(abs(out$origination - n1), 20)
})

test_that("radf_svadf's default min_duration matches psy_ds()", {
  set.seed(1)
  y <- cumsum(rnorm(150))
  out <- radf_svadf(y)
  expect_equal(attr(out, "min_duration"), psy_ds(150))
})

test_that("radf_svadf has a low false-alarm rate under H0 (pure random
  walk, no bubble)", {
  skip_on_cran()
  set.seed(5)
  nrep <- 60
  fa <- mean(vapply(seq_len(nrep), function(i) {
    set.seed(1000 + i)
    yy <- cumsum(rnorm(150))
    !is.na(radf_svadf(yy)$origination)
  }, logical(1)))
  expect_lt(fa, 0.20)
})
