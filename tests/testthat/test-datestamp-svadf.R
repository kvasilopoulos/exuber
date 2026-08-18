context("datestamp option = 'svadf'")

test_that("svadf_threshold matches the paper's own closed-form formulas
  exactly", {
  expect_equal(exuber:::svadf_threshold(100, "origination"), log(100) / 10)
  expect_equal(exuber:::svadf_threshold(100, "collapse"), log(100) / 2)
})

test_that("datestamp(option = 'svadf') runs end to end and returns a
  well-formed ds_radf object", {
  # a plain random walk rarely triggers a detection (see the false-alarm
  # test below) -- use a genuine bubble+collapse DGP so this test reliably
  # exercises the non-empty, printed path.
  set.seed(100)
  n1 <- 60
  y <- 100 + cumsum(rnorm(n1))
  n2 <- 40
  bubble <- y[n1] * 1.04^(1:n2) + cumsum(rnorm(n2, sd = 1))
  n3 <- 40
  collapse <- bubble[n2] - cumsum(abs(rnorm(n3, mean = 3, sd = 1)))
  yy <- c(y, bubble, collapse)

  r <- radf(yy, lag = 0)
  out <- datestamp(r, option = "svadf", min_duration = psy_ds(length(yy)))

  expect_s3_class(out, "ds_radf")
  expect_equal(attr(out, "option"), "svadf")
  expect_match(attr(out, "method"), "SV-ADF")
  expect_gt(length(out), 0)
  expect_output(print(out), "SV-ADF")
  for (nm in names(out)) {
    expect_setequal(colnames(out[[nm]]), c("Start", "Peak", "End", "Duration", "Signal", "Ongoing"))
  }
})

test_that("datestamp(option = 'svadf') never dates a collapse before the
  origination date", {
  skip_on_cran()
  set.seed(3)
  for (i in 1:20) {
    yy <- cumsum(rnorm(150))
    r <- radf(yy, lag = 0)
    out <- datestamp(r, option = "svadf", min_duration = psy_ds(150))
    for (nm in names(out)) {
      expect_true(all(out[[nm]]$End > out[[nm]]$Start))
    }
  }
})

test_that("datestamp(option = 'svadf') detects a genuine bubble+collapse
  episode with reasonable dating accuracy", {
  skip_on_cran()
  set.seed(100)
  n1 <- 60
  y <- 100 + cumsum(rnorm(n1))
  n2 <- 40
  bubble <- y[n1] * 1.04^(1:n2) + cumsum(rnorm(n2, sd = 1))
  n3 <- 40
  collapse <- bubble[n2] - cumsum(abs(rnorm(n3, mean = 3, sd = 1)))
  yy <- c(y, bubble, collapse)

  r <- radf(yy, lag = 0)
  out <- datestamp(r, option = "svadf", min_duration = psy_ds(length(yy)))
  expect_length(out, 1)
  expect_lt(abs(out[[1]]$Start - n1), 20)
})

test_that("datestamp(option = 'svadf') has a low false-alarm rate under H0
  (pure random walk, no bubble)", {
  skip_on_cran()
  set.seed(5)
  nrep <- 60
  fa <- mean(vapply(seq_len(nrep), function(i) {
    set.seed(1000 + i)
    yy <- cumsum(rnorm(150))
    r <- radf(yy, lag = 0)
    out <- datestamp(r, option = "svadf", min_duration = psy_ds(150))
    length(out) > 0
  }, logical(1)))
  expect_lt(fa, 0.20)
})
