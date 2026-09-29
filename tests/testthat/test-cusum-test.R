context("cusum_test")

brute_cusum <- function(y, type) {
  d <- diff(y)
  nd <- length(d)
  sig <- sqrt(mean(d^2))
  sig_eta <- sqrt(mean(d^4) - mean(d^2)^2)
  win <- function(j, k) { # increments j+1..k
    dd <- d[(j + 1):k]
    if (type %in% c("cs", "gcs")) {
      sum(dd) / (sig * sqrt(nd))
    } else {
      (sum(dd^2) - (k - j) / nd * sum(d^2)) / (sig_eta * sqrt(nd))
    }
  }
  starts <- function(k) if (type %in% c("cs", "cssq")) 0 else 0:(k - 1)
  vals <- lapply(1:nd, function(k) vapply(starts(k), function(j) win(j, k), 0))
  list(sup = max(unlist(vals)), inf = min(unlist(vals)))
}

test_that("cusum_test's running max/min matches a brute-force double loop
  over every window, for all four statistics", {
  set.seed(7)
  y <- cumsum(rnorm(60)) + c(rep(0, 40), cumsum(1.1^(1:20)))
  for (type in c("cs", "gcs", "cssq", "gcssq")) {
    out <- cusum_test(y, type = type)
    b <- brute_cusum(y, type)
    expect_equal(unname(out$sup), b$sup, tolerance = 1e-10, info = type)
    if (type %in% c("cssq", "gcssq")) expect_equal(unname(out$inf), b$inf, tolerance = 1e-10, info = type)
  }
})

test_that("cusum_test uses Table I's critical values and the two-sided
  rule for CUSUM-SQ", {
  set.seed(8)
  y <- cumsum(rnorm(100))
  expect_equal(cusum_test(y, type = "cs")$crit, 1.93)
  expect_equal(cusum_test(y, type = "gcs", sig_lvl = 99)$crit, 2.78)
  sq <- cusum_test(y, type = "cssq", sig_lvl = 90)
  expect_equal(unname(sq$crit), c(1.19, -1.21))
  expect_equal(unname(sq$detected), unname(sq$sup >= 1.19 | sq$inf <= -1.21))
  expect_error(cusum_test(y, sig_lvl = 80))
  expect_output(print(sq), "CSSQ")
})

test_that("cusum_test's false-alarm rates under H0 are near nominal", {
  skip_on_cran()
  fa <- vapply(c("cs", "gcs", "cssq", "gcssq"), function(type) {
    mean(vapply(1:300, function(i) {
      set.seed(i)
      unname(cusum_test(cumsum(rnorm(200)), type = type)$detected)
    }, logical(1)))
  }, 0)
  expect_true(all(fa < 0.10))
})
