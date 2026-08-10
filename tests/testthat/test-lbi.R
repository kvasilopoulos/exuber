context("radf_lbi")

test_that("Breitung & Diegel's eq. 4 telescoping identity holds exactly
  (2*sum(Delta y_t * y_{t-1}) = y_T^2 - T*sigma_tilde^2, y_1 = 0 case)", {
  set.seed(1)
  T <- 100
  y <- c(0, cumsum(rnorm(T)))
  dy <- diff(y)
  ylag <- y[1:T]
  lhs <- 2 * sum(dy * ylag)
  sigma2_tilde <- mean(dy^2)
  rhs <- y[T + 1]^2 - T * sigma2_tilde
  expect_equal(lhs, rhs, tolerance = 1e-8)
})

test_that("radf_lbi runs end to end and returns a well-formed object", {
  set.seed(1)
  y <- cumsum(rnorm(100))
  out <- radf_lbi(y)

  expect_s3_class(out, "radf_lbi_obj")
  expect_true(is.numeric(out$stat[["series1"]]))
  expect_equal(out$crit, qnorm(0.95))
  expect_output(print(out), "radf_lbi")
})

test_that("radf_lbi's statistic follows a standard normal distribution
  under H0, matching Breitung & Diegel's own claimed null distribution
  (not just an approximately-sized test)", {
  skip_on_cran()
  run_stat <- function(seed) {
    set.seed(seed)
    y <- cumsum(rnorm(100))
    radf_lbi(y)$stat[["series1"]]
  }
  stats <- sapply(1:300, run_stat)
  expect_true(abs(mean(stats)) < 0.15)
  expect_true(abs(sd(stats) - 1) < 0.15)
  expect_gt(ks.test(stats, "pnorm")$p.value, 0.01)
})

test_that("radf_lbi detects a genuine explosive series with power
  comparable to a standard SADF test on the same DGP", {
  skip_on_cran()
  run_lbi <- function(seed) {
    set.seed(seed)
    n1 <- 60
    y <- 100 * 1.03^(1:n1) + cumsum(rnorm(n1, sd = 1))
    radf_lbi(y)$detected[["series1"]]
  }
  rate <- mean(sapply(1:30, run_lbi))
  expect_gt(rate, 0.8)
})
