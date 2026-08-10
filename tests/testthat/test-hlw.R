context("radf_hlw")

test_that("hlw_local_to_global maps a window-local breakpoint to the
  correct global i-index and date position", {
  g <- exuber:::hlw_local_to_global(local_tau = 5L, s = 21L)
  expect_equal(g$i_index, 25L)
  expect_equal(g$position, 26L)
})

test_that("radf_hlw runs end to end and returns a well-formed object", {
  skip_on_cran()
  set.seed(11)
  n1 <- 60; n2 <- 25; n3 <- 25; n4 <- 40
  unit1 <- 100 + cumsum(rnorm(n1))
  bubble <- unit1[n1] * 1.05^(1:n2) + cumsum(rnorm(n2))
  target <- bubble[n2] * 0.5
  collapse <- numeric(n3)
  collapse[1] <- bubble[n2] + rnorm(1)
  for (k in 2:n3) collapse[k] <- target + 0.85 * (collapse[k - 1] - target) + rnorm(1)
  recovery <- collapse[n3] + cumsum(rnorm(n4))
  y <- c(unit1, bubble, collapse, recovery)

  out <- radf_hlw(y, trim = 0.1, min_duration = psy_ds(length(y)), nboot = 199, seed = 1)

  expect_s3_class(out, "radf_hlw_obj")
  expect_true("series1" %in% names(out))
  expect_true(is.data.frame(out[["series1"]]))
  expect_output(print(out), "radf_hlw")
})

test_that("radf_hlw's final window, on a single clean bubble episode,
  matches standalone radf_hls() applied to the whole series", {
  skip_on_cran()
  set.seed(11)
  n1 <- 60; n2 <- 25; n3 <- 25; n4 <- 40
  unit1 <- 100 + cumsum(rnorm(n1))
  bubble <- unit1[n1] * 1.05^(1:n2) + cumsum(rnorm(n2))
  target <- bubble[n2] * 0.5
  collapse <- numeric(n3)
  collapse[1] <- bubble[n2] + rnorm(1)
  for (k in 2:n3) collapse[k] <- target + 0.85 * (collapse[k - 1] - target) + rnorm(1)
  recovery <- collapse[n3] + cumsum(rnorm(n4))
  y <- c(unit1, bubble, collapse, recovery)

  hls_out <- radf_hls(y, trim = 0.1)
  hlw_out <- radf_hlw(y, trim = 0.1, min_duration = psy_ds(length(y)), nboot = 199, seed = 1)
  df <- hlw_out[["series1"]]
  last <- df[nrow(df), ]

  expect_equal(unname(hls_out$model[["series1"]]), last$model)
  expect_equal(unname(hls_out$origination[["series1"]]), last$origination)
  expect_equal(unname(hls_out$collapse[["series1"]]), last$collapse)
})

test_that("radf_hlw does not error and returns a zero-row result under a
  pure random-walk null with no bubble at all", {
  skip_on_cran()
  set.seed(2)
  y <- 100 + cumsum(rnorm(150))
  out <- radf_hlw(y, trim = 0.1, nboot = 199, seed = 1)
  expect_equal(nrow(out[["series1"]]), 0L)
})

test_that("radf_hlw recovers two genuine, well-separated bubble episodes
  with accurate dates when it detects exactly two windows", {
  skip_on_cran()
  sim_two_bubbles <- function(seed, n1a = 50, n2a = 20, n3a = 30, n1b = 50, n2b = 20, n3b = 30) {
    set.seed(seed)
    e1 <- 100 + cumsum(rnorm(n1a))
    b1 <- e1[n1a] * 1.05^(1:n2a) + cumsum(rnorm(n2a))
    u1 <- b1[n2a] + cumsum(rnorm(n3a))
    e2 <- u1[n3a] + cumsum(rnorm(n1b))
    b2 <- e2[n1b] * 1.05^(1:n2b) + cumsum(rnorm(n2b))
    u2 <- b2[n2b] + cumsum(rnorm(n3b))
    y <- c(e1, b1, u1, e2, b2, u2)
    list(y = y, true1 = c(n1a, n1a + n2a), true2 = c(n1a + n2a + n3a + n1b, n1a + n2a + n3a + n1b + n2b))
  }
  run_once <- function(seed) {
    sim <- sim_two_bubbles(seed)
    out <- radf_hlw(sim$y, trim = 0.1, min_duration = psy_ds(length(sim$y)), nboot = 199, seed = 1)
    df <- out[["series1"]]
    list(df = df, true1 = sim$true1, true2 = sim$true2)
  }
  res <- lapply(1:8, run_once)
  two_win <- Filter(function(r) nrow(r$df) == 2, res)
  skip_if(length(two_win) == 0, "no 2-window replications in this small sample")

  orig1_bias <- sapply(two_win, function(r) as.numeric(r$df$origination[1]) - r$true1[1])
  orig2_bias <- sapply(two_win, function(r) as.numeric(r$df$origination[2]) - r$true2[1])
  expect_true(mean(abs(orig1_bias)) < 10)
  expect_true(mean(abs(orig2_bias)) < 10)

  ordered <- sapply(two_win, function(r) as.numeric(r$df$origination[1]) < as.numeric(r$df$origination[2]))
  expect_true(all(ordered))
})
