context("index")

dating_y <- seq(as.Date("1997/01/01"), by = "year", length.out = 100)
dating_q <- seq(as.Date("1997/01/01"), by = "quarter", length.out = 100)
dating_m <- seq(as.Date("1997/01/01"), by = "month", length.out = 100)
dating_w <- seq(as.Date("1997/01/01"), by = "week", length.out = 100)
dating_d <- seq(as.Date("1997/01/01"), by = "day", length.out = 100)

test_that("data.frame", {
  # No date index
  expect_equal(index(dta), seq(from = 1, to = NROW(dta)))

  # parse dates from data.frame
  df_y <- data.frame(dating_y, dta)
  expect_equal(index(radf(df_y)), dating_y)
  df_q <- data.frame(dating_q, dta)
  expect_equal(index(radf(df_q)), dating_q)
  df_m <- data.frame(dating_m, dta)
  expect_equal(index(radf(df_m)), dating_m)
  df_w <- data.frame(dating_w, dta)
  expect_equal(index(radf(df_w)), dating_w)
  df_d <- data.frame(dating_d, dta)
  expect_equal(index(radf(df_d)), dating_d)

  # extract index from data.frame
  expect_equal(index(df_y), dating_y)
  expect_equal(index(df_m), dating_m)
  expect_equal(index(df_w), dating_w)
})

test_that("ts", {
  # parse dates from ts objects
  ts_y <- ts(dta, frequency = 1, start = c(1997))
  expect_equal(index(radf(ts_y)), dating_y)
  ts_q <- ts(dta, frequency = 4, start = c(1997, 1))
  expect_equal(index(radf(ts_q)), dating_q)
  ts_m <- ts(dta, frequency = 12, start = c(1997, 1))
  expect_equal(index(radf(ts_m)), dating_m)
  expect_error(index(ts_m), regexp = NA)

  # weeks are more problematic because simulation creates 53 weeks in a year
  ts_w <- ts(dta, frequency = 52, start = c(1997, 1))
  expect_equal(index(radf(ts_w)), dating_w, tolerance = 1e-2)
  ts_d <- ts(dta, frequency = 365, start = c(1997, 1))
  expect_equal(index(radf(ts_d)), dating_d)
})


test_that("matrix", {
  # parse index from matrix
  mat1 <- as.matrix(dta, ncol = 5)
  rmat1 <- radf(mat1)
  expect_equal(index(rmat1), seq(from = 1, to = NROW(mat1)))
  expect_error(
    index(rmat1) <- seq(from = 1, to = NROW(mat1) - 1),
    "length of index vectors does not match"
  )
})

test_that("datestamp", {
  expect_equal(radf_dta %>% datestamp(cv = mc)
               %>% index(trunc = T), index(radf_dta, trunc = T))
})

