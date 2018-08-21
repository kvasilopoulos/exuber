context("radf")

test_that("Right output", {

  expect_output(str(radf_dta), "List of 7")
  expect_equal(names(radf_dta), c("adf", "badf", "sadf", "bsadf",
                                  "gsadf", "bsadf_panel", "gsadf_panel" ))
  expect_output(str(attributes(radf_dta)), "List of 6")
  expect_equal(names(attributes(radf_dta)),
               c("names", "index", "lag", "minw", "col_names", "class"))
})

test_that("lag check", {
  expect_error(
    radf(dta, lag = -1), "Argument 'lag' should be a non-negative integer")
  expect_equal(lagr(radf_dta), 0)
  expect_equal(lagr(radf_dta_lag1), 1)
})

test_that("minw check radf", {
  expect_error(
    radf(dta, minw = -1), "Argument 'minw' should be a positive integer")
  expect_error(
    radf(dta, minw = 0), "Argument 'minw' should be a positive integer")
  expect_error(radf(dta, minw = 2), "Argument 'minw' is too small")
  expect_equal(minw(radf_dta),
               floor((0.01 + 1.8 / sqrt(NROW(dta))) * NROW(dta)))
})

test_that("col_names check", {
  series_names <- colnames(dta)

  expect_equal(col_names(radf(dta)), series_names)
  expect_equal(col_names(radf(as.ts(dta))), series_names)
  expect_equal(col_names(radf(as.matrix(dta,
    ncol = 5, dimnames = list(NULL, series_names)))), series_names)
  expect_equal(
    col_names(radf(as.data.frame(dta, optional = TRUE))), series_names)
})

test_that("col_names <-  check ", {
  series_index_names <- col_names(radf_dta) <- c("dgp1", "dgp2", "evans",
                                                 "dividends", "blanchard")
  expect_equal(col_names(radf_dta), series_index_names)
  expect_error(
    (col_names(radf_dta) <- c("A")),
    "length of col_names vectors does not match"
  )
})


test_that("index works in different classes", {
  dating_y <- seq(as.Date("1997/01/01"), by = "year", length.out = 100)
  dating_q <- seq(as.Date("1997/01/01"), by = "quarter", length.out = 100)
  dating_m <- seq(as.Date("1997/01/01"), by = "month", length.out = 100)
  dating_w <- seq(as.Date("1997/01/01"), by = "week", length.out = 100)
  dating_d <- seq(as.Date("1997/01/01"), by = "day", length.out = 100)
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
  # parse dates from rows with findDates function
  df_row <- data.frame(dta)
  rownames(df_row) <- dating_m
  expect_equal(index(radf(df_row)), dating_m)
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
  # parse index from matrix
  mat1 <- matrix(dta, ncol = 5)
  expect_equal(index(radf(mat1)), seq(1, NROW(mat1)))
  expect_error(index(radf(mat1)) <- seq(1, NROW(mat1) - 1),
    "length of index vectors does not match")
})

test_that("class check", {
  expect_error(radf(as.list(dta)), "Unsupported class")
})

test_that("NA handling", {
  expect_error(
    radf(dta_na), "Recursive least square estimation cannot handle NA")
})
