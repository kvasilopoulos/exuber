context("radf")

test_that("Right output", {
  expect_output(str(radf_dta), "List of 5")
  expect_output(str(attributes(radf_dta)), "List of 6")
})

test_that("lag check", {
  expect_error(
    radf(dta, lag = -1),
    "Argument 'lag' should be a non-negative integer"
  )
  expect_equal(lagr(radf_dta), 0)
  expect_equal(lagr(radf_dta_lag1), 1)
})

test_that("minw check radf", {
  expect_error(
    radf(dta, minw = -1),
    "Argument 'minw' should be a positive integer"
  )
  expect_error(
    radf(dta, minw = 0),
    "Argument 'minw' should be a positive integer"
  )
  expect_error(
    radf(dta, minw = 2),
    "Argument 'minw' is too small"
  )
})

test_that("colnames check", {
  series_names <- colnames(dta)
  expect_equal(col_names(radf(dta)), series_names)
  expect_equal(col_names(radf(as.ts(dta))), series_names)
  expect_equal(col_names(radf(as.matrix(
    dta,
    ncol = 5, dimnames = list(NULL, series_names)
  ))), series_names)
  expect_equal(
    col_names(radf(as.data.frame(dta, optional = TRUE))),
    series_names
  )
})

test_that("col_names <-  check ", {
  series_index_names <- col_names(radf_dta) <- c(
    "dgp1", "dgp2", "evans",
    "dividends", "blanchard"
  )
  expect_equal(col_names(radf_dta), series_index_names)
  expect_error(
    (col_names(radf_dta) <- c("A")),
    "length of col_names vectors does not match"
  )
})


test_that("index works in different classes", {
  dating <- seq(as.Date("1991/10/01"), as.Date("2000/01/01"), by = "month")
  df1 <- data.frame(dating, dta)
  expect_equal(index(radf(df1)), dating)
  df2 <- data.frame(dta)
  rownames(df2) <- dating
  expect_equal(index(radf(df2)), dating)
  ts1 <- ts(dta, frequency = 12, start = c(1991, 1))
  expect_error(invisible(index(ts1)), regexp = NA)
  expect_equal(index(radf(ts1)), index(ts1))
  mat1 <- matrix(dta, ncol = 5)
  # expect_error(invisible(index(mat1)), regexp = NA)
  # expect_error(index(mat1) <- seq(1, NROW(mat1)), regexp = NA)
  expect_equal(index(radf(mat1)), seq(1, NROW(mat1)))
  expect_error(
    index(radf(mat1)) <- seq(1, NROW(mat1) - 1),
    "length of index vectors does not match"
  )
})

test_that("error generation", {
  dta_na <- dta
  dta_na[1, 3] <- NA
  expect_error(
    radf(dta_na),
    "Recursive least square estimation cannot handle NA"
  )
  expect_error(
    wb_cv(dta_na),
    "Recursive least square estimation cannot handle NA"
  )
  expect_error(radf(as.list(dta)), "Unsupported class")
})
