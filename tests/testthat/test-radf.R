context("radf")

test_that("Right output", {
  expect_s3_class(radf_dta, class = "radf_obj")
  nm <- c("adf", "badf", "sadf", "bsadf", "gsadf", "bsadf_panel", "gsadf_panel")
  expect_output(str(radf_dta), "List of 7")
  expect_equal(names(radf_dta), nm)
  expect_output(str(attributes(radf_dta)), "List of 7")
  expect_equal(
    names(attributes(radf_dta)),
    c("names", "index", "lag", "n", "minw", "series_names", "class")
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
  expect_error(radf(as.list(dta)), "unsupported class")
})

test_that("NA handling", {
  expect_error(radf(dta_na), "rls estimation cannot handle NA")
})
