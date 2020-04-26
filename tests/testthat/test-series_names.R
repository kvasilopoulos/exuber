context("series_names")

cnames <- c("psy1", "psy2", "evans", "div", "blan")

test_that("series_names check", {
  expect_equal(cnames, colnames(dta))
  expect_equal(series_names(radf(dta)), cnames)
  expect_equal(series_names(radf(as.ts(dta))), cnames)

  mat_dta <- radf(as.matrix(dta, ncol = 5, dimnames = list(NULL, series_names)))
  ts_dta <- radf(as.ts(dta, optional = TRUE))

  expect_equal(series_names(mat_dta), cnames)
  expect_equal(series_names(ts_dta), cnames)
})

test_that("series_names <-  check ", {
  cnames2 <- c("psy1", "psy2", "ev", "div", "blan")
  expect_equal(series_names(radf_dta) <- cnames2, cnames2)
  # expect_error(
  #   (series_names(radf_dta) <- c("A")),
  #   "length of series_names vectors does not match"
  # )
})
