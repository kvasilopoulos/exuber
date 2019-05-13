context("col_names")

cnames <- c("psy1", "psy2", "evans", "div", "blan")

test_that("col_names check", {
  expect_equal(cnames, colnames(dta))
  expect_equal(cnames, col_names(dta))
  expect_equal(col_names(radf(dta)), cnames)
  expect_equal(col_names(radf(as.ts(dta))), cnames)

  mat_dta <- radf(as.matrix(dta, ncol = 5, dimnames = list(NULL, series_names)))
  ts_dta <- radf(as.ts(dta, optional = TRUE))

  expect_equal(col_names(mat_dta), cnames)
  expect_equal(col_names(ts_dta), cnames)
})

test_that("col_names <-  check ", {
  cnames2 <- c("psy1", "psy2", "ev", "div", "blan")
  expect_equal(col_names(radf_dta) <- cnames2, cnames2)
  expect_error(
    (col_names(radf_dta) <- c("A")),
    "length of col_names vectors does not match"
  )
})
