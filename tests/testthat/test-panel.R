context("panel")


test_that(": different lag", {
  msg <- "Different lag values"
  expect_error(summary(radf_dta, sb1), msg)
  expect_error(diagnostics(radf_dta, sb1), msg)
  expect_error(datestamp(radf_dta, sb1), msg)
  expect_error(autoplot(radf_dta, sb1), msg)
})

test_that("lag 1 or more",{
  sb1 <- sb_cv(dta, lag = 1, nboot = 20)
  summary(radf_dta_lag1, sb1)
  expect_error(summary(radf_dta, sb1), "Different lag values")
  expect_error(summary(radf_dta_lag1, sb), "Different lag values")

  fortify(radf_dta_lag1, cv = sb1)

  # fix index when sb lag = 1
})
