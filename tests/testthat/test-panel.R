context("panel")

test_that(": robust lag specification", {

  msg <- "lag value does not match"
  expect_error(summary(radf_dta, sb1), msg)
  expect_error(diagnostics(radf_dta, sb1), msg)
  expect_error(datestamp(radf_dta, sb1), msg)
  expect_error(autoplot(radf_dta, sb1), msg)
  expect_error(summary(radf_dta, sb1), msg)
  expect_error(summary(radf_dta_lag1, sb), msg)
})
