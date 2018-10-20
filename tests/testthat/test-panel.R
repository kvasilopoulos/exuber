context("panel")


test_that(": different lag", {
  msg <- "Different lag values"
  expect_error(summary(radf_dta, sb1), msg)
  expect_error(diagnostics(radf_dta, sb1), msg)
  expect_error(datestamp(radf_dta, sb1), msg)
  expect_error(autoplot(radf_dta, sb1), msg)
})



test_that("assert panel",{

})

