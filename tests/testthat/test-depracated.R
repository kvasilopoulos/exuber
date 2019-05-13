context("depracated")

test_that("depracated functions", {
  expect_warning(report(radf_dta, mc), "'report' is deprecated")
  expect_warning(plot(radf_dta, mc), "'plot.radf' is deprecated")
  expect_warning(plot(radf_dta, mc, plot_type = "single"),
                 "'plot.radf' is deprecated")
})
