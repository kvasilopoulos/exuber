context("deprecated")

test_that("deprecated functions", {
  expect_warning(report(radf_dta, mc), "'report' is deprecated")
  expect_error(plot(radf_dta, y = mc), "'plot.radf' is defunct.", class = "error")
  expect_error(plot(radf_dta, mc, plot_type = "single"),
                 "'plot.radf' is defunct", class = "error")
})
