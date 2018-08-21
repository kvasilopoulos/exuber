
withr::with_options(
  c(warn = 2),
  test_that("no problem running (date, lag, wb)", {
    expect_error(plot(radf_dta_lag1, wb), regexp = NA)
    expect_error(plot(radf_dta_lag1, wb, option = "sadf"),
      message("Explosive periods with Wild Bootstraped critical",
              "values apply only for the option 'gsadf'"))
    expect_error(plot(radf_dta_lag1, wb, plot_type = "single"), regexp = NA)
    expect_error(plot(radf_dta_lag1, wb, option = "sadf", plot_type = "single"),
      message("Explosive periods with Wild Bootstraped critical",
              "values apply only for the option 'gsadf'"))
  })
)


test_that("plot warnings & errors",{
  expect_error(plot(radf_div,mc),
    "Cannot reject H0, do not proceed for date stamping or plotting")
  expect_warning(plot(radf_dta, mc, plot_type = "single", breaks_y = 1),
    "Argument 'breaks_y' is redundant when 'plot_type' is set to 'single'")
  expect_warning(plot(radf(dta[,1]), mc, plot_type = "single"),
    "Argument 'plot_type' should be set to 'multiple' ",
    "when there is only one series to plot")
})

test_that("further arguements to multiple plot",{
  expect_error(plot(radf_dta, mc, breaks_x = 10), regexp = NA)
  expect_error(plot(radf_dta, mc, breaks_x = 10, plot_type = "multiple"), NA)
  expect_error(plot(radf_dta, mc, breaks_y = 2), regexp = NA)
  expect_error(plot(radf_dta, mc, plot_type = "single", breaks_x = 10), NA)
})


# Dates -------------------------------------------------------------------

dating <- seq(as.Date("1991/10/01"), by = "month", length.out = 100)
index(radf_dta) <- dating

test_that("further arguements to multiple plot with dates",{
  expect_error(plot(radf_dta, mc, breaks_x = "1 year", format_date = "%Y"), NA)
  expect_error(plot(radf_dta, mc, format_date = "%Y"), regexp = NA)
  expect_error(plot(radf_dta, mc, breaks_x = "1 year"), regexp = NA)
})

test_that("further arguements to single plot with dates",{
  expect_error(plot(radf_dta, mc, plot_type = "single",
                    breaks_x = "1 year", format_date = "%Y"), regexp = NA)
  expect_error(plot(radf_dta, mc, plot_type = "single",
                    format_date = "%Y"), regexp = NA)
  expect_error(plot(radf_dta, mc, plot_type = "single",
                    breaks_x = "1 year"), regexp = NA)
  expect_error(plot(radf(dta[, 1]), mc), regexp = NA)
})


# Panel -------------------------------------------------------------------

test_that("",{

})
