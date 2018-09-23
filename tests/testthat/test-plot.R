context("plot")

withr::with_options(
  c(warn = 2),
  test_that("no problem running (date, lag, wb)", {
    expect_error(autoplot(radf_dta_lag1, wb), regexp = NA)
    expect_error(autoplot(radf_dta_lag1, wb, option = "sadf"))
  })
)

test_that("plot warnings & errors",{
  expect_error(autoplot(radf_div,mc),
    "Cannot reject H0")
})

# Dates -------------------------------------------------------------------

dating <- seq(as.Date("1991/10/01"), by = "month", length.out = 100)
index(radf_dta) <- dating

# Panel -------------------------------------------------------------------
