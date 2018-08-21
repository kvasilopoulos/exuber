context("report")

test_that("printing coverage", {
  expect_error(capture.output(report(radf_dta, mc)), regexp = NA)
  expect_error(capture.output(diagnostics(radf_dta, mc)), regexp = NA)
  # Panel
  expect_error(capture.output(report(radf_dta, mc, panel = TRUE)), NA)
  expect_error(capture.output(diagnostics(radf_dta, mc, panel = TRUE)), NA)
})

test_that("class checks", {
  expect_error(diagnostics(dta, mc), "Argument 'x' should be of class 'radf'")
  expect_error(report(dta, mc), "Argument 'x' should be of class 'radf'")
  expect_error(datestamp(dta, mc), "Argument 'x' should be of class 'radf'")
  expect_error(
    diagnostics(radf_dta, dta),"Argument 'y' should be of class 'cv'")
  expect_error(report(radf_dta, dta), "Argument 'y' should be of class 'cv'")
  expect_error(datestamp(radf_dta, dta),"Argument 'y' should be of class 'cv'")
})

test_that("error diagnostics", {
  expect_error(diagnostics(radf_div, mc),
    "Cannot reject H0, do not proceed for date stamping or plotting")
  expect_error(diagnostics(radf_95, mc),
    "You cannot reject H0 for significance level 95%")
})

test_that("different minw", {
  expect_error(report(radf_dta, mc2),
    message("The critical values should have the same",
            "minumum window with the t-statistics!"))
  expect_error(diagnostics(radf_dta, mc2),
    message("The critical values should have the same", "
               minumum window with the t-statistics!"))
  expect_error(datestamp(radf_dta, mc2),
    message("The critical values should have the same", "
               minumum window with the t-statistics!"))
})

test_that("panel: different lag", {
  expect_error(report(radf_dta, sb1, panel = TRUE), "Different lag values")
  expect_error(diagnostics(radf_dta, sb1, panel = TRUE), "Different lag values")
  expect_error(datestamp(radf_dta, sb1, panel = TRUE), "Different lag values")
  expect_error(plot(radf_dta, sb1, panel = TRUE), "Different lag values")
})


test_that("panel errors: sb in ind estimation",{
  expect_error(report(radf_dta, sb),
  "Sieve Bootstrapped critical values are used for panel estimation")
  expect_error(diagnostics(radf_dta, sb),
  "Sieve Bootstrapped critical values are used for panel estimation")
  expect_error(datestamp(radf_dta, sb),
  "Sieve Bootstrapped critical values are used for panel estimation")
  expect_error(plot(radf_dta, sb),
  "Sieve Bootstrapped critical values are used for panel estimation")
})


test_that("assert panel",{
  expect_error(report(radf_dta, wb, panel = TRUE), "Wrong critical values")
  expect_error(datestamp(radf_dta, wb, panel = TRUE), "Wrong critical values")
  expect_error(report(radf_dta, mc, panel = TRUE), regexp = NA)
  expect_error(datestamp(radf_dta, mc, panel = TRUE), regexp = NA)
  expect_error(report(radf_dta, sb, panel = TRUE), regexp = NA)
  expect_error(datestamp(radf_dta, sb, panel = TRUE), regexp = NA)
})


test_that("Correct output in report/datestamp", {
  expect_output(str(report(radf_dta, mc)), "List of 5")
  expect_output(str(datestamp(radf_dta, mc)), "List of 4") # do not pass diagn
  expect_output(str(report(radf_dta, wb)), "List of 5")
  expect_output(str(datestamp(radf_dta, wb)), "List of 4")
})

withr::with_options(
  c(warn = 2),
  test_that("no problem running rp/ds/dg (mc)", {
    expect_error(report(radf_dta, mc), regexp = NA)
    expect_error(diagnostics(radf_dta, mc), regexp = NA)
    expect_error(diagnostics(radf_dta, mc, option = "sadf"), regexp = NA)
    expect_error(datestamp(radf_dta, mc), regexp = NA)
    expect_error(datestamp(radf_dta, mc, min_duration = 50),
                 "Argument 'min_duration' excludes all the explosive periods")
    expect_error(datestamp(radf_dta, mc, option = "sadf"), regexp = NA)
    expect_error(plot(radf_dta, mc), regexp = NA)
    expect_error(plot(radf_dta, mc, option = "sadf"), regexp = NA)
    expect_error(plot(radf_dta, mc, plot_type = "single"), regexp = NA)
  })
)

withr::with_options(
  c(warn = 2),
  test_that("no problem running report (lag, mc)", {
    expect_error(report(radf_dta_lag1, mc), regexp = NA)
    expect_error(diagnostics(radf_dta_lag1, mc), regexp = NA)
    expect_error(diagnostics(radf_dta_lag1, mc, option = "sadf"), regexp = NA)
    expect_error(datestamp(radf_dta_lag1, mc), regexp = NA)
    expect_error(datestamp(radf_dta_lag1, mc, option = "sadf"), regexp = NA)
    expect_error(plot(radf_dta_lag1, mc), regexp = NA)
    expect_error(plot(radf_dta_lag1, mc, option = "sadf"), regexp = NA)
  })
)

withr::with_options(
  c(warn = 2),
  test_that("no problem running report (wb)", {
    expect_error(report(radf_dta, wb), regexp = NA)
    expect_error(diagnostics(radf_dta, wb), regexp = NA)
    expect_error(
      diagnostics(radf_dta, wb, option = "sadf"),
      message(
        " Explosive periods with Wild Bootstraped critical ",
        "values apply only for the option 'gsadf'"
      )
    )
    expect_error(datestamp(radf_dta, wb), regexp = NA)
    expect_error(
      datestamp(radf_dta, wb, option = "sadf"),
      message(
        "Explosive periods with Wild Bootstraped critical ",
        "values apply only for the option 'gsadf'"))
    expect_error(plot(radf_dta, wb), regexp = NA)
    expect_error(plot(radf_dta, wb, option = "sadf"),
      message("Explosive periods with Wild Bootstraped critical ",
              "values apply only for the option 'gsadf'"))
    expect_error(plot(radf_dta, wb, plot_type = "single"), regexp = NA)
  })
)

withr::with_options(
  c(warn = 2),
  test_that("no problem running report (lag,wb)", {
    expect_error(report(radf_dta_lag1, wb), regexp = NA)
    expect_error(diagnostics(radf_dta_lag1, wb), regexp = NA)
    expect_error(diagnostics(radf_dta_lag1, wb, option = "sadf"),
      message("Explosive periods with Wild Bootstraped critical ",
              "values apply only for the option 'gsadf'"))
    expect_error(datestamp(radf_dta_lag1, wb), regexp = NA)
    expect_error(datestamp(radf_dta_lag1, wb, option = "sadf"),
      message("Explosive periods with Wild Bootstraped critical ",
              "values apply only for the option 'gsadf'"))
    expect_error(plot(radf_dta_lag1, wb), regexp = NA)
    expect_error(plot(radf_dta_lag1, wb, option = "sadf"),
      message("Explosive periods with Wild Bootstraped critical ",
              "values apply only for the option 'gsadf'"))
    expect_error(plot(radf_dta_lag1, wb, plot_type = "single"), regexp = NA)
    expect_error(plot(radf_dta_lag1, wb, option = "sadf", plot_type = "single"),
      message("Explosive periods with Wild Bootstraped critical",
              "values apply only for the option 'gsadf'"))
  })
)

index(radf_dta) <- seq(as.Date("1991/10/01"), by = "month", length.out = 100)

withr::with_options(
  c(warn = 2),
  test_that("no problem running with (date, mc)", {
    expect_error(datestamp(radf_dta, mc), regexp = NA)
    expect_error(datestamp(radf_dta, mc, option = "sadf"), regexp = NA)
    expect_error(plot(radf_dta, mc), regexp = NA)
    expect_error(plot(radf_dta, mc, option = "sadf"), regexp = NA)
    expect_error(plot(radf_dta, mc, plot_type = "single"), regexp = NA)
    expect_error(plot(radf_dta, mc, option = "sadf", plot_type = "single"),
      regexp = NA
    )
  })
)

withr::with_options(
  c(warn = 2),
  test_that("no problem running report with (date, lag, mc)", {
    expect_error(datestamp(radf_dta_lag1, mc), regexp = NA)
    expect_error(datestamp(radf_dta_lag1, mc, option = "sadf"), regexp = NA)
    expect_error(plot(radf_dta_lag1, mc), regexp = NA)
    expect_error(plot(radf_dta_lag1, mc, option = "sadf"), regexp = NA)
    expect_error(plot(radf_dta_lag1, mc, plot_type = "single"), regexp = NA)
    expect_error(plot(radf_dta_lag1, mc, option = "sadf", plot_type = "single"),
      regexp = NA)
  })
)

withr::with_options(
  c(warn = 2),
  test_that("no problem running report (date, wb)", {
    expect_error(datestamp(radf_dta, wb), regexp = NA)
    expect_error(datestamp(radf_dta, wb, option = "sadf"),
      message("Explosive periods with Wild Bootstraped critical",
              "values apply only for the option 'gsadf'"))
    expect_error(plot(radf_dta, wb), regexp = NA)
    expect_error(plot(radf_dta, wb, option = "sadf"),
      message("Explosive periods with Wild Bootstraped critical",
              "values apply only for the option 'gsadf'"))
    expect_error(plot(radf_dta, wb, plot_type = "single"), regexp = NA)
    expect_error(
      plot(radf_dta, wb, option = "sadf", plot_type = "single"),
      message("Explosive periods with Wild Bootstraped critical",
              "values apply only for the option 'gsadf'"))
  })
)

withr::with_options(
  c(warn = 2),
  test_that("no problem running report (date, lag, wb)", {
    expect_error(datestamp(radf_dta_lag1, wb), regexp = NA)
    expect_error(datestamp(radf_dta_lag1, wb, option = "sadf"),
      message("Explosive periods with Wild Bootstraped critical",
              "values apply only for the option 'gsadf'"))
  })
)
