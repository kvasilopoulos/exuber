context("summary")

test_that("printing coverage", {
  expect_error(capture.output(summary(radf_dta, mc)), regexp = NA)
  expect_error(capture.output(diagnostics(radf_dta, mc)), regexp = NA)
  expect_error(capture.output(summary(radf_dta, sb)), NA) # Panel
})

test_that("class checks", {
  msgx <- "Argument 'object' should be of class 'radf'"
  msgy <- "Argument 'cv' should be of class 'cv'"
  expect_error(diagnostics(dta, mc), msgx)
  expect_error(datestamp(dta, mc), msgx)
  expect_error(diagnostics(radf_dta, dta), msgy)
  expect_error(summary(radf_dta, dta), msgy)
  expect_error(datestamp(radf_dta, dta), msgy)
})

test_that("error diagnostics", {
  expect_error(diagnostics(radf_div, mc), "Cannot reject H0")
  expect_error(diagnostics(radf_95, mc),
    "Cannot reject H0 for significance level 95%")
})

test_that("different minw", {
  msg <- "Different minimum window"
  expect_error(summary(radf_dta, mc2), msg)
  expect_error(diagnostics(radf_dta, mc2), msg)
  expect_error(datestamp(radf_dta, mc2), msg)
  expect_error(summary(radf_dta, wb2), msg)
  expect_error(diagnostics(radf_dta, wb2), msg)
  expect_error(datestamp(radf_dta, wb2), msg)
  expect_error(summary(radf_dta, sb2), msg)
  expect_error(diagnostics(radf_dta, sb2), msg)
  expect_error(datestamp(radf_dta, sb2), msg)
})

test_that("panel: different lag", {
  msg <- "Different lag values"
  expect_error(summary(radf_dta, sb1), msg)
  expect_error(diagnostics(radf_dta, sb1), msg)
  expect_error(datestamp(radf_dta, sb1), msg)
  expect_error(autoplot(radf_dta, sb1), msg)
})



test_that("assert panel",{
  # expect_error(summary(radf_dta, wb), "Wrong critical values")
  # expect_error(datestamp(radf_dta, wb), "Wrong critical values")
  # expect_error(summary(radf_dta, mc), regexp = NA)
  # expect_error(datestamp(radf_dta, mc), regexp = NA)
  # expect_error(summary(radf_dta, sb), regexp = NA)
  # expect_error(datestamp(radf_dta, sb), regexp = NA)
})


test_that("Correct output in summary/datestamp", {
  expect_output(str(summary(radf_dta, mc)), "List of 5")
  expect_output(str(datestamp(radf_dta, mc)), "List of 5") # 4 plus bool
  expect_output(str(summary(radf_dta, wb)), "List of 5")
  expect_output(str(datestamp(radf_dta, wb)), "List of 5")
})

withr::with_options(
  c(warn = 2),
  test_that("no problem running rp/ds/dg (mc)", {
    expect_error(summary(radf_dta, mc), regexp = NA)
    expect_error(diagnostics(radf_dta, mc), regexp = NA)
    expect_error(diagnostics(radf_dta, mc, option = "sadf"), regexp = NA)
    expect_error(datestamp(radf_dta, mc), regexp = NA)
    expect_error(datestamp(radf_dta, mc, min_duration = 50),
                 "Argument 'min_duration' excludes all the explosive periods")
    expect_error(datestamp(radf_dta, mc, option = "sadf"), regexp = NA)
    expect_error(autoplot(radf_dta, mc), regexp = NA)

# wokr here ---------------------------------------------------------------

    expect_error(autoplot(radf_dta, mc, option = "sadf"), regexp = NA)
  })
)

withr::with_options(
  c(warn = 2),
  test_that("no problem running summary (lag, mc)", {
    expect_error(summary(radf_dta_lag1, mc), regexp = NA)
    expect_error(diagnostics(radf_dta_lag1, mc), regexp = NA)
    expect_error(diagnostics(radf_dta_lag1, mc, option = "sadf"), regexp = NA)
    expect_error(datestamp(radf_dta_lag1, mc), regexp = NA)
    expect_error(datestamp(radf_dta_lag1, mc, option = "sadf"), regexp = NA)
    expect_error(autoplot(radf_dta_lag1, mc), regexp = NA)
  })
)




withr::with_options(
  c(warn = 2),
  test_that("no problem running summary (wb)", {
    expect_error(summary(radf_dta, wb), regexp = NA)
    msg <- "'sadf' applies onyl to MC critical values"
    expect_error(diagnostics(radf_dta, wb), regexp = NA)
    expect_error(diagnostics(radf_dta, wb, option = "sadf"), msg)
    expect_error(datestamp(radf_dta, wb), regexp = NA)
    expect_error(datestamp(radf_dta, wb, option = "sadf"), msg)
    expect_error(autoplot(radf_dta, wb), regexp = NA)
    expect_error(autoplot(radf_dta, wb, option = "sadf"), msg)
  })
)

withr::with_options(
  c(warn = 2),
  test_that("no problem running summary (lag,wb)", {
    expect_error(summary(radf_dta_lag1, wb), regexp = NA)
    expect_error(diagnostics(radf_dta_lag1, wb), regexp = NA)
    msg <- "'sadf' applies onyl to MC critical values"
    expect_error(diagnostics(radf_dta_lag1, wb, option = "sadf"), msg)
    expect_error(datestamp(radf_dta_lag1, wb), regexp = NA)
    expect_error(datestamp(radf_dta_lag1, wb, option = "sadf"), msg)
    expect_error(autoplot(radf_dta_lag1, wb), regexp = NA)
    expect_error(autoplot(radf_dta_lag1, wb, option = "sadf"), msg)
    expect_error(autoplot(radf_dta_lag1, wb, plot_type = "single"), regexp = NA)
  })
)

index(radf_dta) <- seq(as.Date("1991/10/01"), by = "month", length.out = 100)

withr::with_options(
  c(warn = 2),
  test_that("no problem running with (date, mc)", {
    expect_error(datestamp(radf_dta, mc), regexp = NA)
    expect_error(datestamp(radf_dta, mc, option = "sadf"), regexp = NA)
    expect_error(autoplot(radf_dta, mc), regexp = NA)
    expect_error(autoplot(radf_dta, mc, option = "sadf"), regexp = NA)
  })
)

withr::with_options(
  c(warn = 2),
  test_that("no problem running summary with (date, lag, mc)", {
    expect_error(datestamp(radf_dta_lag1, mc), regexp = NA)
    expect_error(datestamp(radf_dta_lag1, mc, option = "sadf"), regexp = NA)
    expect_error(autoplot(radf_dta_lag1, mc), regexp = NA)
    expect_error(autoplot(radf_dta_lag1, mc, option = "sadf"), regexp = NA)
  })
)

withr::with_options(
  c(warn = 2),
  test_that("no problem running summary (date, wb)", {
    expect_error(datestamp(radf_dta, wb), regexp = NA)
    msg <- "'sadf' applies onyl to MC critical values"
    expect_error(datestamp(radf_dta, wb, option = "sadf"), msg)
    expect_error(autoplot(radf_dta, wb), regexp = NA)
    expect_error(autoplot(radf_dta, wb, option = "sadf"), msg)
  })
)

withr::with_options(
  c(warn = 2),
  test_that("no problem running summary (date, lag, wb)", {
    expect_error(datestamp(radf_dta_lag1, wb), regexp = NA)
    msg <- "'sadf' applies onyl to MC critical values"
    expect_error(datestamp(radf_dta_lag1, wb, option = "sadf"), msg)
  })
)
