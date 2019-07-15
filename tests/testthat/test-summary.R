context("summary")

test_that("printing coverage", {
  expect_error(capture.output(summary(radf_dta, mc)), regexp = NA)
  expect_error(capture.output(diagnostics(radf_dta, mc)), regexp = NA)
  expect_error(capture.output(summary(radf_dta, sb)), NA) # Panel
  expect_error(capture.output(diagnostics(radf_dta, sb)), regexp = NA)
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
  expect_message(print(
    diagnostics(radf_div, mc)), "Cannot reject H0")
  expect_message(
    print(diagnostics(radf_95, mc)),
    "Cannot reject H0 for significance level 95%"
  )
})

test_that("different minw", {
  msg <- "minimum window does not match"
  expect_error(summary(radf_dta, mc2_minw20), msg)
  expect_error(diagnostics(radf_dta, mc2_minw20), msg)
  expect_error(datestamp(radf_dta, mc2_minw20), msg)
  expect_error(summary(radf_dta, wb2_minw20), msg)
  expect_error(diagnostics(radf_dta, wb2_minw20), msg)
  expect_error(datestamp(radf_dta, wb2_minw20), msg)
  expect_error(summary(radf_dta, sb2_minw20), msg)
  expect_error(diagnostics(radf_dta, sb2_minw20), msg)
  expect_error(datestamp(radf_dta, sb2_minw20), msg)
})

test_that("Correct output in summary/datestamp", {
  expect_output(str(summary(radf_dta, mc)), "List of 5")
  expect_output(str(datestamp(radf_dta, mc)), "List of 4") # 4 plus dummy
  expect_output(str(summary(radf_dta, wb)), "List of 5")
  expect_output(str(datestamp(radf_dta, wb)), "List of 2")
})

withr::with_options(
  c(warn = 2),
  test_that("no problem running rp/ds/dg (mc)", {
    expect_error(summary(radf_dta, mc), regexp = NA)
    expect_error(diagnostics(radf_dta, mc), regexp = NA)
    expect_error(diagnostics(radf_dta, mc, option = "sadf"), regexp = NA)
    expect_error(datestamp(radf_dta, mc), regexp = NA)
    expect_error(
      datestamp(radf_dta, mc, min_duration = 50),
      "Argument 'min_duration' excludes all explosive periods"
    )
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
    expect_error(diagnostics(radf_dta, wb), regexp = NA)
    expect_error(diagnostics(radf_dta, wb, option = "sadf"), regexp = NA)
    expect_error(datestamp(radf_dta, wb), regexp = NA)
    expect_error(datestamp(radf_dta, wb, option = "sadf"), regexp = NA)
    expect_error(autoplot(radf_dta, wb), regexp = NA)
    expect_error(autoplot(radf_dta, wb, option = "sadf"), regexp = NA)
  })
)

withr::with_options(
  c(warn = 2),
  test_that("no problem running summary (lag,wb)", {
    expect_error(summary(radf_dta_lag1, wb), regexp = NA)
    expect_error(diagnostics(radf_dta_lag1, wb), regexp = NA)
    expect_error(diagnostics(radf_dta_lag1, wb, option = "sadf"), regexp = NA)
    expect_error(datestamp(radf_dta_lag1, wb), regexp = NA)
    expect_error(datestamp(radf_dta_lag1, wb, option = "sadf"), regexp = NA)
    expect_error(autoplot(radf_dta_lag1, wb), regexp = NA)
    expect_error(autoplot(radf_dta_lag1, wb, option = "sadf"), regexp = NA)
  })
)

index(radf_dta) <- seq(from = as.Date("1991/10/01"),
                       by = "month", length.out = 100)

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
    expect_error(datestamp(radf_dta, wb, option = "sadf"), regexp = NA)
    expect_error(autoplot(radf_dta, wb), regexp = NA)
    expect_error(autoplot(radf_dta, wb, option = "sadf"), regexp = NA)
  })
)

withr::with_options(
  c(warn = 2),
  test_that("no problem running summary (date, lag, wb)", {
    expect_error(datestamp(radf_dta_lag1, wb), regexp = NA)
    expect_error(datestamp(radf_dta_lag1, wb, option = "sadf"), regexp = NA)
  })
)
