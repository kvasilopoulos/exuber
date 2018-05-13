context("test_radf")

# Simulate data
set.seed(444)
df <- matrix(c(sim_dgp1(100), sim_dgp2(100), sim_evans(100), sim_div(100), sim_blan(100)), ncol = 5)
radf_df <- radf(df)
radf_df_lag1 <- radf(df, lag = 1)
mc <- mc_cv(100, 200)

withr::with_options(
  c(warn = 2),
  test_that("no problem running report",{
    expect_error(report(radf_df, mc), regexp = NA)
    expect_error(diagnostics(radf_df, mc), regexp = NA)
    expect_error(diagnostics(radf_df, mc, option = "sadf"), regexp = NA)
    expect_error(datestamp(radf_df, mc), regexp = NA)
    expect_error(datestamp(radf_df, mc, option = "sadf"), regexp = NA)
    expect_error(plot(radf_df, mc), regexp = NA)
    expect_error(plot(radf_df, mc, option = "sadf"), regexp = NA)
    expect_error(plot(radf_df, mc, plot_type = "single"), regexp = NA)
  })
)

withr::with_options(
  c(warn = 2),
  test_that("no problem running report with lag",{
    expect_error(report(radf_df_lag1, mc), regexp = NA)
    expect_error(diagnostics(radf_df_lag1, mc), regexp = NA)
    expect_error(diagnostics(radf_df_lag1, mc, option = "sadf"), regexp = NA)
    expect_error(datestamp(radf_df_lag1, mc), regexp = NA)
    expect_error(datestamp(radf_df_lag1, mc, option = "sadf"), regexp = NA)
    expect_error(plot(radf_df_lag1, mc), regexp = NA)
    expect_error(plot(radf_df_lag1, mc, option = "sadf"), regexp = NA)
  })
)

test_that("Right output", {
  expect_output(str(radf_df), "List of 5")
  expect_output(str(attributes(radf_df)), "List of 6")
})

series_names <- c("Series 1", "Series 2", "Series 3", "Series 4")

test_that("colnames check",{
  expect_equal(col_names(radf(df)), series_names)
  expect_equal(col_names(radf(as.ts(df))), series_names)
  expect_equal(col_names(radf(as.data.frame(df, optional = TRUE))), series_names)
})

series_index_names <- index(radf_df) <- c("dgp1", "dgp2", "evans",  "dividends", "blanchard")

test_that("colnames check after index",{
  expect_equal(col_names(radf(df)), series_names)
  expect_equal(col_names(radf(as.ts(df))), series_names)
  expect_equal(col_names(radf(as.data.frame(df, optional = TRUE))), series_names)
})

test_that("lag check", {
  expect_error(radf(df, lag = -1), "Argument 'lag' should be a non-negative integer")
  expect_equal(lagr(radf_df_lag1), 1)
})

test_that("minw check radf", {
  expect_error(radf(df, minw = -1), "Argument 'minw' should be a positive integer")
  expect_error(radf(df, minw = 0), "Argument 'minw' should be a positive integer")
  expect_error(radf(df, minw = 2), "Argument 'minw' is too small")
})

test_that("minw check cv", {
  expect_error(mc_cv(df, minw = -1), "Argument 'minw' should be a positive integer")
  expect_error(mc_cv(df, minw = 0), "Argument 'minw' should be a positive integer")
  expect_error(mc_cv(df, minw = 2), "Argument 'minw' is too small")
  expect_error(wb_cv(df, minw = -1), "Argument 'minw' should be a positive integer")
  expect_error(wb_cv(df, minw = 0), "Argument 'minw' should be a positive integer")
  expect_error(wb_cv(df, minw = 2), "Argument 'minw' is too small")
})

index(radf_df) <- seq(as.Date('1991/10/01'), as.Date('2000/01/01'), by = "month")

withr::with_options(
  c(warn = 2),
  test_that("no problem running with dates",{
    expect_error(datestamp(radf_df, mc), regexp = NA)
    expect_error(datestamp(radf_df, mc, option = "sadf"), regexp = NA)
    expect_error(plot(radf_df, mc), regexp = NA)
    expect_error(plot(radf_df, mc, option = "sadf"), regexp = NA)
    expect_error(plot(radf_df, mc, plot_type = "single"), regexp = NA)
  })
)
