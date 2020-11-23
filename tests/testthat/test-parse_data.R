test_that("parse the right index", {
  expect_equal(index(parse_data(sim_data_wdate)), sim_data_wdate[,6, drop = TRUE])
})

test_that("index > 1",{
  dating_m <- seq(as.Date("1997/01/01"), by = "month", length.out = 100)
  df <- data.frame(dating_m, dta, dating_m)
  expect_error(parse_data(df))
})

test_that("right data format", {
  expect_error(parse_data(ggplot2::economics), NA)
  expect_error(parse_data(ggplot2::economics_long),
  "The data do not have the appropriate format.")
})
