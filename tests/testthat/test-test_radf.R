context("test_radf")

dta <- sim_dgp1(100)
rf <- radf(dta)
rf1 <- radf(dta, lag = 1)

test_that("Correct output", {
  expect_output(str(rf), "List of 5")
  expect_output(str(attributes(rf)), "List of 6")
})

test_that("colnames",{
  expect_equal(col_names(radf(dta)), "series1")
  expect_equal(col_names(radf(as.matrix(dta))), "series1")
  expect_equal(col_names(radf(as.ts(dta))), "series1")
  expect_equal(col_names(radf(as.data.frame(dta, optional = TRUE))), "series1")
})


test_that("Lag check", {
  expect_error(radf(dta, lag = -1), "Argument 'lag' should be a non-negative integer")
  expect_equal(attributes(rf1)$lag, 1)
})

dtm <- cbind(sim_dgp1(100), sim_dgp2(100), sim_div(100))


# test_that("colnames",{
#   expect_equal(col_names(radf(dtm)), colnames(dtm))
#   expect_equal(col_names(radf(as.matrix(dta))), colnames(dtm))
#   expect_equal(col_names(radf(as.ts(dta))), colnames(dtm))
#   expect_equal(col_names(radf(as.data.frame(dta))), colnames(dtm))
#   expect_equal(col_names(radf(zoo::as.zoo(dta))), colnames(dtm))
# })


# Arguements

test_that("minw is correctly specified", {
  expect_error(radf(dta, minw = -1), "Argument 'minw' should be a positive integer")
  expect_error(radf(dta, minw = 0), "Argument 'minw' should be a positive integer")
  expect_error(radf(dta, minw = 2), "Argument 'minw' is too small")
})
