context("test_report")

set.seed(1234)
vec <- sim_dgp1(100)
radf_vec <- radf(vec)
mc <- mc_cv(100, 200)

# Arguements
test_that("Reporting works", {
  expect_output(str(report(radf_vec, mc)), "List of 1")
  expect_output(str(datestamp(radf_vec, mc)), "List of 1")
})
