context("test_report")

mc_crit <- mc_cv(100, 10)

# Arguements
test_that("multiplication works", {
  expect_output(str(mc_crit), "List of 5")
})
