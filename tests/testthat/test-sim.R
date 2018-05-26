context("sim")

test_that("sim_evans", {
  expect_error(sim_evans(100, delta = 1.5))
})

test_that("sim_div", {
  expect_error(sim_div(100, log = T), regexp = NA)
  expect_error(sim_div(100, output = "d"), regexp = NA)
})

test_that("dgp1", {
  expect_error(sim_dgp1(100, te = 120))
  expect_error(sim_dgp1(100, tf = 120))
})
