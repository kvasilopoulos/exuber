context("test-dist")

test_that("run properly", {
  expect_error(mc_distr(100, nrep = 10), regexp = NA)
  expect_error(wb_distr(dta, nboot = 10), regexp = NA)
  expect_error(sb_distr(dta, nboot = 10), regexp = NA)
})

test_that("tidy methods work",{

  mcd <- mc_distr(100, nrep = 10)
  wbd <- wb_distr(dta, nboot = 10)
  sbd <- sb_distr(dta, nboot = 10)

  expect_error(tidy(mcd), regexp = NA)
  expect_error(tidy(wbd), regexp = NA)
  expect_error(tidy(sbd), regexp = NA)

  # glance_warning <- function(x)
  #   glue("No glance method for objects of class {class(x)}")
  # augment_warning <- function(x)
  #   glue("No augment method for objects of class {class(x)}")

  expect_error(glance(mcd))
  expect_error(glance(wbd))
  expect_error(glance(sbd))

  expect_error(augment(mcd))
  expect_error(augment(wbd))
  expect_error(augment(sbd))


  nms <- c("adf", "sadf", "gsadf")
  expect_equal(names(tidy(mcd)), nms)
  expect_equal(names(tidy(wbd)), c("id", nms))
  expect_equal(names(tidy(sbd)), c("gsadf_panel"))

})


test_that("autoplot works",{

  mcd <- mc_distr(100, nrep = 10)
  wbd <- wb_distr(dta, nboot = 10)
  sbd <- sb_distr(dta, nboot = 10)

  expect_error(autoplot(mcd), regexp = NA)
  expect_error(autoplot(wbd), regexp = NA)
  expect_error(autoplot(sbd), regexp = NA)
})

