context("test-dist")

test_that("run properly", {
  expect_error(radf_mc_distr(100, nrep = 10), regexp = NA)
  expect_error(radf_wb_distr(dta, nboot = 10), regexp = NA)
  expect_error(radf_sb_distr(dta, nboot = 10), regexp = NA)
})

test_that("tidy methods work",{

  mcd <- radf_mc_distr(100, nrep = 10)
  wbd <- radf_wb_distr(dta, nboot = 10)
  sbd <- radf_sb_distr(dta, nboot = 10)

  expect_error(tidy(mcd), regexp = NA)
  expect_error(tidy(wbd), regexp = NA)
  expect_error(tidy(sbd), regexp = NA)

  # No glance or augment methods in radf_distr
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

  mcd <- radf_mc_distr(100, nrep = 10)
  wbd <- radf_wb_distr(dta, nboot = 10)
  sbd <- radf_sb_distr(dta, nboot = 10)

  expect_error(autoplot(mcd), regexp = NA)
  expect_error(autoplot(wbd), regexp = NA)
  expect_error(autoplot(sbd), regexp = NA)
})

