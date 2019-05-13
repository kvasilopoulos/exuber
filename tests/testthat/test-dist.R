context("test-dist")

test_that("run properly", {
  expect_error(mc_dist(100, nrep = 10), regexp = NA)
  expect_error(wb_dist(dta, nboot = 10), regexp = NA)
  expect_error(sb_dist(dta, nboot = 10), regexp = NA)
})

test_that("tidy methods work",{

  mcd <- mc_dist(100, nrep = 10)
  wbd <- wb_dist(dta, nboot = 10)
  sbd <- sb_dist(dta, nboot = 10)

  expect_error(tidy(mcd), regexp = NA)
  expect_error(tidy(wbd), regexp = NA)
  expect_error(tidy(sbd), regexp = NA)

  expect_error(glance(mcd),
               "no applicable method for 'glance' applied to an object of class \"mc_dist\"")
  expect_error(glance(wbd),
               "no applicable method for 'glance' applied to an object of class \"wb_dist\"")
  expect_error(glance(sbd),
               "no applicable method for 'glance' applied to an object of class \"sb_dist\"")

  expect_error(augment(mcd),
               "no applicable method for 'augment' applied to an object of class \"mc_dist\"")
  expect_error(augment(wbd),
               "no applicable method for 'augment' applied to an object of class \"wb_dist\"")
  expect_error(augment(sbd),
               "no applicable method for 'augment' applied to an object of class \"sb_dist\"")


  nms <- c("adf", "sadf", "gsadf")
  expect_equal(names(tidy(mcd)), nms)
  expect_equal(names(tidy(wbd)), c("name", nms))
  expect_equal(names(tidy(sbd)), c("gsadf_panel"))

})


test_that("autoplot works",{

  mcd <- mc_dist(100, nrep = 10)
  wbd <- wb_dist(dta, nboot = 10)
  sbd <- sb_dist(dta, nboot = 10)

  expect_error(autoplot(mcd), regexp = NA)
  expect_error(autoplot(wbd), regexp = NA)
  expect_error(autoplot(sbd), regexp = NA)
})
