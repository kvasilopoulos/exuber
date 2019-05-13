context("tidiers")

test_that("tidy output", {

  nms <- c("adf", "sadf", "gsadf")

  expect_equal(names(tidy(radf_dta)), c("id", nms))
  expect_equal(names(tidy(radf_dta, format = "long")), c("id", "name", "tstat"))


  expect_equal(names(tidy(mc)), c("sig", nms))
  expect_equal(names(tidy(mc, format = "long")), c("name", "sig", "crit"))

  expect_equal(names(tidy(wb)), c("id", "sig", nms))
  expect_equal(names(tidy(wb, format = "long")), c("id", "name", "sig", "crit"))

  expect_equal(names(tidy(sb)), c("sig", "gsadf_panel"))
  expect_equal(names(tidy(sb, format = "long")), c("id", "name", "sig", "crit"))

})


test_that("augment output",{

  expect_equal(names(augment(radf_dta)),
               c("index", "id", "bsadf_panel", "badf", "bsadf"))
  expect_equal(names(augment(radf_dta, format = "long")),
               c("index", "id", "name", "tstat"))


  expect_equal(names(augment(mc)), c("sig", "badf", "bsadf"))
  expect_equal(names(augment(mc, format = "long")), c("name", "sig", "crit"))

  id_nms <- c("dgp1", "dgp2", "evans", "div", "blan")

  expect_equal(names(augment(wb)), c("sig", "name", id_nms))
  expect_equal(names(augment(wb, format = "long")),
               c("id", "name", "sig", "crit"))

  expect_equal(names(augment(sb)), c("sig", "bsadf_panel"))
  expect_equal(names(augment(sb, format = "long")),
               c("id", "name", "sig", "crit"))

})

test_that("glance output",{
  # Glance
  expect_equal(names(glance(radf_dta)), "panel")
})
