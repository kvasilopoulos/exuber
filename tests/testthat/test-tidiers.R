context("tidiers")

test_that("tidy output", {
  nms <- c("adf", "sadf", "gsadf")
  expect_equal(names(tidy(radf_dta)), c("id", nms))
  expect_equal(names(tidy(radf_dta, format = "long")), c("id", "name", "tstat"))
  expect_equal(names(tidy(mc)), c("sig", nms))
  expect_equal(names(tidy(mc, format = "long")), c("name", "sig", "crit"))
  expect_equal(names(tidy(wb)), c("id", "sig", nms))
  expect_equal(names(tidy(wb, format = "long")), c("id", "name", "sig", "crit"))
  expect_equal(names(tidy(sb)), c("id","sig", "gsadf_panel"))
  expect_equal(names(tidy(sb, format = "long")), c("id", "name", "sig", "crit"))

})

test_that("augment output",{
  expect_equal(names(augment(radf_dta)),
               c("key", "index", "id", "badf", "bsadf"))
  expect_equal(names(augment(radf_dta, format = "long")),
               c("key", "index", "id", "name", "tstat"))
  expect_equal(names(augment(mc)), c("key", "sig", "badf", "bsadf"))
  expect_equal(names(augment(mc, format = "long")), c("key", "name", "sig", "crit"))

  id_nms <- c("psy1", "psy2", "evans", "div", "blan")
  expect_equal(names(augment(wb)), c("key", "index", "sig", "name", id_nms))
  expect_equal(names(augment(wb, format = "long")),
               c("key", "index", "id", "name", "sig", "crit"))
  expect_equal(names(augment(sb)), c("key", "index", "sig", "bsadf_panel"))
  expect_equal(names(augment(sb, format = "long")),
               c("key", "index", "id", "name","sig", "crit"))

})

test_that("augment_join", {
  vec_na <- function(x) is.na(x) %>% all()
  expect_false(augment_join(radf_dta, mc) %>% vec_na())
  expect_false(augment_join(radf_dta_lag1, mc) %>% vec_na())
  expect_false(augment_join(radf_dta_lag1, mc) %>% vec_na())
  expect_false(augment_join(radf_dta_lag1, wb) %>% vec_na())
  expect_false(augment_join(radf_dta_lag1, sb1) %>% vec_na())
  expect_false(augment_join(radf_dta_lag1, sb1) %>% vec_na())
  expect_error(augment_join(radf_dta_lag1, sb))
})

test_that("glance output", {
  # Glance
  expect_equal(names(tidy(radf_dta, panel = TRUE)), "gsadf_panel")
})
