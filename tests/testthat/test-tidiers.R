context("tidiers")

test_that("tidy works", {
  expect_error(tidy(radf_dta), NA)
  expect_error(tidy(radf_dta, "long"), NA)
  expect_error(tidy(mc), NA)
  expect_error(tidy(mc, "long"), NA)
  expect_error(tidy(wb), NA)
  expect_error(tidy(wb, "long"), NA)

  # Panel
  expect_error(tidy(radf_dta, panel = TRUE), NA)
  expect_error(tidy(radf_dta, "long", panel = TRUE), NA)
  expect_error(tidy(sb), NA)
  expect_error(tidy(sb, "long"), NA)
})

test_that("tidy_join works", {
  expect_error(tidy_join(radf_dta, mc), NA)
  expect_error(tidy_join(radf_dta, wb), NA)
  expect_error(tidy_join(radf_dta, sb), NA)
})

test_that("augment works",{
  expect_error(augment(radf_dta), NA)
  expect_error(augment(radf_dta, "long"), NA)
  expect_error(augment(radf_dta, "long", trunc = FALSE), NA)
  expect_error(augment(mc), NA)
  expect_error(augment(mc, "long"), NA)
  expect_error(augment(mc, "long", trunc = FALSE), NA)
  expect_error(augment(wb), NA)
  expect_error(augment(wb, "long"), NA)
  expect_error(augment(wb, "long", trunc = FALSE), NA)

  # Panel
  expect_error(augment(radf_dta, panel = TRUE),NA)
  expect_error(augment(radf_dta, "long", panel = TRUE),NA)
  expect_error(augment(radf_dta, "long", panel = TRUE, trunc = FALSE),NA)
  expect_error(augment(sb), NA)
  expect_error(augment(sb, "long"), NA)
  expect_error(augment(sb, "long", trunc = FALSE), NA)

})

test_that("augment_join works",{
  expect_error(augment_join(radf_dta, mc),NA)
  expect_error(augment_join(radf_dta, wb),NA)
  expect_error(augment_join(radf_dta, sb),NA)
})

test_that("tidy output", {
  nms <- c("adf", "sadf", "gsadf")
  expect_equal(names(tidy(radf_dta)), c("id", nms))
  expect_equal(names(tidy(radf_dta, format = "long")), c("id", "stat", "tstat"))
  expect_equal(names(tidy(mc)), c("sig", nms))
  expect_equal(names(tidy(mc, format = "long")), c("stat", "sig", "crit"))
  expect_equal(names(tidy(wb)), c("id", "sig", nms))
  expect_equal(names(tidy(wb, format = "long")), c("id", "stat", "sig", "crit"))
  expect_equal(names(tidy(sb)), c("id","sig", "gsadf_panel"))
  expect_equal(names(tidy(sb, format = "long")), c("id", "stat", "sig", "crit"))

})

test_that("augment output",{
  expect_equal(names(augment(radf_dta)),
               c("key", "index", "id", "data","badf", "bsadf"))
  expect_equal(names(augment(radf_dta, format = "long")),
               c("key", "index", "id", "data", "stat", "tstat"))
  expect_equal(names(augment(mc)), c("key", "sig", "badf", "bsadf"))
  expect_equal(names(augment(mc, format = "long")), c("key", "stat", "sig", "crit"))

  id_nms <- c("psy1", "psy2", "evans", "div", "blan")
  expect_equal(names(augment(wb)), c("key", "index", "id", "sig", "badf", "bsadf"))
  expect_equal(names(augment(wb, format = "long")),
               c("key", "index", "id", "stat", "sig", "crit"))
  expect_equal(names(augment(sb)), c("key", "index", "sig", "bsadf_panel"))
  expect_equal(names(augment(sb, format = "long")),
               c("key", "index", "id", "stat","sig", "crit"))
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

test_that("old-glance output", {
  # Glance
  expect_equal(names(tidy(radf_dta, panel = TRUE)), "gsadf_panel")
})

