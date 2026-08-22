context("radf_wb_ps_cv / radf_wb_ps_distr (Phillips & Shi 2020 wild bootstrap)")

test_that("radf_wb_ps_cv returns a well-formed radf_cv/wb_cv object", {
  wb <- radf_wb_ps_cv(dta, nboot = 10)
  expect_s3_class(wb, c("radf_cv", "wb_cv"))
  expect_setequal(names(wb), c("adf_cv", "sadf_cv", "gsadf_cv", "badf_cv", "bsadf_cv"))
})

test_that("radf_wb_ps_distr returns a well-formed radf_distr/wb_distr object", {
  wbd <- radf_wb_ps_distr(dta, nboot = 10)
  expect_s3_class(wbd, c("radf_distr", "wb_distr"))
  expect_setequal(names(wbd), c("adf_distr", "sadf_distr", "gsadf_distr"))
})

test_that("radf_wb_cv2/radf_wb_distr2 are deprecated in favor of
  radf_wb_ps_cv/radf_wb_ps_distr", {
  expect_warning(radf_wb_cv2(dta, nboot = 10), "radf_wb_ps_cv")
  expect_warning(radf_wb_distr2(dta, nboot = 10), "radf_wb_ps_distr")
})

test_that("radf_wb_cv2/radf_wb_distr2 are pure pass-throughs to the renamed
  functions -- identical output for the same seed, not just similar", {
  suppressWarnings({
    old_cv <- radf_wb_cv2(dta, nboot = 10, seed = 1)
    new_cv <- radf_wb_ps_cv(dta, nboot = 10, seed = 1)
    old_distr <- radf_wb_distr2(dta, nboot = 10, seed = 1)
    new_distr <- radf_wb_ps_distr(dta, nboot = 10, seed = 1)
  })
  expect_equal(unclass(old_cv), unclass(new_cv))
  expect_equal(unclass(old_distr), unclass(new_distr))
})

test_that("radf_wb_ps_cv's tb argument (training-window boundary) still works
  post-rename -- this is what monitor() relies on", {
  wb_tb <- radf_wb_ps_cv(dta, nboot = 10, tb = 60)
  expect_equal(dim(wb_tb$badf_cv)[1], nrow(dta) - attr(wb_tb, "minw"))
})
