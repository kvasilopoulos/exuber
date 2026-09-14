context("deprecated")

test_that("deprecated functions warn and call through", {
  expect_warning(cn <- col_names(radf_dta), "'col_names' is deprecated")
  expect_equal(cn, series_names(radf_dta))

  expect_warning(m <- mc_cv(20, nrep = 5, seed = 1), "'mc_cv' is deprecated")
  expect_equal(m, radf_mc_cv(20, nrep = 5, seed = 1))

  expect_warning(w <- wb_cv(dta, nboot = 5, seed = 1), "'wb_cv' is deprecated")
  expect_equal(w, radf_wb_cv(dta, nboot = 5, seed = 1))

  expect_warning(s <- sb_cv(dta, nboot = 5, seed = 1), "'sb_cv' is deprecated")
  expect_equal(s, radf_sb_cv(dta, nboot = 5, seed = 1))
})

test_that("defunct functions", {
  expect_error(fortify(radf_dta), "'fortify.radf_obj' is defunct", class = "defunctError")
  expect_error(ggarrange(1), "'ggarrange' is defunct", class = "defunctError")
  expect_error(report(1), "'report' is defunct", class = "defunctError")
  expect_error(sim_dgp1(1), "'sim_dgp1' is defunct", class = "defunctError")
  expect_error(sim_dgp2(1), "'sim_dgp2' is defunct", class = "defunctError")
})
