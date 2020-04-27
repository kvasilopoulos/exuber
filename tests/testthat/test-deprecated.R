context("deprecated")

skip(TRUE)

test_that("deprecated functions", {
  expect_warning(col_names(radf_dta), "'col_names' is deprecated.")
  expect_warning(mc_cv(20, nrep = 1), "'mc_cv' is deprecated.")
  expect_warning(wb_cv(dta, nboot = 1), "'wb_cv' is deprecated.")
  expect_warning(sb_cv(dta, nboot = 1), "'sb_cv' is deprecated.")
})

test_that("defunct functions", {
  expect_error(fortify(radf_dta), "'fortify.radf_obj' is defunct.", class = "defunctError")
  expect_error(ggarrange(1), "'ggarrange' is defunct.", class = "defunctError")
  expect_error(report(1), "'report' is defunct.", class = "defunctError")
  expect_error(sim_dgp1(1), "'sim_dgp1' is defunct.", class = "defunctError")
  expect_error(sim_dgp2(1), "'sim_dgp2' is defunct.", class = "defunctError")
})
