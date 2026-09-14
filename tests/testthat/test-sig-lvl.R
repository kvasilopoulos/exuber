context("sig_lvl")

# Regression test for the Aug-2026 fix: diagnostics()/datestamp()/autoplot()
# must gate on the critical value of the *requested* sig_lvl, not always 95.
test_that("diagnostics() gates on the requested sig_lvl", {
  tj <- tidy_join(radf_dta, mc) %>% dplyr::filter(stat == "gsadf")
  for (lvl in c(90, 95, 99)) {
    dg <- diagnostics(radf_dta, cv = mc, sig_lvl = lvl)
    expected <- tj$id[tj$sig == lvl & tj$tstat >= tj$crit]
    expect_setequal(dg$positive, as.character(expected))
    expect_equal(attr(dg, "sig_lvl"), lvl)
  }
  expect_error(diagnostics(radf_dta, cv = mc, sig_lvl = 80), "sig_lvl")
})

test_that("datestamp()/autoplot() error names the requested level when nothing rejects", {
  expect_error(datestamp(radf_95, cv = mc, sig_lvl = 99), "1% significance level")
  expect_error(autoplot(radf_95, cv = mc, sig_lvl = 99), "1% significance level")
})

test_that("datestamp() at 90 never finds fewer episodes than at 99", {
  ds90 <- datestamp(radf_dta, cv = mc, sig_lvl = 90)
  ds99 <- datestamp(radf_dta, cv = mc, sig_lvl = 99)
  expect_gte(sum(attr(ds90, "dummy"), na.rm = TRUE), sum(attr(ds99, "dummy"), na.rm = TRUE))
  expect_equal(attr(ds90, "sig_lvl"), 90)
  expect_equal(attr(ds99, "sig_lvl"), 99)
})

test_that("every sig_lvl-taking function agrees on the 0-100 scale", {
  y <- sim_psy1(80, seed = 1)
  expect_error(lbi_test(y, sig_lvl = 0.95), "0-100 scale")
  expect_error(ssu_test(y, sig_lvl = 0.95), "must be one of")
  expect_error(monitor_lbi(y, sig_lvl = 0.95), "must be one of")
  expect_error(quantile_test(y, sig_lvl = 0.95, nrep = 5), "should be one of")
  expect_error(cobubble_test(y, y, sig_lvl = 0.05, nboot = 5), "should be one of")
  expect_error(rootstamp(y, sig_lvl = 0.95), "0-100 scale")
  expect_error(monitor(y, boundary = "kurozumi", sig_lvl = 0.95), "must be one of")
  expect_error(monitor_cusum(y, boundary = "finite", sig_lvl = 0.95), "must be one of")
})
