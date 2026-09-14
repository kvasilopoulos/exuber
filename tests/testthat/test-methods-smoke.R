context("print/autoplot methods")

# One object per standalone class, built as cheaply as possible, then every
# print() and autoplot() method is exercised once. This is a smoke test for
# the S3 surface (a method that errors or silently falls through to
# print.default/print.data.frame fails here), not a check of the numbers.

set.seed(1)
y_smoke <- sim_psy1(100, seed = 1)
x_smoke <- sim_psy1(100, seed = 2)
suppressMessages({
  smoke <- list(
    lbi_test = lbi_test(y_smoke),
    monitor_lbi = monitor_lbi(y_smoke, r_star = 0.5),
    ssu_test = ssu_test(y_smoke, minw = 20),
    quantile_test = quantile_test(y_smoke, tau = 0.5, nrep = 10, seed = 1),
    monitor_quantile = monitor_quantile(y_smoke, minw = 20, nrep = 10, seed = 1),
    cobubble_test = cobubble_test(y_smoke, x_smoke, lag = 0, nboot = 10, seed = 1),
    contagion_reg = contagion_reg(y_smoke, x_smoke, S = 50, d = 1),
    dating_hls = dating_hls(y_smoke),
    dating_hlw = dating_hlw(y_smoke, minw = 20, nboot = 10, seed = 1),
    dating_knp = dating_knp(y_smoke),
    dating_pdc = dating_pdc(y_smoke),
    monitor_cusum = monitor_cusum(y_smoke, r_star = 0.5),
    monitor = monitor(y_smoke, r_star = 0.5, minw = 20, boundary = "kurozumi"),
    radf_recovery = radf_recovery(y_smoke, minw = 20, nrep = 10, seed = 1),
    radf_sbz_union = radf_sbz_union(y_smoke, minw = 20, nboot = 10, seed = 1),
    radf_sbz = radf_sbz(y_smoke, minw = 20),
    radf_tt = radf_tt(y_smoke, minw = 20),
    radf_sign = radf_sign(y_smoke, minw = 20),
    radf_sign_dm = radf_sign_dm(y_smoke, minw = 20),
    rootstamp = rootstamp(y_smoke[60:100])
  )
})

test_that("every standalone class has a print() method that names its function", {
  for (nm in names(smoke)) {
    expect_output(print(smoke[[nm]]), nm, info = nm)
  }
  # the radf_obj family prints through print.radf_obj
  expect_output(print(radf_dta), "radf [(]minw")
  expect_output(print(mc), "Monte Carlo")
  expect_output(print(diagnostics(radf_dta, cv = mc)), "Diagnostics")
  expect_output(print(summary(radf_dta, cv = mc)), "Summary")
  expect_output(print(datestamp(radf_dta, cv = mc)), "Datestamp")
  expect_output(print(y_smoke), "[[]1[]]")
})

test_that("every standalone class has an autoplot() method returning a ggplot", {
  # the radf_obj-family members need a matching cv (covered in their own files)
  radf_family <- c("radf_sbz", "radf_tt", "radf_sign", "radf_sign_dm")
  for (nm in setdiff(names(smoke), radf_family)) {
    expect_s3_class(autoplot(smoke[[nm]]), "ggplot")
  }
  ds <- datestamp(radf_dta, cv = mc)
  expect_s3_class(autoplot(ds), "ggplot")
  expect_s3_class(autoplot2(radf_dta, cv = mc), "ggplot")
  expect_s3_class(autoplot(radf_mc_distr(50, nrep = 20, seed = 1)), "ggplot")
  expect_s3_class(autoplot(y_smoke), "ggplot")
})

test_that("tidy.dg_radf() returns one row per series", {
  td <- tidy(diagnostics(radf_dta, cv = mc))
  expect_equal(nrow(td), ncol(dta))
  expect_named(td, c("series", "positive", "negative", "sig"))
  expect_true(all(td$positive != td$negative))
})
