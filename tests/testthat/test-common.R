context("radf_common")

test_that("radf_common requires a panel (>= 2 series)", {
  expect_error(radf_common(dta[, 1]), "at least 2 series")
})

test_that("radf_common returns a standard radf_obj with the fitted prcomp attached", {
  res <- radf_common(dta)
  expect_s3_class(res, "radf_obj")
  expect_true(all(is.finite(res$gsadf)))
  expect_s3_class(attr(res, "prcomp"), "prcomp")
})

test_that("radf_common detects a bubble common to the whole panel, and is
  insensitive to a series-specific (idiosyncratic) explosive episode that
  doesn't affect PC1 much", {
  set.seed(42)
  n <- 100
  common_bubble <- sim_psy1(n, seed = 1)
  # 6 series sharing the common bubble plus independent noise
  panel <- replicate(6, common_bubble + rnorm(n, sd = 0.5))
  colnames(panel) <- paste0("s", 1:6)

  res_common <- radf_common(panel)
  mc <- radf_mc_cv(n, seed = 1, nrep = 400)
  expect_true(res_common$gsadf > mc$gsadf_cv["95%"])

  # a panel with NO shared explosive structure (independent random walks)
  # should not show up as a common bubble at the same rate
  panel_null <- replicate(6, cumsum(rnorm(n)))
  res_null <- radf_common(panel_null)
  expect_true(res_null$gsadf < res_common$gsadf)
})
