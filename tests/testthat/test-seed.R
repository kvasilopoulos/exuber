context("seed")

test_that("seed reproduces Monte Carlo and bootstrap critical values", {
  skip_on_cran()
  expect_equal(radf_mc_cv(20, nrep = 20, seed = 123), radf_mc_cv(20, nrep = 20, seed = 123))
  expect_equal(radf_wb_cv(dta, nboot = 20, seed = 123), radf_wb_cv(dta, nboot = 20, seed = 123))
})

test_that("seed reproduces across serial and parallel execution", {
  skip_on_cran()
  # multisession workers load the *installed* package; under load_all() they
  # cannot see the dev tree's internals, so this only runs in R CMD check/CI
  skip_if(requireNamespace("pkgload", quietly = TRUE) && pkgload::is_dev_package("exuber"),
          "parallel workers need an installed exuber")
  withr::local_options(list(exuber.parallel = TRUE, exuber.ncores = 2))
  x <- radf_mc_cv(20, nrep = 20, seed = 123)
  withr::local_options(list(exuber.parallel = FALSE))
  y <- radf_mc_cv(20, nrep = 20, seed = 123)
  attr(x, "parallel") <- attr(y, "parallel") <- NULL
  expect_equal(x, y)
})

test_that("exuber.global_seed: unset draws differ, set draws repeat, local seed wins", {
  skip_on_cran()
  withr::local_options(list(exuber.global_seed = NA))
  expect_false(isTRUE(all.equal(
    radf_mc_cv(50, nrep = 20)$gsadf_cv, radf_mc_cv(50, nrep = 20)$gsadf_cv
  )))

  withr::local_options(list(exuber.global_seed = 124))
  expect_equal(radf_mc_cv(50, nrep = 20)$gsadf_cv, radf_mc_cv(50, nrep = 20)$gsadf_cv)
  expect_equal(radf_mc_cv(50, nrep = 20, seed = 123)$gsadf_cv, radf_mc_cv(50, nrep = 20, seed = 123)$gsadf_cv)
  expect_false(isTRUE(all.equal(
    radf_mc_cv(50, nrep = 20, seed = 123)$gsadf_cv, radf_mc_cv(50, nrep = 20, seed = 124)$gsadf_cv
  )))
})
