context("seed")

test_that("seed gets the same results",{
  skip_on_cran()
  options(exuber.parallel = TRUE)
  expect_true(
    all.equal(
      mc_cv(10, nrep = 20, seed = 123),
      mc_cv(10, nrep = 20, seed = 123)
    )
  )
  options(exuber.parallel = FALSE)
})

test_that("seed gets the same results",{
  skip_on_cran()
  options(exuber.parallel = TRUE)
  expect_true(
    all.equal(
      wb_cv(dta, nboot = 20, seed = 123),
      wb_cv(dta, nboot = 20, seed = 123)
    )
  )
  options(exuber.parallel = FALSE)
})

test_that("seed is the same with or without paraller", {
  skip_on_cran()
  x <- mc_cv(10, nrep = 20, seed = 123)
  options(exuber.parallel = TRUE)
  expect_true(
    all.equal(
      x,
      mc_cv(10, nrep = 20, seed = 123)
    )
  )
  options(exuber.parallel = FALSE)
})

test_that("local options", {
  options(exuber.global_seed = NA)
  expect_false(
    isTRUE(
      all.equal(
        mc_cv(100, nrep = 20)$gsadf_cv,
        mc_cv(100, nrep = 20)$gsadf_cv
      )
    )
  )
  expect_true(
    isTRUE(
      all.equal(
        mc_cv(100, nrep = 20, seed = 124)$gsadf_cv,
        mc_cv(100, nrep = 20, seed = 124)$gsadf_cv
      )
    )
  )
})

test_that("global options", {
  options(exuber.global_seed = 124)
  expect_true(
    isTRUE(
      all.equal(
        mc_cv(100, nrep = 20)$gsadf_cv,
        mc_cv(100, nrep = 20)$gsadf_cv
      )
    )
  )
  options(exuber.global_seed = NA)
})

test_that("local options overwrite global", {
  options(exuber.global_seed = 124)
  expect_true(
    isTRUE(
      all.equal(
        mc_cv(100, nrep = 20, seed = 123)$gsadf_cv,
        mc_cv(100, nrep = 20, seed = 123)$gsadf_cv
      )
    )
  )

  expect_false(
    isTRUE(
      all.equal(
        mc_cv(100, nrep = 20, seed = 123)$gsadf_cv,
        mc_cv(100, nrep = 20, seed = 124)$gsadf_cv
      )
    )
  )
  options(exuber.global_seed = NA)
})
