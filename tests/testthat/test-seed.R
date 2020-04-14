context("seed")
skip(TRUE)
# avail_cores <- Sys.getenv("_R_CHECK_LIMIT_CORES_", "")
# options(exuber.ncores = 2)

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

test_that("seed gets the same results - wb",{
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

# test_that("seed is the same with or without parallel", {
#   skip_on_cran()
#   options(exuber.parallel = TRUE)
#   x <- mc_cv(10, nrep = 20, seed = 123)
#   options(exuber.parallel = FALSE)
#   y <- mc_cv(10, nrep = 20, seed = 123)
#   expect_true(all.equal(x,y))
# })

test_that("local options", {
  skip_on_cran()
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

test_that("global options works", {
  skip_on_cran()
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
  skip_on_cran()
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

