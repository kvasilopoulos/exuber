context("seed")

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






