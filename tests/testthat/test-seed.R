context("seed")

test_that("local options", {
  options(exuber.global_seed = NA)
  expect_false(
    isTRUE(
      all.equal(
        mc_cv(100, nrep = 20)$gsadf,
        mc_cv(100, nrep = 20)$gsadf
      )
    )
  )
  expect_true(
    isTRUE(
      all.equal(
        mc_cv(100, nrep = 50, seed = 124)$gsadf,
        mc_cv(100, nrep = 50, seed = 124)$gsadf
      )
    )
  )
})

test_that("global options", {
  options(exuber.global_seed = 124)
  expect_true(
    isTRUE(
      all.equal(
        mc_cv(100, nrep = 50)$gsadf,
        mc_cv(100, nrep = 50)$gsadf
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
        mc_cv(100, nrep = 50, seed = 123)$gsadf,
        mc_cv(100, nrep = 50, seed = 123)$gsadf
      )
    )
  )
  options(exuber.global_seed = NA)
})






