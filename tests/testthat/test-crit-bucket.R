write_synthetic_crit <- function(path, n = 20L, minw = 5L, lag = 2L) {
  nrows <- n - minw
  con <- xzfile(path, "wb")
  writeBin(as.integer(c(n, minw, lag, nrows)), con, size = 4L)
  writeBin(as.double(1:3), con)
  writeBin(as.double(4:6), con)
  writeBin(as.double(7:9), con)
  writeBin(as.double(t(matrix(seq_len(nrows * 3), ncol = 3, byrow = TRUE))), con)
  close(con)
}

test_that("parse_crit_bin round-trips the documented binary layout", {
  path <- withr::local_tempfile(fileext = ".bin.xz")
  write_synthetic_crit(path)
  cv <- parse_crit_bin(path)
  expect_s3_class(cv, c("radf_cv", "mc_cv"))
  expect_equal(attr(cv, "n"), 20L)
  expect_equal(attr(cv, "minw"), 5L)
  expect_equal(attr(cv, "lag"), 2L)
  expect_equal(unname(cv$adf_cv), c(1, 2, 3))
  expect_equal(unname(cv$gsadf_cv), c(7, 8, 9))
  expect_equal(dim(cv$bsadf_cv), c(15, 3))
  expect_equal(unname(cv$bsadf_cv[2, ]), c(4, 5, 6))
  expect_equal(unname(cv$badf_cv[1, ]), c(-0.44, -0.08, 0.6))
})

test_that("fetch_crit_bucket errors on an unreachable store", {
  withr::local_envvar(R_USER_CACHE_DIR = withr::local_tempdir())
  expect_error(
    fetch_crit_bucket(9999, lag = 4, base_url = "http://127.0.0.1:9/crit2"),
    "Cannot reach the critical-value store"
  )
})

test_that("fetch_crit_bucket: live store serves lag 0 n > 600 and 404s to NULL", {
  skip_on_cran()
  skip_if_offline()
  withr::local_envvar(R_USER_CACHE_DIR = withr::local_tempdir())
  cv <- fetch_crit_bucket(700, lag = 0)
  expect_s3_class(cv, "mc_cv")
  expect_equal(attr(cv, "n"), 700L)
  expect_true(file.exists(crit_cache_path(700, 0)))
  expect_null(fetch_crit_bucket(4999, lag = 4))
})

test_that("fetch_crit_bucket serves a cached file without the network and memoises it", {
  withr::local_envvar(R_USER_CACHE_DIR = withr::local_tempdir())
  .pkgenv$crit <- list()
  withr::defer(.pkgenv$crit <- list())
  write_synthetic_crit(crit_cache_path(20, 2), n = 20L, minw = 5L, lag = 2L)
  cv <- fetch_crit_bucket(20, lag = 2, base_url = "http://127.0.0.1:9/crit2")
  expect_equal(attr(cv, "n"), 20L)
  expect_identical(.pkgenv$crit[["lag2-n20"]], cv)
  unlink(crit_cache_path(20, 2))
  # second call is served from memory even though the file is gone
  expect_identical(fetch_crit_bucket(20, lag = 2, base_url = "http://127.0.0.1:9/crit2"), cv)
})

test_that("fetch_crit_bucket discards a corrupt cache file and refetches", {
  withr::local_envvar(R_USER_CACHE_DIR = withr::local_tempdir())
  .pkgenv$crit <- list()
  withr::defer(.pkgenv$crit <- list())
  writeLines("not xz", crit_cache_path(21, 2))
  expect_error(
    fetch_crit_bucket(21, lag = 2, base_url = "http://127.0.0.1:9/crit2"),
    "Cannot reach the critical-value store"
  )
  expect_false(file.exists(crit_cache_path(21, 2)))
})
