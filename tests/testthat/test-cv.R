context("cv")

test_that("exuberdata::crit as data", {
  skip_on_cran()
  skip_if(!"exuberdata" %in% loadedNamespaces())
  # crit <- exuberdata::crit
  # expect_error(capture.output(print(crit)), NA)
  # expect_error(crit, NA)
  # expect_error(crit[[100]], NA)
  # expect_error(radf_dta %>% retrieve_crit(), NA)
  # expect_error(crit[[2001]], "subscript out of bounds")
  # msg_crit <- "cannot provide MC critical values see help(crit)"
  # expect_error(sim_blan(4) %>% retrieve_crit(), msg_crit, fixed = TRUE)
  # expect_error(sim_blan(2001) %>% retrieve_crit(), msg_crit, fixed = TRUE)
  # expect_error(sim_blan(2001) %>% radf(minw = 2000) %>% retrieve_crit(),
  #   msg_crit,
  #   fixed = TRUE
  # )
})

test_that("n positive integer", {
  msg <- "Argument 'n' should be a positive integer"
  expect_error(radf_mc_cv(dta, minw = 0), msg)
  expect_error(radf_mc_cv(0, minw = 0), msg)
  expect_error(radf_mc_cv(-1, minw = 0), msg)
})

test_that("nboot positive integer", {
  msg <- "Argument 'nboot' should be a positive integer"
  expect_error(radf_sb_cv(dta, nboot = 0), msg)
  expect_error(radf_sb_cv(dta, nboot = -2), msg)
  expect_error(radf_wb_cv(dta, nboot = 0), msg)
  expect_error(radf_wb_cv(dta, nboot = -2), msg)
})

test_that("minw positive integer", {
  msg <- "Argument 'minw' should be a positive integer"
  expect_error(radf_mc_cv(100, minw = -1), msg)
  expect_error(radf_mc_cv(100, minw = 0), msg)

  expect_error(radf_wb_cv(dta, minw = -1), msg)
  expect_error(radf_wb_cv(dta, minw = 0), msg)
  expect_error(radf_sb_cv(dta, minw = -1), msg)
  expect_error(radf_sb_cv(dta, minw = 0), msg)
})

test_that("n/nboot/minw too small", {
  msg_n <- "Argument 'n' should be greater than '5'"
  msg_minw <- "Argument 'minw' should be greater than '2'"
  msg_nboot <- "Argument 'nboot' should be greater than '2'"

  expect_error(radf_mc_cv(2), msg_n)

  expect_error(radf_mc_cv(100, minw = 2), msg_minw)
  expect_error(radf_wb_cv(dta, minw = 2), msg_minw)
  expect_error(radf_sb_cv(dta, minw = 2), msg_minw)

  expect_error(radf_wb_cv(dta, nboot = 2), msg_nboot)
  expect_error(radf_sb_cv(dta, nboot = 2), msg_nboot)
})

test_that("minw too small", {
  msg <- "Argument 'minw' should be greater than '2'"
  expect_error(radf_mc_cv(100, minw = 2), msg)
  expect_error(radf_wb_cv(dta, minw = 2), msg)
  expect_error(radf_sb_cv(dta, minw = 2), msg)
})

test_that("NA handling", {
  msg <- "rls estimation cannot handle NA"
  expect_error(radf_wb_cv(dta_na), msg)
  expect_error(radf_sb_cv(dta_na), msg)
})

test_that("distribution_rad works", {
  expect_error(invisible(capture.output(
    radf_wb_cv(dta, nboot = 10, dist_rad = TRUE)
  )), regexp = NA)
})


# test_that("show_progress", {
#   options(exuber.show_progress = TRUE)
#   expect_error(capture.output(mc_cv(100, nrep = 10)), NA)
#   expect_error(capture.output(wb_cv(dta, nboot = 10)), NA)
#   expect_error(capture.output(sb_cv(dta, nboot = 10)), NA)
#   options(exuber.show_progress = FALSE)
# })

# test_that("parallel-ncores arguments",{
#   msg <- "Argument 'ncores' is redundant"
#   expect_warning(
#     invisible(capture.output(
#       mc_cv(100, nrep = 10, parallel = FALSE, ncores = 3))), msg)
#   expect_warning(
#     invisible(capture.output(
#       wb_cv(dta, nboot = 10, parallel = FALSE, ncores = 3))), msg)
#   expect_warning(
#     invisible(capture.output(
#       wb_cv(dta, nboot = 10, parallel = FALSE, ncores = 3))), msg)
# })


# with_parallel <- function(code) {
#   skip_on_cran()
#   doParallel::registerDoParallel(cores = 2)
#   on.exit(doParallel::stopImplicitCluster())
#   code
# }

# test_that("parallel works", {
#   skip_on_travis()
#   skip_on_cran()
#   with_parallel({
#   expect_error(invisible(capture.output(mc_cv(100, 12, parallel = TRUE))),
#     regexp = NA)
#   expect_error(invisible(capture.output(wb_cv(dta, 12, parallel = TRUE))),
#     regexp = NA)
#   expect_error(
#     invisible(capture.output(
#       wb_cv(dta, 12, parallel = TRUE, dist_rad = TRUE))), regexp = NA)
#   })
# })
