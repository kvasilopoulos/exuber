test_that("exuber_functions returns a well-formed registry", {
  reg <- exuber_functions()
  expect_s3_class(reg, "tbl_df")
  expect_true(all(c("name", "family", "description") %in% names(reg)))
  expect_true(all(vapply(reg$name, exists, logical(1), where = asNamespace("exuber"))))
})

test_that("exuber_functions filters by family", {
  mon <- exuber_functions(family = "monitor")
  expect_true(all(grepl("monitor", mon$family)))
  expect_true("monitor" %in% mon$name)
  expect_true("monitor_cusum" %in% mon$name)

  expect_error(exuber_functions(family = "bogus"), "must be one of")
})
