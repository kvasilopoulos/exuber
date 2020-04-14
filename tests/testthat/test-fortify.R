context("fortify")

skip("Removed fortify")

test_that("basic", {
  fort <- radf_dta %>% fortify()
  expect_equal(col_names(fort), c("index", diagnostics(radf_dta)$accepted, "cv"))
  expect_equal(index(radf_dta, trunc = TRUE), fort$index)
})

test_that("select", {
  fort_1 <- radf_dta %>% fortify(select = 1)
  fort_dgp1 <- radf_dta %>% fortify(select = "psy1")
  expect_equal(fort_1, fort_dgp1)
  expect_error(radf_dta %>% fortify(select = "div"), "subscript out of bounds")
})

test_that("panel", {
  fort_panel <- radf_dta %>% fortify(cv = sb)
  expect_equal(col_names(fort_panel), c("index", "Panel", "cv_panel"))
  expect_warning(radf_dta %>% fortify(cv = sb, include = TRUE), warn_include)
  expect_warning(radf_dta %>% fortify(cv = sb, select = 1), warn_select)

  w <- capture_warnings(radf_dta %>%
    fortify(cv = sb, include = TRUE, select = 1))
  expect_match(w[1], warn_select)
  expect_match(w[2], warn_include)
})

test_that("include = TRUE & select = 'div'", {
  fort <- radf_dta %>% fortify(include = TRUE)
  expect_equal(colnames(fort), c("index", colnames(dta), "cv"))
  fort_1 <- radf_dta %>% fortify(include = TRUE, select = 1)
  expect_equal(col_names(fort_1), c("index", colnames(dta)[1], "cv"))

  # div is there
  fort_4 <- radf_dta %>% fortify(include = TRUE, select = 4)
  fort_div <- radf_dta %>% fortify(include = TRUE, select = "div")
  expect_equal(fort_4, fort_div)
  expect_equal(col_names(fort_4), c("index", colnames(dta)[4], "cv"))
})

test_that("wb-names", {
  fort_wb <- radf_dta %>% fortify(cv = wb, include = TRUE)
  cnames <- paste0("cv_", colnames(dta))
  expect_equal(colnames(fort_wb), c("index", colnames(dta), cnames))
})

test_that("sb - lag potential issues", {
  expect_error(radf_dta %>% fortify(cv = sb), NA)
  expect_error(radf_dta_lag1 %>% fortify(cv = sb1), NA)
  # expect_error(radf_dta_lag2 %>% fortify(cv = sb2), NA)

  expect_equal(radf_dta %>% fortify(cv = sb) %>% NROW(), 81)
  # First jump 3 when you add lag = 1, and then progress by 1
  expect_equal(radf_dta_lag1 %>% fortify(cv = sb1) %>% NROW(), 78)
  # expect_equal(radf_dta_lag2 %>% fortify(cv = sb2) %>% NROW(), 77)
})

# Fortify datestamp -------------------------------------------------------

test_that("fortify- datestamp", {
  fort <- radf_dta %>% datestamp() %>% fortify()
  fort_panel <- radf_dta %>% datestamp(cv = sb) %>% fortify()

  expect_equal(fort %>% dim(), c(8, 4))
  expect_equal(radf_dta %>%
    datestamp(cv = wb) %>%
    fortify() %>%
    dim(), c(4, 4))
  expect_equal(fort_panel %>% dim(), c(3, 4))
  expect_equal(rbind(fort, fort_panel) %>% dim(), c(11, 4))

  # Rest of the argument
  expect_equal(radf_dta %>%
    datestamp(min_duration = 10) %>%
    fortify() %>%
    dim(), c(2, 4))
  expect_equal(radf_dta %>%
    datestamp(option = "sadf") %>%
    fortify() %>%
    dim(), c(3, 4))
  expect_equal(radf_dta %>%
    datestamp(option = "sadf", min_duration = 10) %>%
    fortify() %>%
    dim(), c(2, 4))
})
