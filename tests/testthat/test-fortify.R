context("fortify")


test_that("basic",{
  fort <- radf_dta %>% fortify()
  expect_equal(col_names(fort), c("index", diagnostics(radf_dta)$accepted,"cv"))
  expect_equal(index(radf_dta, trunc = TRUE), fort$index )
})

test_that("select",{
  fort_1 <- radf_dta %>% fortify(select = 1)
  fort_dgp1 <- radf_dta %>% fortify(select = "dgp1")
  expect_equal(fort_1, fort_dgp1)
  expect_error(radf_dta %>% fortify(select = "div"), "subscript out of bounds")

})

test_that("panel",{
  fort_panel <- radf_dta %>% fortify(cv = sb)
  expect_equal(col_names(fort_panel), c("index", "Panel", "cv_panel"))
  expect_warning(radf_dta %>% fortify(cv = sb, include = TRUE),
         "argument 'include' is redundant")
  expect_warning(radf_dta %>% fortify(cv = sb, select = 1),
         "argument 'select' is redundant")
})

test_that("include = TRUE",{
  fort <- radf_dta %>% fortify(include = TRUE)
  expect_equal(colnames(fort), c("index", colnames(dta),"cv"))
  fort_1 <- radf_dta %>% fortify(include = TRUE, select = 1)
  expect_equal(col_names(fort_1), c("index", colnames(dta)[1],"cv"))

  #div is there
  fort_4 <- radf_dta %>% fortify(include = TRUE, select = 4)
  fort_div <- radf_dta %>% fortify(include = TRUE, select = "div")
  expect_equal(fort_4, fort_div)
  expect_equal(col_names(fort_4), c("index", colnames(dta)[4],"cv"))
})

test_that("wb-names",{
  fort_wb <- radf_dta %>% fortify(cv = wb, include = TRUE)
  cnames <- paste0("cv_", colnames(dta))
  expect_equal(colnames(fort_wb), c("index", colnames(dta), cnames))
})

test_that("lag potential issues",{

})

