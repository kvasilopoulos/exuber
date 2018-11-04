context("autoplot")

test_that("basic",{
  p <- autoplot(radf_dta)
  expect_error(p, NA)
  expect_equal(p$dgp1$labels$title, "dgp1")
  expect_equal(p$dgp1$data, fortify(radf_dta, select = "dgp1"))
  expect_equal(p$dgp1$layers[[3]]$data, datestamp(radf_dta)[[1]][,-3])
  parr <- ggarrange(p)
})


withr::with_options(
  c(warn = 2),
  test_that("no problem running (date, lag, wb)", {

    p <- autoplot(radf_dta_lag1, wb)
    expect_error(autoplot(radf_dta_lag1, wb), regexp = NA)
    expect_error(autoplot(radf_dta_lag1, wb, option = "sadf"))
  })
)

test_that("plot warnings & errors",{
  expect_error(autoplot(radf_div, mc),
    "Cannot reject H0")
})

test_that("include",{

  radf_dta %>% autoplot(include = FALSE) %>% ggarrange()
  radf_dta %>% autoplot(include = FALSE, select = "blan")
  radf_dta %>% autoplot(include = TRUE) %>% ggarrange()
  radf_dta %>% autoplot(include = TRUE, select = "div")
  radf_dta %>% autoplot(cv = sb)
  radf_dta %>% autoplot(cv = sb, include = TRUE)
  radf_dta %>% autoplot(cv = sb, select = 1)
  radf_dta %>% autoplot(cv = sb, include = TRUE, select = 1)
  radf_dta_lag1 %>% fortify(cv = sb1)
})




# Dates -------------------------------------------------------------------

dating <- seq(as.Date("1991/10/01"), by = "month", length.out = 100)
index(radf_dta) <- dating

# Panel -------------------------------------------------------------------
