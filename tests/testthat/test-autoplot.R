context("autoplot")

# skip("need new tests here")

p <- autoplot(radf_dta, cv = mc)

test_that("basic", {
  expect_error(p, NA)
  expect_s3_class(p, class = c("gg", "ggplot"))
  expect_equal(unique(p$data$index), index(radf_dta, trunc = TRUE))

  # Blanchard
  blan <- radf_dta %>% autoplot(select_series = "blan")
  expect_equal(blan$labels$title, "blan")
  expect_equal(blan$layers %>% length(), 2)

  # Dividends
  div <- radf_dta %>% autoplot(include_negative = TRUE, select_series = "div")
  expect_equal(div$labels$title, "div")
  expect_equal(div$layers %>% length(), 1) # no geom_rect
})

withr::with_options(
  c(warn = 2),
  test_that("wb", {
    expect_error(radf_dta %>% autoplot(cv = wb), regexp = NA)
    expect_error(radf_dta_lag1 %>% autoplot(cv = wb, option = "sadf"), NA)
  })
)

test_that("plot warnings & errors", {
  expect_error(autoplot(radf_div, mc), "Cannot reject H0")
})


test_that("panel", {
  expect_error(radf_dta %>% autoplot(cv = sb), NA)
  expect_error(radf_dta_lag1 %>% autoplot(cv = sb1), NA)
})


# Dates -------------------------------------------------------------------

test_that("dates", {
  dating <- seq(as.Date("1991/10/01"), by = "month", length.out = 100)
  index(radf_dta) <- dating
  p <- autoplot(radf_dta)
  expect_true(p$data$index %>% is.Date())
  expect_equal(unique(p$data$index), dating[-c(1:19)])

  pds <- radf_dta %>% datestamp() %>% autoplot()
  expect_true(pds$data$Start %>% is.Date())
})

