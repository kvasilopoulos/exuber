context("autoplot")

skip("need new tests here")

p <- autoplot(radf_dta)

test_that("basic", {
  expect_s3_class(p, class = c("gg", "ggplot"))
  expect_error(p, NA)
  expect_equal(p$labels$title, "psy1")
  expect_equal(unique(p$data$index), index(radf_dta, trunc = TRUE))
  # expect_equal(p$data, augment_join(radf_dta, mc))
  expect_equal(p$layers[[2]]$data, tidy(datestamp(radf_dta)))

  # Blanchard
  blan <- radf_dta %>% autoplot(include = FALSE, select = "blan")
  expect_equal(blan$labels$title, "blan")
  expect_equal(blan$layers %>% length(), 3)
  # Dividends
  div <- radf_dta %>% autoplot(include = TRUE, select = "div")
  expect_equal(div$labels$title, "div")
  expect_equal(div$layers %>% length(), 2)
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

test_that("ggarrange", {
  expect_equal(radf_dta %>%
    autoplot(include = FALSE, arrange = FALSE) %>%
    ggarrange() %>%
    pluck("layout") %>%
    NROW(), 4)
  expect_equal(radf_dta %>%
    autoplot(include = TRUE, arrange = FALSE) %>%
    ggarrange() %>%
    pluck("layout") %>%
    NROW(), 5)
  expect_error(parr, regexp = NA)
})

test_that("panel", {
  expect_error(radf_dta %>% autoplot(cv = sb), NA)
  expect_error(radf_dta_lag1 %>% autoplot(cv = sb1), NA)
  expect_warning(radf_dta %>% autoplot(cv = sb, include = TRUE), warn_include)
  expect_warning(radf_dta %>% autoplot(cv = sb, select = 1), warn_select)

  w <- capture_warnings(radf_dta %>%
    fortify(cv = sb, include = TRUE, select = 1))
  expect_match(w[1], warn_select)
  expect_match(w[2], warn_include)
})


# Dates -------------------------------------------------------------------


test_that("dates", {
  dating <- seq(as.Date("1991/10/01"), by = "month", length.out = 100)
  index(radf_dta) <- dating
  p <- radf_dta %>% autoplot(arrange = FALSE)
  expect_equal(p$psy1$data$index, dating[-c(1:19)])
  expect_true(p$psy1$data$index %>% is.Date())

  pds <- radf_dta %>% datestamp() %>% autoplot()
  expect_true(pds$data$Start %>% is.Date())
})

