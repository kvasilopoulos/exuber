test_that("quantile_narm matches quantile() when there are no NAs", {
  x <- c(1, 2, 3, 4, 5)
  expect_equal(
    as.numeric(quantile_narm(x, probs = c(0.9, 0.95))),
    as.numeric(quantile(x, probs = c(0.9, 0.95)))
  )
})

test_that("quantile_narm drops NA/NaN and warns how many", {
  x <- c(1, 2, 3, 4, 5, NA, NaN)
  expect_warning(
    res <- quantile_narm(x, probs = c(0.9, 0.95)),
    "2 of 7"
  )
  expect_equal(
    as.numeric(res),
    as.numeric(quantile(x, probs = c(0.9, 0.95), na.rm = TRUE))
  )
})

test_that("quantile_narm returns NA (not an error) when everything is NA", {
  expect_warning(
    res <- quantile_narm(c(NA_real_, NaN), probs = c(0.9, 0.95)),
    "2 of 2"
  )
  expect_true(all(is.na(res)))
})
