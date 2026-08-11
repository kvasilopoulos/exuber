context("uneven panel (NA padding)")

set.seed(42)
n <- 100
a_core <- sim_psy1(n - 5, te = 40, tf = 60)
dta_uneven <- data.frame(
  a = c(rep(NA, 5), a_core),
  b = sim_psy1(n)
)

test_that("radf() accepts leading/trailing NA and NA-pads badf/bsadf", {
  expect_warning(r <- radf(dta_uneven, minw = 20), "panel statistic")

  expect_true(all(is.na(r$badf[1:5, "a"])))
  expect_false(anyNA(r$badf[6:nrow(r$badf), "a"]))
  expect_false(anyNA(r$badf[, "b"]))

  # padded series matches radf() run on its own trimmed subsequence (same
  # minw), at the correct offset
  r_trim <- radf(data.frame(a = a_core), minw = 20)
  offset <- 5L
  n_local <- nrow(r_trim$badf)
  expect_equal(
    as.numeric(r$badf[offset + seq_len(n_local), "a"]),
    as.numeric(r_trim$badf[, 1])
  )
  expect_equal(unname(r$adf["a"]), unname(r_trim$adf))
  expect_equal(unname(r$gsadf["a"]), unname(r_trim$gsadf))

  # panel statistic is unavailable for an uneven panel
  expect_true(all(is.na(r$bsadf_panel)))
  expect_true(is.na(r$gsadf_panel))
})

test_that("radf() still rejects an interior NA", {
  dta_interior <- dta_uneven
  dta_interior$b[50] <- NA
  expect_error(radf(dta_interior), "interior NA")
})

test_that("regular (fully populated) data is unaffected", {
  r <- radf(dta)
  expect_false(anyNA(r$badf))
  expect_false(anyNA(r$bsadf_panel))
  expect_equal(r$bsadf_panel, apply(r$bsadf, 1, mean))
  vr <- get_valid_range(r)
  expect_true(all(vr["start", ] == 1))
  expect_true(all(vr["end", ] == nrow(dta)))
})

test_that("autoplot shades the NA-padded region", {
  r <- suppressWarnings(radf(dta_uneven))

  rects_padded <- na_pad_rects(get_valid_range(r), index(r, trunc = FALSE), "a")
  expect_false(is.null(rects_padded))
  expect_equal(nrow(rects_padded), 1L) # leading pad only, no trailing pad
  expect_equal(rects_padded$xmax, 5)

  rects_unpadded <- na_pad_rects(get_valid_range(r), index(r, trunc = FALSE), "b")
  expect_null(rects_unpadded)

  cv <- radf_mc_cv(n, nrep = 200)
  p_padded <- autoplot(r, cv, select_series = "a", nonrejected = TRUE)
  is_rect <- function(p) vapply(p$layers, function(l) inherits(l$geom, "GeomRect"), logical(1))
  expect_gt(sum(is_rect(p_padded)), 0)
})

test_that("datestamp's dummy attribute is NA over the padded region", {
  r <- suppressWarnings(radf(dta_uneven))
  cv <- radf_mc_cv(n, nrep = 200)
  ds <- suppressWarnings(datestamp(r, cv, nonrejected = TRUE))

  dummy <- attr(ds, "dummy")
  expect_true("a" %in% colnames(dummy))
  expect_true(all(is.na(dummy[1:5, "a"])))
  expect_false(anyNA(dummy[6:nrow(dummy), "a"]))
})
