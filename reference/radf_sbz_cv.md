# SBZ Weighted Least Squares Bubble Test with Union-of-Rejections

`radf_sbz_cv` performs the HLST (2016) wild bootstrap – the same
algorithm as
[`radf_wb_cv`](https://kvasilopoulos.github.io/exuber/reference/radf_wb_cv.md)
– *jointly* on the classic sup-ADF statistic (`supDF`, i.e.
[`radf()`](https://kvasilopoulos.github.io/exuber/reference/radf.md)'s
`sadf`) and the WLS/kernel-volatility statistic `supBZ` of Harvey,
Leybourne & Zu (2019), and combines them into the paper's
union-of-rejections statistic `U`. supBZ can have substantially higher
power than supDF under many time-varying-volatility patterns, at the
cost of lower power under others (e.g. upward volatility trends); `U` is
designed to capture whichever of the two is more powerful for a given
series.

## Usage

``` r
radf_sbz_cv(
  data,
  minw = NULL,
  nboot = 499L,
  kernel = c("gaussian", "uniform"),
  h = NULL,
  seed = NULL
)
```

## Arguments

- data:

  A univariate or multivariate numeric time series object, a numeric
  vector or matrix, or a data.frame. A column may have leading and/or
  trailing `NA` values (an uneven/unbalanced panel where series enter or
  exit the sample at different times) – those periods are filled with
  `NA` in `badf`/`bsadf` and excluded from that series' `adf`/`sadf`/
  `gsadf`. Interior `NA` values (a gap in the middle of a series) are
  not supported. When any series is padded this way, the panel statistic
  (`bsadf_panel`/`gsadf_panel`) is not available and is returned as
  `NA`, with a warning.

- minw:

  A positive integer. The minimum window size (default = \\(0.01 +
  1.8/\sqrt(T))T\\, where T denotes the sample size).

- nboot:

  A positive integer. Number of bootstraps (default = 500L).

- kernel:

  Kernel for the spot-volatility estimator (eq. 6), `"gaussian"`
  (default, as in the paper) or `"uniform"`.

- h:

  Bandwidth for the spot-volatility estimator. Default: leave-one-out
  cross-validation over the paper's own search range.

- seed:

  An object specifying if and how the random number generator (rng)
  should be initialized. Either NULL or an integer will be used in a
  call to `set.seed` before simulation. If set, the value is saved as
  "seed" attribute of the returned value. The default, NULL, will not
  change rng state, and return .Random.seed as the "seed" attribute.
  Results are reproducible across the parallel and non-parallel option
  when the same seed is used.

## Value

A list with bootstrap p-values (`p_supDF`, `p_supBZ`, `p_U`) and
critical values (`supDF_cv`, `supBZ_cv`, `U_cv`) for each series.

## Status

**\[experimental\]**

## References

Harvey, D. I., Leybourne, S. J., & Zu, Y. (2019). Testing explosive
bubbles with time-varying volatility. Econometric Reviews, 38(10),
1131-1151.

## See also

[`radf_wb_cv`](https://kvasilopoulos.github.io/exuber/reference/radf_wb_cv.md)
for the underlying (supDF-only) wild bootstrap, and
[`radf_tt`](https://kvasilopoulos.github.io/exuber/reference/radf_tt.md)
for a bootstrap-free heteroskedasticity-robust alternative.
