# Wild Bootstrap Critical Values for the SBZ Statistic

`radf_sbz_cv` performs the HLST (2016) wild bootstrap – the same
algorithm as
[`radf_wb_cv`](https://kvasilopoulos.github.io/exuber/reference/radf_wb_cv.md),
applied to
[`radf_sbz`](https://kvasilopoulos.github.io/exuber/reference/radf_sbz.md)'s
WLS/kernel-volatility statistic instead of the classic `supDF` one – to
generate critical values, including the time-varying
`badf_cv`/`bsadf_cv` boundary
[`datestamp`](https://kvasilopoulos.github.io/exuber/reference/datestamp.md)/
`autoplot` need, not just the three scalar critical values
[`summary()`](https://rdrr.io/r/base/summary.html) uses.

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

  Kernel for the spot-volatility estimator (eq. 6 of Harvey, Leybourne &
  Zu 2019), `"gaussian"` (default, as in the paper) or `"uniform"`.

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

An object of class `radf_cv`/`sbz_cv`/`wb_cv`: a list with critical
values `adf_cv`, `sadf_cv`, `gsadf_cv` (one row per series) and
`badf_cv`, `bsadf_cv` (one array per series, one row per recursion
point).

## Status

**\[experimental\]**

## References

Harvey, D. I., Leybourne, S. J., & Zu, Y. (2019). Testing explosive
bubbles with time-varying volatility. Econometric Reviews, 38(10),
1131-1151.

## See also

[`radf_sbz`](https://kvasilopoulos.github.io/exuber/reference/radf_sbz.md)
for the statistic this pairs with, and
[`radf_sbz_union`](https://kvasilopoulos.github.io/exuber/reference/radf_sbz_union.md)
for the bundled union-of-rejections test against the classic `supDF`
statistic (not obtainable from this function and
[`radf_wb_cv`](https://kvasilopoulos.github.io/exuber/reference/radf_wb_cv.md)
independently – see that function's Details for why).
