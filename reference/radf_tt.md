# Time-Transformed Test for Explosive Bubbles under Non-stationary Volatility

`radf_tt` computes the STADF/GSTADF test statistics of Kurozumi,
Skrobotov & Tsarev, a heteroskedasticity-robust alternative to
[`radf`](https://kvasilopoulos.github.io/exuber/reference/radf.md) that
requires no bootstrap: the series is time-deformed using a nonparametric
estimate of its variance profile, after which the usual (asymptotic,
homoskedastic) recursive sup-ADF critical values apply.

## Usage

``` r
radf_tt(data, minw = NULL, kernel = c("uniform", "gaussian"), h = NULL)
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

- kernel:

  Kernel used in the local variance-profile regression, `"uniform"`
  (default, as in the paper's simulations) or `"gaussian"`.

- h:

  Bandwidth for the variance-profile kernel regression. Default
  `T^(-2/5)`, the midpoint (on the log scale) of the paper's
  cross-validation search range \\\[T^{-0.5}, T^{-0.3}\]\\.

## References

Kurozumi, E., Skrobotov, A., & Tsarev, A. (2024). Time-Transformed Test
for Bubbles under Non-stationary Volatility. Journal of Financial
Econometrics.
[doi:10.1093/jjfinec/nbae026](https://doi.org/10.1093/jjfinec/nbae026)

## See also

[`radf_tt_cv`](https://kvasilopoulos.github.io/exuber/reference/radf_tt_cv.md)
for the (pivotal, bootstrap-free) asymptotic critical values, and
[`radf_wb_cv`](https://kvasilopoulos.github.io/exuber/reference/radf_wb_cv.md)
for the bootstrap-based alternative (Harvey, Leybourne, Sollis &
Taylor).
