# Stochastic Unit Root Bubble Test (Kurozumi & Nishi 2025)

`radf_ssu` implements the SSU statistic of Kurozumi & Nishi (2025): a
sup-type test for a bubble based on testing for a stochastic (rather
than deterministic) unit root in the *squared* first differences,
`(Delta y_t)^2 = mu2 + omega*y_{t-1}^2 + eta_t`, bias-corrected against
its dependence on the correlation between this regression's and the
plain ADF regression's innovations.

## Usage

``` r
radf_ssu(data, minw = NULL, level = 0.95)
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

- level:

  Nominal confidence level, one of `0.90`, `0.95`, `0.99` (the levels
  Kurozumi & Nishi's Table I tabulates).

## Value

An object of class `radf_ssu_obj`: a list with the statistic path
(`stat`, one value per candidate end point from `minw` to `n`), the
constant `crit` from Table I, and `sadf` (the maximum, compared against
`crit`) and `detected`.

## Details

A different generalization from the rest of exuber's volatility
-robustness tests: it doesn't touch the innovation variance at all, but
instead allows the explosive AR coefficient itself to vary
stochastically over time, `1 + c1/T + a*u_t/sqrt(T)`, rather than the
deterministic `1 + c/T^alpha` every recursive-ADF-family statistic in
this package assumes.

Only the single-recursion `SSU` statistic (sup over the end point, start
fixed at the beginning of the sample) is implemented – not `GSSU` (the
double-recursion generalization), the paper's separate CUSUM/CUSUM-SQ
statistics, or the union-of-rejections procedure combining SSU/GSSU with
SADF/GSADF.

## Status

**\[experimental\]**

## References

Kurozumi, E., & Nishi, M. (2025). Bubble testing with stochastically
varying explosive coefficient. Journal of Time Series Analysis, 46(5),
945-965.

## See also

[`radf`](https://kvasilopoulos.github.io/exuber/reference/radf.md) for
the deterministic-coefficient recursive ADF-family alternative this
complements.
