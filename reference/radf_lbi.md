# Locally Best Invariant Test for a Bubble (Breitung & Diegel 2025)

`radf_lbi` implements the static locally best invariant (LBI) test of
Breitung & Diegel (2025) for a bubble known (or assumed) to span the
entire sample: `LBI = (y_T - y_1) / (sigma_tilde * sqrt(T - 1))`, with
`sigma_tilde^2` the sample variance of first differences.
Heteroskedasticity-robust by construction (the statistic's invariance
property does not depend on the exact form of the innovation variance),
with a standard normal null distribution – no bootstrap, no simulation,
no published table.

## Usage

``` r
radf_lbi(data, level = 0.95)
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

- level:

  Nominal confidence level for the (one-sided, right-tailed – positive
  bubbles only) test (default `0.95`).

## Value

An object of class `radf_lbi_obj`: a list with the test statistic
`stat`, the standard-normal critical value `crit`, and `detected`
(logical, `stat > crit`).

## Details

Only the static (single, full-sample window) test is implemented.
Breitung & Diegel's own headline contribution is a sequential/
exponentially-weighted extension for monitoring an unknown start date,
whose exact weighting scheme and boundary constant are not pinned down
here and are not implemented.

## Status

**\[experimental\]**

## References

Breitung, J., & Diegel, M. (2025). A locally best invariant sequential
test for explosive behavior in the presence of nonstationary volatility.
Journal of Time Series Analysis.

## See also

[`radf`](https://kvasilopoulos.github.io/exuber/reference/radf.md) for
the recursive ADF-family alternative this complements.
