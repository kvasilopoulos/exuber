# Locally Best Invariant Test for a Bubble (Breitung & Diegel 2025)

`lbi_test` implements the static locally best invariant (LBI) test of
Breitung & Diegel (2025) for a bubble known (or assumed) to span the
entire sample: `LBI = (y_T - y_1) / (sigma_tilde * sqrt(T - 1))`, with
`sigma_tilde^2` the sample variance of first differences.
Heteroskedasticity-robust by construction (the statistic's invariance
property does not depend on the exact form of the innovation variance),
with a standard normal null distribution – no bootstrap, no simulation,
no published table.

## Usage

``` r
lbi_test(data, sig_lvl = 95)
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

- sig_lvl:

  Significance level for the (one-sided, right-tailed – positive bubbles
  only) test, on the package-wide 0-100 scale (default `95`); any value
  in `[50, 100)` is accepted since the critical value is a closed-form
  normal quantile.

## Value

An object of class `lbi_test_obj`: a list with the test statistic
`stat`, the standard-normal critical value `crit`, and `detected`
(logical, `stat > crit`).

## Details

Only the static (single, full-sample window) test is implemented.
Breitung & Diegel's own headline contribution is a sequential/
exponentially-weighted extension for monitoring an unknown start date,
whose exact weighting scheme and boundary constant are not pinned down
here and are not implemented.

## Note

The critical value is closed-form: the standard normal (`qnorm`)
quantile at `sig_lvl` – no bootstrap, no simulation, no table needed.

Returns its own class (not `radf_obj`), so it does not plug into
[`summary()`](https://rdrr.io/r/base/summary.html)/`\link{datestamp}`/`tidy`;
it has its own [`print()`](https://rdrr.io/r/base/print.html) and
[`autoplot()`](https://ggplot2.tidyverse.org/reference/autoplot.html)
methods instead. Prints its own statistic/critical-value/detected
summary – see
[`vignette("naming-and-analysis", package = "exuber")`](https://kvasilopoulos.github.io/exuber/articles/naming-and-analysis.md)
for the full picture of which functions do and don't fit that pipeline.

## Status

**\[experimental\]**

## References

Breitung, J., & Diegel, M. (2025). A locally best invariant sequential
test for explosive behavior in the presence of nonstationary volatility.
Journal of Time Series Analysis.

## See also

[`radf`](https://kvasilopoulos.github.io/exuber/reference/radf.md) for
the recursive ADF-family alternative this complements.

Other alternative tests:
[`quantile_test()`](https://kvasilopoulos.github.io/exuber/reference/quantile_test.md)

## Examples

``` r
# \donttest{
y <- sim_psy1(n = 60, te = 1, tf = 60, seed = 1) # explosive from the start
res <- lbi_test(y)
print(res)
#> 
#> ── lbi_test (n = 60, sig_lvl = 95%) ────────────────────────────────────────────
#> 
#>    series   stat   crit  detected
#>   series1  4.892  1.645      TRUE
#> 

# Compare the statistic to its critical value
autoplot(res)

# }
```
