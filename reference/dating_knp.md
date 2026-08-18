# Bias-Corrected Single-Bubble Dating (Kejriwal, Nguyen & Perron 2025)

`dating_knp` dates a single bubble episode (origination, collapse) by
minimising a residual-omission-corrected sum of squared residuals over a
three-regime model (unit root, explosive, unit root resuming from a
shifted level after an instantaneous collapse). Plain OLS over this
model is provably inconsistent – the origination-date estimate converges
to the true *collapse* date, not the origination date – which
`omit = TRUE` (the default) fixes by dropping the single squared
residual at the candidate collapse date from the objective before
minimising.

## Usage

``` r
dating_knp(data, trim = 0.05, omit = TRUE)
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

- trim:

  Minimum fraction of the (differenced) sample required in each regime
  (default 0.05).

- omit:

  Use Kejriwal, Nguyen & Perron's consistency-restoring correction
  (default `TRUE`). `FALSE` gives the plain, provably inconsistent OLS
  estimator (their Theorem 1) – kept mainly to demonstrate the
  correction's effect, not for practical dating.

## Value

An object of class `dating_knp_obj`: a list with `origination`,
`collapse` (dates) and `delta` (the fitted explosive AR coefficient).

## Note

This is a residual-sum-of-squares model-selection dating procedure, not
a hypothesis test – it needs no critical values at all.

Returns its own class (not `radf_obj`), so it does not plug into
[`summary()`](https://rdrr.io/r/base/summary.html)/`\link{datestamp}`/`tidy`/`autoplot`
– prints its own dating table (model, origination, collapse, recovery) –
see
[`vignette("naming-and-analysis", package = "exuber")`](https://kvasilopoulos.github.io/exuber/articles/naming-and-analysis.md)
for the full picture of which functions do and don't fit that pipeline.

## Status

**\[experimental\]**

## References

Kejriwal, M., Nguyen, L., & Perron, P. (2025). An improved procedure for
retrospectively dating the emergence and collapse of bubbles. Journal of
Time Series Analysis, 46(5), 867-883.

## See also

[`dating_hls`](https://kvasilopoulos.github.io/exuber/reference/dating_hls.md),
[`dating_pdc`](https://kvasilopoulos.github.io/exuber/reference/dating_pdc.md)
for related SSR-based dating approaches.

## Examples

``` r
# \donttest{
res <- dating_knp(sim_data$psy1, trim = 0.05)
print(res)
#> 
#> ── dating_knp (n = 100, trim = 0.05, omit = TRUE) ──────────────────────────────
#> 
#>    series  origination  collapse  delta
#>   series1           41        55  0.964
#> 
# }
```
