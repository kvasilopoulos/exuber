# Quantile Unit Root Test for Bubble Detection (Global Test)

`radf_quantile` implements the "global test" of Wu, Shi & Wu (2025): a
quantile-regression (QR) analogue of the Dickey-Fuller t-ratio, testing
for explosive behavior at a chosen conditional quantile `tau` of `y_t`
on `y_{t-1}` rather than at the conditional mean. A single static test,
not a recursive scan (compare
[`radf`](https://kvasilopoulos.github.io/exuber/reference/radf.md)'s
single-shot `adf` statistic, not its recursive `bsadf`).

## Usage

``` r
radf_quantile(
  data,
  tau = "optimal",
  tau_grid = seq(0.2, 0.8, by = 0.05),
  nrep = 1000L,
  level = 95,
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

- tau:

  Quantile to test at, in `(0, 1)`, or `"optimal"` (default) to select
  it via eq. 33's grid search.

- tau_grid:

  Grid searched when `tau = "optimal"`. Default
  `seq(0.2, 0.8, by = 0.05)`, matching the paper's own recommended
  practical range (excluding the extreme quantiles 0.1/0.9).

- nrep:

  Number of Monte Carlo replications for the critical value.

- level:

  Significance level, one of `90`, `95`, `99`.

- seed:

  Optional seed for the Monte Carlo draws.

## Value

An object of class `radf_quantile_obj`: a list with the test statistic
`tstat`, the selected `tau`, the estimated correlation `delta`, the
simulated `crit` value, and `detected` (logical, `tstat > crit`).

## Details

`tau = "optimal"` (the default) selects the quantile minimizing the
asymptotic variance of the QR estimator (their eq. 33) by grid search
over `tau_grid`, excluding the extreme quantiles the paper itself
recommends avoiding at practical sample sizes.

The critical value is simulated per call (not a fixed table): the
statistic's limiting null distribution is
`sqrt(1 - delta^2) * z + delta * Q`, with `z ~ N(0, 1)` and `delta` a
data-estimated correlation coefficient; `Q` is the standard demeaned
Dickey-Fuller t-statistic distribution, simulated by the same
random-walk-plus-OLS-t-stat construction used elsewhere in this package
(see
[`radf_mc_cv`](https://kvasilopoulos.github.io/exuber/reference/radf_mc_cv.md)).

## Status

**\[experimental\]**

## References

Wu, R., Shi, S., & Wu, J. (2025). Quantile analysis for financial bubble
detection and surveillance. Journal of Time Series Analysis, 46(5),
908-931.

## See also

[`radf`](https://kvasilopoulos.github.io/exuber/reference/radf.md) for
the mean-regression (ADF/SADF/GSADF) family this complements.
