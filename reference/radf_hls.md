# SSR/BIC Bubble Dating (Harvey, Leybourne & Sollis 2017)

`radf_hls` dates a single bubble episode by fitting four candidate
regime-dummy regressions of `Delta y_t` on `y_{t-1}` (unit-root-to-end,
unit-root-bubble-unit-root, unit-root-bubble-collapse, and
unit-root-bubble-collapse-unit-root), each by residual-sum-of-squares
minimisation over candidate break fractions, and selects among them by
BIC.

## Usage

``` r
radf_hls(data, trim = 0.05)
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

  Minimum fraction of the (differenced) sample required in every regime
  (default 0.05, following Harvey, Leybourne & Sollis's own
  empirical-application choice; their simulations use 0.1).

## Value

An object of class `radf_hls_obj`: a list with the selected model
(`model`, one of `1:4`), its breakpoint date(s) (`origination`,
`collapse`, `recovery` – `NA` for breakpoints the selected model doesn't
have), and the BIC value of every candidate model (`bic`, for inspecting
how close the selection was).

## Details

Unlike
[`datestamp`](https://kvasilopoulos.github.io/exuber/reference/datestamp.md)
(threshold-crossing on the recursive BSADF statistic) or
[`radf_pdc`](https://kvasilopoulos.github.io/exuber/reference/radf_pdc.md)
(a fixed 3/4-regime structure with sequentially, not jointly, estimated
breaks), this jointly searches breakpoints within each of four candidate
regime structures and lets BIC pick the structure itself – so it can
distinguish "bubble that collapses to a new stationary regime" (Model 3)
from "bubble that fully reverts to a unit root" (Model 4) from "bubble
ongoing at the sample end" (Model 1), which `radf_pdc`'s fixed regime
count cannot. The cost is a genuine joint grid search rather than
`radf_pdc`'s sequential one-break- at-a-time scan.

## Status

**\[experimental\]**

## References

Harvey, D. I., Leybourne, S. J., & Sollis, R. (2017). Improving the
accuracy of asset price bubble start and end date estimators. Journal of
Empirical Finance, 40, 121-138.

## See also

[`radf_pdc`](https://kvasilopoulos.github.io/exuber/reference/radf_pdc.md)
for the cheaper sequential-splitting alternative this complements, and
[`datestamp`](https://kvasilopoulos.github.io/exuber/reference/datestamp.md)
for PSY's original threshold-crossing rule.
