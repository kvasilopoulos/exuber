# Multi-Bubble SSR/BIC Dating (Harvey, Leybourne & Whitehouse 2020)

`dating_hlw` extends
[`dating_hls`](https://kvasilopoulos.github.io/exuber/reference/dating_hls.md)
to series with more than one explosive episode. It first runs the
existing detection and dating of PSY
([`radf`](https://kvasilopoulos.github.io/exuber/reference/radf.md) and
[`datestamp`](https://kvasilopoulos.github.io/exuber/reference/datestamp.md))
to locate a preliminary start and end for each episode. It then splits
the sample into disjoint date windows around them and dates each window
again with the SSR/BIC fitting of
[`dating_hls`](https://kvasilopoulos.github.io/exuber/reference/dating_hls.md),
restricted to Models 2 and 4 for every window except the last.

## Usage

``` r
dating_hlw(
  data,
  cv = NULL,
  minw = NULL,
  trim = 0.1,
  min_duration = NULL,
  nboot = 199L,
  seed = NULL,
  join = 3L
)
```

## Arguments

- data:

  A univariate or multivariate numeric time series object, a numeric
  vector or matrix, or a data.frame. A column may have leading or
  trailing `NA` values, which describes an unbalanced panel in which
  series enter or exit the sample at different times. Those periods are
  filled with `NA` in `badf` and `bsadf` and excluded from the `adf`,
  `sadf` and `gsadf` of that series. Interior `NA` values (a gap in the
  middle of a series) are not supported. When any series is padded in
  this way, the panel statistics (`bsadf_panel` and `gsadf_panel`) are
  not available, and the function returns `NA` for them with a warning.

- cv:

  Critical values for the step-1 PSY detection and dating, as accepted
  by
  [`datestamp`](https://kvasilopoulos.github.io/exuber/reference/datestamp.md).
  The default `NULL` computes
  [`radf_wb_cv`](https://kvasilopoulos.github.io/exuber/reference/radf_wb_cv.md)
  internally.

- minw:

  Minimum window size for the step-1
  [`radf`](https://kvasilopoulos.github.io/exuber/reference/radf.md)
  call. The default is
  [`psy_minw`](https://kvasilopoulos.github.io/exuber/reference/psy_minw.md).

- trim:

  Minimum fraction of the (differenced) sample required in every regime
  (default 0.05, following the choice of Harvey, Leybourne & Sollis in
  their empirical application. Their simulations use 0.1).

- min_duration:

  Minimum duration (in observations) for a step-1 PSY episode to be
  counted. The default is
  [`psy_ds`](https://kvasilopoulos.github.io/exuber/reference/psy_minw.md)
  (the \\\ln(T)\\ rule of HLW).

- nboot, seed:

  Passed to
  [`radf_wb_cv`](https://kvasilopoulos.github.io/exuber/reference/radf_wb_cv.md)
  when `cv` is not supplied.

- join:

  The run-joining rule of HLW for fragmented step-1 detections. Two
  explosive runs that are separated by at most `join` non-rejections and
  are each at least \\\ln(T)\\ long are treated as one episode. The
  default is 3, the value in the paper, and `0` disables joining.

## Value

An object of class `dating_hlw_obj`: a list with one element for each
series. Each element is a data frame with one row for each detected
episode (`model`, `origination`, `collapse`, `recovery`). A series with
no step-1 detected episode gets a data frame with zero rows.

## Details

When exactly one episode is detected, the function reduces to
[`dating_hls`](https://kvasilopoulos.github.io/exuber/reference/dating_hls.md)
applied to the whole series, which is a stated property of the paper.
The single window then runs over `[1, n]` and fits all four models.

## Note

The step-2 SSR/BIC dating within each window needs no critical values,
as in
[`dating_hls`](https://kvasilopoulos.github.io/exuber/reference/dating_hls.md).
The step-1 PSY detection and dating pass does use a wild bootstrap
critical value (`cv`, `nboot` and `seed` below), but only to locate the
preliminary episode windows and not for the dating step.

The function returns its own class and not `radf_obj`, so it does not
work with [`summary()`](https://rdrr.io/r/base/summary.html),
`\link{datestamp}` and `tidy`. It has its own
[`print()`](https://rdrr.io/r/base/print.html) and
[`autoplot()`](https://ggplot2.tidyverse.org/reference/autoplot.html)
methods instead. [`print()`](https://rdrr.io/r/base/print.html) shows
the dating table (model, origination, collapse, recovery). See
[`vignette("naming-and-analysis", package = "exuber")`](https://kvasilopoulos.github.io/exuber/articles/naming-and-analysis.md)
for which functions fit the shared pipeline and which do not.

## Status

**\[experimental\]**

## References

Harvey, D. I., Leybourne, S. J., & Whitehouse, E. J. (2020).
Date-stamping multiple bubble regimes. Journal of Empirical Finance, 58,
226-246.

## See also

[`dating_hls`](https://kvasilopoulos.github.io/exuber/reference/dating_hls.md)
for the single-bubble fitting that this function wraps, and
[`datestamp`](https://kvasilopoulos.github.io/exuber/reference/datestamp.md)
for the multi-bubble threshold-crossing dating of PSY.

Other dating:
[`dating_hls()`](https://kvasilopoulos.github.io/exuber/reference/dating_hls.md),
[`dating_knp()`](https://kvasilopoulos.github.io/exuber/reference/dating_knp.md),
[`dating_pdc()`](https://kvasilopoulos.github.io/exuber/reference/dating_pdc.md),
[`radf_recovery()`](https://kvasilopoulos.github.io/exuber/reference/radf_recovery.md),
[`rootstamp()`](https://kvasilopoulos.github.io/exuber/reference/rootstamp.md)

## Examples

``` r
# \donttest{
res <- dating_hlw(sim_data$psy1, trim = 0.1, nboot = 199L, seed = 1)
print(res)
#> 
#> ── dating_hlw (n = 100, trim = 0.1) ────────────────────────────────────────────
#> 
#> series1:
#>  model origination collapse recovery
#>      4          41       55       71
#> 

# Plot the breakpoints of every detected episode over the series
autoplot(res)


# A two-bubble series, for which dating_hls() alone would fit only one bubble
res2 <- dating_hlw(sim_psy2(n = 200, seed = 123), trim = 0.1, nboot = 199L, seed = 1)
autoplot(res2)

# }
```
