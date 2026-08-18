# Multi-Bubble SSR/BIC Dating (Harvey, Leybourne & Whitehouse 2020)

`dating_hlw` extends
[`dating_hls`](https://kvasilopoulos.github.io/exuber/reference/dating_hls.md)
to series with more than one explosive episode: it first runs PSY's
existing detection and dating
([`radf`](https://kvasilopoulos.github.io/exuber/reference/radf.md)/[`datestamp`](https://kvasilopoulos.github.io/exuber/reference/datestamp.md))
to locate a preliminary start/end for each episode, splits the sample
into disjoint date windows around them, then re-dates each window with
[`dating_hls`](https://kvasilopoulos.github.io/exuber/reference/dating_hls.md)-style
SSR/BIC fitting (restricted to Models 2 and 4 for every window but the
last).

## Usage

``` r
dating_hlw(
  data,
  cv = NULL,
  minw = NULL,
  trim = 0.1,
  min_duration = NULL,
  nboot = 199L,
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

- cv:

  Critical values for the step-1 PSY detection/dating step, as accepted
  by
  [`datestamp`](https://kvasilopoulos.github.io/exuber/reference/datestamp.md).
  Default `NULL` computes
  [`radf_wb_cv`](https://kvasilopoulos.github.io/exuber/reference/radf_wb_cv.md)
  internally.

- minw:

  Minimum window size for the step-1
  [`radf`](https://kvasilopoulos.github.io/exuber/reference/radf.md)
  call. Default
  [`psy_minw`](https://kvasilopoulos.github.io/exuber/reference/psy_minw.md).

- trim:

  Minimum fraction of the (differenced) sample required in every regime
  (default 0.05, following Harvey, Leybourne & Sollis's own
  empirical-application choice; their simulations use 0.1).

- min_duration:

  Minimum duration (in observations) for a step-1 PSY episode to be
  counted. Default
  [`psy_ds`](https://kvasilopoulos.github.io/exuber/reference/psy_minw.md)
  (HLW's own \\\ln(T)\\ rule).

- nboot, seed:

  Passed to
  [`radf_wb_cv`](https://kvasilopoulos.github.io/exuber/reference/radf_wb_cv.md)
  when `cv` is not supplied.

## Value

An object of class `dating_hlw_obj`: a list, one element per series,
each a data frame with one row per detected episode (`model`,
`origination`, `collapse`, `recovery`). A series with no step-1 detected
episode gets a zero-row data frame.

## Details

When exactly one episode is detected, this reduces to
[`dating_hls`](https://kvasilopoulos.github.io/exuber/reference/dating_hls.md)
applied to the whole series – the paper's own stated property, since the
single window then runs `[1, n]` and fits all four models.

## Note

The step-2 SSR/BIC dating within each window needs no critical values at
all, same as
[`dating_hls`](https://kvasilopoulos.github.io/exuber/reference/dating_hls.md).
The step-1 PSY detection/dating pass does use a wild bootstrap critical
value (`cv`/`nboot`/`seed` below), but only to locate the preliminary
episode windows, not for the dating step itself.

Returns its own class (not `radf_obj`), so it does not plug into
[`summary()`](https://rdrr.io/r/base/summary.html)/`\link{datestamp}`/`tidy`/`autoplot`
– prints its own dating table (model, origination, collapse, recovery) –
see
[`vignette("naming-and-analysis", package = "exuber")`](https://kvasilopoulos.github.io/exuber/articles/naming-and-analysis.md)
for the full picture of which functions do and don't fit that pipeline.

## Status

**\[experimental\]**

## References

Harvey, D. I., Leybourne, S. J., & Whitehouse, E. J. (2020).
Date-stamping multiple bubble regimes. Journal of Empirical Finance, 58,
226-246.

## See also

[`dating_hls`](https://kvasilopoulos.github.io/exuber/reference/dating_hls.md)
for the single-bubble fitting this wraps, and
[`datestamp`](https://kvasilopoulos.github.io/exuber/reference/datestamp.md)
for PSY's own multi-bubble threshold-crossing dating.

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
# }
```
