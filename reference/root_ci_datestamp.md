# Root Confidence Intervals for Every Datestamped Episode

Convenience wrapper that runs
[`explosive_root`](https://kvasilopoulos.github.io/exuber/reference/explosive_root.md)/[`root_ci`](https://kvasilopoulos.github.io/exuber/reference/root_ci.md)
on every episode in a
[`datestamp`](https://kvasilopoulos.github.io/exuber/reference/datestamp.md)
result, so root inference doesn't have to be hand-run per episode.

## Usage

``` r
root_ci_datestamp(object, ds, level = 0.95, type = c("normal", "cauchy"))
```

## Arguments

- object:

  A `radf_obj` (or subclass) that `ds` was computed on – needs the
  original data, retrieved via its `"mat"` attribute.

- ds:

  A
  [`datestamp`](https://kvasilopoulos.github.io/exuber/reference/datestamp.md)
  result computed on `object`. Set `min_duration` there to exclude
  episodes too short for reliable root inference.

- level:

  Confidence level, passed to
  [`root_ci`](https://kvasilopoulos.github.io/exuber/reference/root_ci.md)
  (default 0.95).

- type:

  CI type, passed to
  [`root_ci`](https://kvasilopoulos.github.io/exuber/reference/root_ci.md)
  (default `"normal"`).

## Value

A named list (one element per series in `ds`; the panel sieve-bootstrap
case, whose `ds` entry is named `"panel"` and has no single
corresponding series, is dropped with a warning), each a data frame with
one row per datestamped episode: `Start`, `End`, `rho`, `rho_lower`,
`rho_upper`, `doubling_time`, `doubling_time_lower`,
`doubling_time_upper`.

## Details

**Not** folded into
[`summary.radf_obj`](https://kvasilopoulos.github.io/exuber/reference/summary.radf_obj.md):
that function's existing S3 dispatch
(`summary_radf.mc_cv`/`.wb_cv`/`.sb_cv`) is built entirely around
`radf_cv` test-statistic critical values – root CIs are a different kind
of output (needing a
[`datestamp()`](https://kvasilopoulos.github.io/exuber/reference/datestamp.md)
result, not a `radf_cv`) with no natural fit in that dispatch chain. A
standalone function keeps this addition to the size the enhancement
notes actually scoped ("a small follow-up"), rather than restructuring
[`summary()`](https://rdrr.io/r/base/summary.html)'s shared machinery to
accommodate a fundamentally different kind of result.

Root inference on a very short episode is statistically meaningless (the
same way it would be calling
[`explosive_root`](https://kvasilopoulos.github.io/exuber/reference/explosive_root.md)
directly on 2-3 points) – this function doesn't filter episodes itself,
since
[`datestamp`](https://kvasilopoulos.github.io/exuber/reference/datestamp.md)
already has a `min_duration` argument for exactly this; set it there
before piping into this function, rather than expecting this function to
second-guess what counts as "too short".

## Note

Returns its own class (not `radf_obj`), so it does not plug into
[`summary()`](https://rdrr.io/r/base/summary.html)/`\link{datestamp}`/`tidy`/`autoplot`
– prints its own per-episode confidence-interval table – see
[`vignette("naming-and-analysis", package = "exuber")`](https://kvasilopoulos.github.io/exuber/articles/naming-and-analysis.md)
for the full picture of which functions do and don't fit that pipeline.

## Status

**\[experimental\]**

## See also

[`explosive_root`](https://kvasilopoulos.github.io/exuber/reference/explosive_root.md),
[`root_ci`](https://kvasilopoulos.github.io/exuber/reference/root_ci.md),
[`datestamp`](https://kvasilopoulos.github.io/exuber/reference/datestamp.md)

## Examples

``` r
set.seed(2026)
burn <- cumsum(rnorm(60))
bubble <- burn[length(burn)] * 1.04^(1:40) + cumsum(rnorm(40, sd = 0.5))
y <- c(burn, bubble)

r <- radf(y, minw = 20)
cv <- radf_mc_cv(length(y), minw = 20, nrep = 300, seed = 4)
ds <- datestamp(r, cv = cv, min_duration = 3)

root_ci_datestamp(r, ds) # one row per datestamped episode
#> $series1
#>   Start End      rho rho_lower rho_upper doubling_time doubling_time_lower
#> 1    83 100 1.041945  1.029562  1.054328      16.86941            13.10209
#>   doubling_time_upper
#> 1            23.79234
#> 
```
