# SV-ADF Asymmetric-Threshold Bubble Dating (Sarkar & Wells 2026)

`radf_svadf` implements Sarkar & Wells (2026)'s SV-ADF date-stamping
procedure:
[`radf()`](https://kvasilopoulos.github.io/exuber/reference/radf.md)'s
own recursive (backward) ADF t-statistic (`badf`), which the paper's own
asymptotic theory (their Theorem 3.1) justifies under substantially
weaker volatility conditions than PWY/PSY's original derivation
(nearly-nonstationary *stochastic* volatility, not just deterministic
time-varying volatility), compared against two different closed-form,
sample-size-only thresholds: `log(t)/10` for origination and `log(t)/2`
for collapse (`t` the current recursive window's own sample size) – both
from the paper's own calibration exercise (their Section 5.1), not new
simulation.

## Usage

``` r
radf_svadf(data, minw = NULL, min_duration = NULL)
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

- min_duration:

  Minimum number of consecutive periods a threshold crossing must
  persist to be dated (default
  [`psy_ds`](https://kvasilopoulos.github.io/exuber/reference/psy_minw.md)`(n)`).

## Value

An object of class `radf_svadf_obj`: a list with the `badf` statistic
path, the `origination`/`collapse` threshold paths, and
`origination`/`collapse` date indices (`NA` if not detected).

## Details

Origination is dated at the first run of at least `min_duration`
consecutive points with `badf` above the origination threshold; collapse
is dated (searching only after the origination date) at the first run of
at least `min_duration` consecutive points with `badf` below the (lower)
collapse threshold.

## Note

Reuses
[`radf`](https://kvasilopoulos.github.io/exuber/reference/radf.md)'s own
`badf` sequence directly, compared against the two closed-form
thresholds above – no simulation and no separate critical-value
function, unlike
[`radf_mc_cv`](https://kvasilopoulos.github.io/exuber/reference/radf_mc_cv.md)/
[`radf_wb_cv`](https://kvasilopoulos.github.io/exuber/reference/radf_wb_cv.md).

Returns its own class (not `radf_obj`), so it does not plug into
[`summary()`](https://rdrr.io/r/base/summary.html)/`\link{datestamp}`/`tidy`/`autoplot`
– prints its own origination/collapse date summary – see
[`vignette("naming-and-analysis", package = "exuber")`](https://kvasilopoulos.github.io/exuber/articles/naming-and-analysis.md)
for the full picture of which functions do and don't fit that pipeline.

## Caveats

**\[experimental\]**

`Sarkar & Wells (2026)` is a non-peer-reviewed preprint, a different bar
than every other source implemented in this package. The same note is
emitted as a message when this function is called (see
[`message`](https://rdrr.io/r/base/message.html)/[`suppressMessages`](https://rdrr.io/r/base/message.html)
to silence it) and stored as `attr(x, "caveat")` on the returned object.

## References

Sarkar, A., & Wells, M. T. (2026). Is there an AI bubble? Robust
date-stamping for periods of exuberance. arXiv:2604.12062.

## See also

[`datestamp`](https://kvasilopoulos.github.io/exuber/reference/datestamp.md)
for the symmetric-threshold PWY/PSY dating this complements.

## Examples

``` r
# \donttest{
res <- radf_svadf(sim_data)
#> Experimental. Sarkar & Wells (2026) is a non-peer-reviewed preprint; see ?radf_svadf, Caveats section.
print(res)
#> 
#> ── radf_svadf (n = 100, minw = 19, min_duration = 5) ───────────────────────────
#> 
#> ℹ Experimental. Sarkar & Wells (2026) is a non-peer-reviewed preprint; see ?radf_svadf, Caveats section.
#> 
#>   series  origination  origination_date  collapse  collapse_date
#>     psy1           48                48        49             49
#>     psy2           23                23        24             24
#>    evans           NA              <NA>        NA           <NA>
#>      div           NA              <NA>        NA           <NA>
#>     blan           NA              <NA>        NA           <NA>
#> 
# }
```
