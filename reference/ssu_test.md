# Stochastic Unit Root Bubble Test (Kurozumi & Nishi 2025)

`ssu_test` implements the SSU and GSSU statistics of Kurozumi & Nishi
(2025). They are sup-type tests for a bubble that test for a stochastic,
and not deterministic, unit root in the *squared* first differences,
`(Delta y_t)^2 = mu2 + omega*y_{t-1}^2 + eta_t`. The statistic is
bias-corrected for the dependence on the correlation between the
innovations of this regression and those of the plain ADF regression.

## Usage

``` r
ssu_test(
  data,
  minw = NULL,
  sig_lvl = 95,
  type = c("ssu", "gssu"),
  union = FALSE,
  cv = NULL
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

- minw:

  Minimum window. The default is
  [`psy_minw`](https://kvasilopoulos.github.io/exuber/reference/psy_minw.md)
  for `"ssu"` and the value of the paper,
  `floor(n * (-0.004 + 2.24/sqrt(n)))`, for `"gssu"`. Table I is
  computed at these values.

- sig_lvl:

  Significance level on the 0 to 100 scale used throughout the package,
  one of `90`, `95` or `99`. Table I of Kurozumi & Nishi tabulates these
  levels.

- type:

  `"ssu"` or `"gssu"`.

- union:

  Logical. Also run the union-of-rejections procedure with SADF
  (`"ssu"`) or GSADF (`"gssu"`).

- cv:

  Critical values for the SADF or GSADF side of the union, for example
  from
  [`radf_mc_cv`](https://kvasilopoulos.github.io/exuber/reference/radf_mc_cv.md)
  with `lag = 0`. The default is the precomputed critical values, which
  are fetched on first use.

## Value

An object of class `ssu_test_obj`: a list with the statistic path
(`stat`, one value for each candidate end point from `minw` to `n`. For
GSSU it is the sup over window starts at each end point), the constant
`crit` from Table I, `sadf` (the maximum, which is compared with `crit`)
and `detected`. With `union = TRUE` the list also contains `adf_stat`
(SADF or GSADF), `union_stat`, `union_crit` and `union_detected`.

## Details

This test generalizes the framework differently from the other
volatility-robustness tests in exuber. It does not touch the innovation
variance at all. It allows the explosive AR coefficient itself to vary
stochastically over time, `1 + c1/T + a*u_t/sqrt(T)`, where every
recursive-ADF-family statistic in this package assumes the deterministic
coefficient `1 + c/T^alpha`.

`type = "ssu"` is the single recursion, with the start fixed at the
beginning of the sample, which has the shape of `SADF`. `type = "gssu"`
also takes the supremum over window starts, which has the shape of
`GSADF`, with the minimum window `r0 = -0.004 + 2.24/sqrt(n)` from the
paper. The paper finds that GSSU is no more powerful than SSU.

`union = TRUE` adds the union-of-rejections procedure that the paper
recommends: `UR = max(SADF / cv_sadf, SSU / cv_ssu)` (or `GUR` with
GSADF and GSSU), compared with the published scaling constant `ur`
(`gur`). Neither SADF nor SSU dominates. SSU wins when the explosive
coefficient is stochastic, SADF wins when it is deterministic, and the
union stays close to the better of the two. The SADF or GSADF side is
[`radf`](https://kvasilopoulos.github.io/exuber/reference/radf.md) with
its default minimum window and `lag = 0`, compared with `cv` (default:
the precomputed critical values).

## Note

The SSU and GSSU critical values and the union constants are published
asymptotic values (Table I of Kurozumi & Nishi 2025), so no simulation
is needed. The union constant is valid only at the level for which the
statistic was built.

The function returns its own class and not `radf_obj`, so it does not
work with [`summary()`](https://rdrr.io/r/base/summary.html),
`\link{datestamp}` and `tidy`. It has its own
[`print()`](https://rdrr.io/r/base/print.html) and
[`autoplot()`](https://ggplot2.tidyverse.org/reference/autoplot.html)
methods instead. [`print()`](https://rdrr.io/r/base/print.html) shows
the statistic and the critical value. See
[`vignette("naming-and-analysis", package = "exuber")`](https://kvasilopoulos.github.io/exuber/articles/naming-and-analysis.md)
for which functions fit the shared pipeline and which do not.

## Status

**\[experimental\]**

## References

Kurozumi, E., & Nishi, M. (2025). Bubble testing with stochastically
varying explosive coefficient. Journal of Time Series Analysis, 46(5),
945-965.

## See also

[`radf`](https://kvasilopoulos.github.io/exuber/reference/radf.md) for
the recursive ADF-family alternative with a deterministic coefficient,
which this test complements.

Other volatility-robust tests:
[`cusum_test()`](https://kvasilopoulos.github.io/exuber/reference/cusum_test.md),
[`radf_kp()`](https://kvasilopoulos.github.io/exuber/reference/radf_kp.md),
[`radf_sbz()`](https://kvasilopoulos.github.io/exuber/reference/radf_sbz.md),
[`radf_sbz_union()`](https://kvasilopoulos.github.io/exuber/reference/radf_sbz_union.md),
[`radf_sign()`](https://kvasilopoulos.github.io/exuber/reference/radf_sign.md),
[`radf_sign_dm()`](https://kvasilopoulos.github.io/exuber/reference/radf_sign_dm.md),
[`radf_tt()`](https://kvasilopoulos.github.io/exuber/reference/radf_tt.md)

## Examples

``` r
# \donttest{
# A stochastically varying explosive root, rho_t = 1 + 3/n + 4 * u_t / sqrt(n).
# This is the alternative that ssu_test() is built for, and lbi_test() is built
# for a fixed root
y <- sim_psy1(n = 150, te = 75, tf = 150, c = 3, alpha = 1, seed = 2001,
  coef_noise = rnorm(149), coef_a = 4)
res <- ssu_test(y, sig_lvl = 95)
print(res)
#> 
#> ── ssu_test (SSU, n = 150, minw = 23, sig_lvl = 95%, crit = 3.3) ───────────────
#> 
#>    series   sadf  detected
#>   series1  15.02      TRUE
#> 

# The double-recursion version
ssu_test(y, type = "gssu")
#> 
#> ── ssu_test (GSSU, n = 150, minw = 26, sig_lvl = 95%, crit = 5.37) ─────────────
#> 
#>    series   sadf  detected
#>   series1  15.02      TRUE
#> 

# Plot the recursive SSU statistic path against its critical value
autoplot(res)

# }
```
