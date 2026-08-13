# Recursively Demeaned Sign-Based Bubble Test (s-bar-PWY / s-bar-PSY)

`radf_sign_dm` computes Harvey, Leybourne & Zu (2020)'s second
sign-based analogue of the recursive right-tailed unit root test,
denoted \\\bar{s}PWY\\/\\\bar{s}PSY\\ in the paper: the same
construction as
[`radf_sign`](https://kvasilopoulos.github.io/exuber/reference/radf_sign.md),
but built on a recursively (expanding-window) demeaned cumulated-sign
series,
`Ctilde_t = sum_{i=2}^{t} (sign(diff(y)_i) - mean(sign(diff(y)_{2:i})))`,
rather than the raw cumulated sign `radf_sign` uses.

## Usage

``` r
radf_sign_dm(data, minw = NULL)
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

## Details

Harvey, Leybourne, Tatlow & Zu (2025) show this statistic shares
[`radf_sign`](https://kvasilopoulos.github.io/exuber/reference/radf_sign.md)'s
asymptotic level-shift robustness (see that function's
`Level-shift robustness` section) without requiring Assumption 2 of the
underlying HLZ (2020) theory (that the innovations' median is zero) – a
strictly weaker requirement than
[`radf_sign`](https://kvasilopoulos.github.io/exuber/reference/radf_sign.md)
needs for its own invariance result. Their finite -sample simulations
also find the recursive demeaning tends to further reduce size
distortion under level shifts relative to `radf_sign`, though both are
asymptotically level-shift robust under the same condition.

## Note

Needs
[`radf_sign_dm_cv`](https://kvasilopoulos.github.io/exuber/reference/radf_sign_dm_cv.md)
for critical values (not
[`radf_sign_cv`](https://kvasilopoulos.github.io/exuber/reference/radf_sign_cv.md),
which is calibrated to the non-demeaned
[`radf_sign`](https://kvasilopoulos.github.io/exuber/reference/radf_sign.md)
statistic instead) – pivotal like `radf_sign`, so no per-dataset
bootstrap is needed.

## Status

**\[experimental\]**

## References

Harvey, D. I., Leybourne, S. J., & Zu, Y. (2020). Sign-based unit root
tests for explosive financial bubbles in the presence of
deterministically time-varying volatility. Econometric Theory, 36(1),
122-169.

Harvey, D. I., Leybourne, S. J., Tatlow, D., & Zu, Y. (2025). Unit root
tests for explosive financial bubbles in the presence of deterministic
level shifts. Oxford Bulletin of Economics and Statistics, 87(5),
879-901. [doi:10.1111/obes.12668](https://doi.org/10.1111/obes.12668)

## See also

[`radf_sign_dm_cv`](https://kvasilopoulos.github.io/exuber/reference/radf_sign_dm_cv.md)
for critical values, and
[`radf_sign`](https://kvasilopoulos.github.io/exuber/reference/radf_sign.md)
for the non-demeaned sign-based analogue.

## Examples

``` r
# \donttest{
res <- radf_sign_dm(sim_data, minw = 20)
print(res)
#> 
#> ── radf_sign_dm (minw = 20) ────────────────────────────────────────────────────
#> 
#>   series       adf     sadf  gsadf
#>     psy1  -0.07152   2.1729  2.976
#>     psy2   1.47812   3.0005  3.433
#>    evans  -2.14492  -0.9121  1.149
#>      div  -1.05486   2.3305  2.369
#>     blan  -0.17252   1.3508  1.489
#> 

cv <- radf_sign_dm_cv(n = 100, minw = 20)
summary(res, cv = cv)
#> Error in full_join(tidy(x, format = "long"), tidy(y, format = "long"),     by = c("stat", join_by), relationship = "many-to-many"): Join columns in `y` must be present in the data.
#> ✖ Problem with `id`.
# }
```
