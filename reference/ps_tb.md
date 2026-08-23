# Helper function to find `tb` from the Phillips and Shi (2020)

This function helps to find the number of observations in the window
over which size is to be controlled.

## Usage

``` r
ps_tb(n, freq = c("monthly", "quarterly", "annual", "weekly"), size = 2)
```

## Arguments

- n:

  A positive integer. The sample size.

- freq:

  The type of date-interval.

- size:

  The size to be controlled.

## References

Phillips, P. C., & Shi, S. (2020). Real time monitoring of asset
markets: Bubbles and crises. In Handbook of Statistics (Vol. 42, pp.
61-80). Elsevier.

Shi, S., Hurn, S., Phillips, P.C.B., 2018. Causal change detection in
possibly integrated systems: Revisiting the money-income relationship.

## Examples

``` r
# Training window controlling size over a 2-year span of monthly data
tb <- ps_tb(100, freq = "monthly", size = 2)
tb
#> [1] 42

# \donttest{
# Use it directly as monitor()'s training window
monitor(sim_data, r_star = tb, boundary = "kurozumi")
#> 
#> ── monitor (T* = 42 / 100, minw = 19, level = 95%, boundary = kurozumi) ────────
#> 
#>   series  boundary  alarm  alarm_date
#>     psy1     1.038     50          50
#>     psy2     1.038     NA        <NA>
#>    evans     1.038     NA        <NA>
#>      div     1.038     NA        <NA>
#>     blan     1.038     NA        <NA>
#> 
# }
```
