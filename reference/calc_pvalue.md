# Calculate p-values

Calculate p-values from `distr` object

## Usage

``` r
calc_pvalue(x, distr = NULL)
```

## Arguments

- x:

  A `radf_obj` object.

- distr:

  A `radf_distr` object.

## Value

A `tibble` with one row per series (`id`) and one p-value column per
statistic (`adf`, `sadf`, `gsadf`; the panel statistic only when `distr`
is a sieve-bootstrap distribution).

## Examples

``` r
# \donttest{
radf_psy1 <- radf(sim_psy1(100))

# Default: p-values against a Monte Carlo null distribution
pv <- calc_pvalue(radf_psy1)
#> Using `radf_mc_distr` for `distr`.
pv
#> # A tibble: 1 × 4
#>   id        adf  sadf gsadf
#>   <fct>   <dbl> <dbl> <dbl>
#> 1 series1 0.743     0     0

# Using the Wild-Bootstrapped null instead
wb_psy1 <- radf_wb_distr(sim_psy1(100))

calc_pvalue(radf_psy1, wb_psy1)
#> # A tibble: 1 × 4
#>   id        adf  sadf gsadf
#>   <chr>   <dbl> <dbl> <dbl>
#> 1 series1 0.932 0.036 0.036

sb_psy1 <- radf_sb_distr(sim_data, nboot = 500)

calc_pvalue(radf(sim_data), sb_psy1)
#> # A tibble: 1 × 2
#>   id    gsadf_panel
#>   <chr>       <dbl>
#> 1 panel           0

# Plot the three p-values for this series
barplot(unlist(pv[, c("adf", "sadf", "gsadf")]), ylab = "p-value",
  main = "calc_pvalue(): rejects at the usual 5% level if the bar is short")
abline(h = 0.05, col = "red", lty = 2)

# }
```
