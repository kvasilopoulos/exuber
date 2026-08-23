# Retrieve/Replace series names

Retrieve or replace the series names of an object.

## Usage

``` r
series_names(x, ...)

series_names(x) <- value

# S3 method for class 'radf_obj'
series_names(x) <- value

# S3 method for class 'wb_cv'
series_names(x) <- value

# S3 method for class 'sb_cv'
series_names(x) <- value
```

## Arguments

- x:

  An object.

- ...:

  Further arguments passed to methods.

- value:

  n ordered vector of the same length as the "index" attribute of x.

## Examples

``` r
# \donttest{
rfd <- radf(sim_data)
series_names(rfd)
#> [1] "psy1"  "psy2"  "evans" "div"   "blan" 

# Rename the series -- propagates through tidy()/autoplot()'s facet labels
series_names(rfd) <- c("Bubble A", "Bubble B", "Bubble C", "Bubble D", "Bubble E")
series_names(rfd)
#> [1] "Bubble A" "Bubble B" "Bubble C" "Bubble D" "Bubble E"
tidy(rfd)
#> # A tibble: 5 × 4
#>   id         adf  sadf gsadf
#>   <fct>    <dbl> <dbl> <dbl>
#> 1 Bubble A -2.46  1.95  5.19
#> 2 Bubble B -2.86  7.88  7.88
#> 3 Bubble C -5.83  5.28  5.99
#> 4 Bubble D -1.95  1.11  1.34
#> 5 Bubble E -5.15  3.93 11.0 
autoplot(rfd)
#> Using `radf_crit` for `cv`.

# }

# Simulate bubble processes
dta <- data.frame(psy1 = sim_psy1(n = 100), psy2 = sim_psy2(n = 100))

rfd <- radf(dta)

series_names(rfd) <- c("OneBubble", "TwoBubbles")
```
