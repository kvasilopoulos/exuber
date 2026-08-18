# Look Up exuber's Test/Procedure Functions by Family

Naming conventions (`radf_`/`_test`/`dating_`/ `monitor_`/`root_`, see
[`vignette("naming-and-analysis")`](https://kvasilopoulos.github.io/exuber/articles/naming-and-analysis.md))
are a guide, not a contract – easy to misremember, and occasionally
traded off deliberately
([`monitor_radf()`](https://kvasilopoulos.github.io/exuber/reference/monitor_radf.md)
is ADF-family internally but named for what it does). This is the
actual, queryable source of truth: which of the package's
test/dating/monitoring/root-inference functions belong to which family.

## Usage

``` r
exuber_functions(family = NULL)
```

## Arguments

- family:

  One of `"adf"` (built on the recursive-ADF core), `"test"` (a
  standalone hypothesis test), `"dating"`
  (point-estimation/model-selection, no formal test), `"monitor"`
  (real-time/sequential), `"root"` (confidence-interval inference on the
  explosive root), `"regression"` (point estimation, no test), or `NULL`
  (default) for every function. A function can belong to more than one
  family
  ([`monitor_radf()`](https://kvasilopoulos.github.io/exuber/reference/monitor_radf.md)
  is both `"adf"` and `"monitor"`).

## Value

A tibble with columns `name`, `family`, and `description`, one row per
function.

## See also

[`vignette("naming-and-analysis", package = "exuber")`](https://kvasilopoulos.github.io/exuber/articles/naming-and-analysis.md)
for the full naming scheme and which functions plug into
[`summary()`](https://rdrr.io/r/base/summary.html)/[`datestamp`](https://kvasilopoulos.github.io/exuber/reference/datestamp.md)/`tidy`/`autoplot`.

## Examples

``` r
exuber_functions()
#> # A tibble: 34 × 3
#>    name         family description                                              
#>    <chr>        <chr>  <chr>                                                    
#>  1 radf         adf    The recursive ADF/SADF/GSADF/BSADF statistic (Phillips, …
#>  2 radf_mc_cv   adf    Monte Carlo critical values for radf().                  
#>  3 radf_wb_cv   adf    Wild bootstrap critical values for radf() (heteroskedast…
#>  4 radf_wb_cv2  adf    Wild bootstrap critical values with a training-window bo…
#>  5 radf_sb_cv   adf    Panel sieve bootstrap critical values for radf().        
#>  6 radf_tt      adf    Time-transformed test (STADF/GSTADF), bootstrap-free het…
#>  7 radf_tt_cv   adf    Pivotal asymptotic critical values for radf_tt().        
#>  8 radf_sign    adf    Sign-based sPWY/sPSY test, exactly invariant to heterosk…
#>  9 radf_sign_cv adf    Critical values for radf_sign().                         
#> 10 radf_sign_dm adf    Recursively demeaned sign-based test, robust to determin…
#> # ℹ 24 more rows
exuber_functions(family = "monitor")
#> # A tibble: 4 × 3
#>   name             family      description                                      
#>   <chr>            <chr>       <chr>                                            
#> 1 monitor_radf     adf,monitor Real-time monitoring (Family A); reuses radf()'s…
#> 2 monitor_cusum    monitor     CUSUM/CUSUMV real-time monitoring, closed-form b…
#> 3 monitor_lbi      monitor     Sequential extension of lbi_test(), constant-bou…
#> 4 monitor_quantile monitor     QPWY recursive quantile-regression monitoring, e…
exuber_functions(family = "test")
#> # A tibble: 5 × 3
#>   name          family description                                              
#>   <chr>         <chr>  <chr>                                                    
#> 1 lbi_test      test   Locally best invariant test for a bubble spanning the wh…
#> 2 ssu_test      test   Stochastic explosive-coefficient test on squared first d…
#> 3 quantile_test test   Quantile-regression global test, an alternative to the m…
#> 4 cobubble_test test   KPSS-type co-explosive test between two series (Evripido…
#> 5 radf_sbz_cv   test   SBZ WLS/kernel-volatility test with union-of-rejections;…
```
