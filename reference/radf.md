# Recursive Augmented Dickey-Fuller Test

`radf` returns the recursive univariate and panel Augmented
Dickey-Fuller test statistics.

## Usage

``` r
radf(data, minw = NULL, lag = 0L)
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

- lag:

  A non-negative integer. The lag length of the Augmented Dickey-Fuller
  regression (default = 0L).

## Value

A list that contains the unit root test statistics (sequence):

- adf:

  Augmented Dickey-Fuller

- badf:

  Backward Augmented Dickey-Fuller

- sadf:

  Supremum Augmented Dickey-Fuller

- bsadf:

  Backward Supremum Augmented Dickey-Fuller

- gsadf:

  Generalized Supremum Augmented Dickey-Fuller

- bsadf_panel:

  Panel Backward Supremum Augmented Dickey-Fuller

- gsadf_panel:

  Panel Generalized Supremum Augmented Dickey-Fuller

And attributes:

- mat:

  The matrix used in the estimation.

- index:

  The index parsed from the dataset.

- lag:

  The lag used in the estimation.

- n:

  The number of rows.

- minw:

  The minimum window used in the estimation.

- series_names:

  The series names.

## Details

The `radf()` function is vectorized, i.e., it can handle multiple series
at once, to improve efficiency. This property also enables the
computation of panel statistics internally as a by-product of the
univariate estimations with minimal additional cost incurred.

## References

Phillips, P. C. B., Wu, Y., & Yu, J. (2011). Explosive Behavior in The
1990s Nasdaq: When Did Exuberance Escalate Asset Values? International
Economic Review, 52(1), 201-226.

Phillips, P. C. B., Shi, S., & Yu, J. (2015). Testing for Multiple
Bubbles: Historical Episodes of Exuberance and Collapse in the S&P 500.
International Economic Review, 56(4), 1043-1078.

Pavlidis, E., Yusupova, A., Paya, I., Peel, D., Martínez-García, E.,
Mack, A., & Grossman, V. (2016). Episodes of exuberance in housing
markets: in search of the smoking gun. The Journal of Real Estate
Finance and Economics, 53(4), 419-449.

## Examples

``` r
# \donttest{
# We will use simulated data that are stored as data
sim_data
#> # A tibble: 100 × 5
#>    psy1  psy2  evans div   blan  
#>    <sim> <sim> <sim> <sim> <sim> 
#>  1 100   100   0.5   68.4  0.1   
#>  2 90.3  102   0.563 66    0.0408
#>  3 93.4  110   0.583 78.9  0.121 
#>  4 92.9  116   0.575 75.6  0.0195
#>  5 106   127   0.545 74.8  0.0216
#>  6 102   114   0.577 77.7  0.0117
#>  7 109   120   0.658 80.8  0.0265
#>  8 105   130   0.7   91.4  0.0238
#>  9 112   128   0.684 87.4  0.0332
#> 10 124   131   0.735 72.2  0.0342
#> # ℹ 90 more rows

rsim <- radf(sim_data)

str(rsim)
#> List of 7
#>  $ adf        : Named num [1:5] -2.46 -2.86 -5.83 -1.95 -5.15
#>   ..- attr(*, "names")= chr [1:5] "psy1" "psy2" "evans" "div" ...
#>  $ badf       : num [1:81, 1:5] -2.31 -2.36 -2.49 -2.26 -1.63 ...
#>   ..- attr(*, "dimnames")=List of 2
#>   .. ..$ : NULL
#>   .. ..$ : chr [1:5] "psy1" "psy2" "evans" "div" ...
#>  $ sadf       : Named num [1:5] 1.95 7.88 5.28 1.11 3.93
#>   ..- attr(*, "names")= chr [1:5] "psy1" "psy2" "evans" "div" ...
#>  $ bsadf      : num [1:81, 1:5] -2.31 -2.36 -2.49 -2.26 -1.26 ...
#>   ..- attr(*, "dimnames")=List of 2
#>   .. ..$ : NULL
#>   .. ..$ : chr [1:5] "psy1" "psy2" "evans" "div" ...
#>  $ gsadf      : Named num [1:5] 5.19 7.88 5.99 1.34 10.95
#>   ..- attr(*, "names")= chr [1:5] "psy1" "psy2" "evans" "div" ...
#>  $ bsadf_panel: num [1:81] -0.0702 -1.5266 -1.2379 -1.101 -0.7169 ...
#>  $ gsadf_panel: num 2.41
#>  - attr(*, "mat")= num [1:100, 1:5] 100 90.3 93.4 92.9 106.2 ...
#>   ..- attr(*, "dimnames")=List of 2
#>   .. ..$ : NULL
#>   .. ..$ : chr [1:5] "psy1" "psy2" "evans" "div" ...
#>   ..- attr(*, "index")= num [1:100] 1 2 3 4 5 6 7 8 9 10 ...
#>   ..- attr(*, "series_names")= chr [1:5] "psy1" "psy2" "evans" "div" ...
#>  - attr(*, "index")= num [1:100] 1 2 3 4 5 6 7 8 9 10 ...
#>  - attr(*, "series_names")= chr [1:5] "psy1" "psy2" "evans" "div" ...
#>  - attr(*, "minw")= num 19
#>  - attr(*, "lag")= int 0
#>  - attr(*, "n")= int 100
#>  - attr(*, "valid_range")= num [1:2, 1:5] 1 100 1 100 1 100 1 100 1 100
#>   ..- attr(*, "dimnames")=List of 2
#>   .. ..$ : chr [1:2] "start" "end"
#>   .. ..$ : chr [1:5] "psy1" "psy2" "evans" "div" ...
#>  - attr(*, "class")= chr [1:2] "radf_obj" "list"

# We would also use data that contain a Date column
sim_data_wdate
#> # A tibble: 100 × 6
#>    psy1  psy2  evans div   blan   date      
#>    <sim> <sim> <sim> <sim> <sim>  <date>    
#>  1 100   100   0.5   68.4  0.1    2000-01-01
#>  2 90.3  102   0.563 66    0.0408 2000-02-01
#>  3 93.4  110   0.583 78.9  0.121  2000-03-01
#>  4 92.9  116   0.575 75.6  0.0195 2000-04-01
#>  5 106   127   0.545 74.8  0.0216 2000-05-01
#>  6 102   114   0.577 77.7  0.0117 2000-06-01
#>  7 109   120   0.658 80.8  0.0265 2000-07-01
#>  8 105   130   0.7   91.4  0.0238 2000-08-01
#>  9 112   128   0.684 87.4  0.0332 2000-09-01
#> 10 124   131   0.735 72.2  0.0342 2000-10-01
#> # ℹ 90 more rows

rsim_wdate <- radf(sim_data_wdate)
#> Using `date` as index variable.

tidy(rsim_wdate)
#> # A tibble: 5 × 4
#>   id      adf  sadf gsadf
#>   <fct> <dbl> <dbl> <dbl>
#> 1 psy1  -2.46  1.95  5.19
#> 2 psy2  -2.86  7.88  7.88
#> 3 evans -5.83  5.28  5.99
#> 4 div   -1.95  1.11  1.34
#> 5 blan  -5.15  3.93 11.0 

augment(rsim_wdate)
#> # A tibble: 405 × 6
#>      key index      id        data   badf  bsadf
#>    <int> <date>     <chr>    <dbl>  <dbl>  <dbl>
#>  1    20 2001-08-01 psy1  104.     -2.31  -2.31 
#>  2    20 2001-08-01 psy2  161.     -0.756 -0.756
#>  3    20 2001-08-01 evans   1.92    5.28   5.28 
#>  4    20 2001-08-01 div   134.      0.184  0.184
#>  5    20 2001-08-01 blan    0.0585 -2.75  -2.75 
#>  6    21 2001-09-01 psy1  110.     -2.36  -2.36 
#>  7    21 2001-09-01 psy2  170.     -0.167 -0.167
#>  8    21 2001-09-01 evans   0.489  -2.73  -2.69 
#>  9    21 2001-09-01 div   140.      0.288  0.288
#> 10    21 2001-09-01 blan    0.0438 -2.81  -2.70 
#> # ℹ 395 more rows

tidy(rsim_wdate, panel = TRUE)
#> # A tibble: 1 × 1
#>   gsadf_panel
#>         <dbl>
#> 1        2.41

head(index(rsim_wdate))
#> [1] "2000-01-01" "2000-02-01" "2000-03-01" "2000-04-01" "2000-05-01"
#> [6] "2000-06-01"

# For lag = 1 and minimum window = 20
rsim_20 <- radf(sim_data, minw = 20, lag = 1)
# }
```
