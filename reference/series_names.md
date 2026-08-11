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

# Simulate bubble processes
dta <- data.frame(psy1 = sim_psy1(n = 100), psy2 = sim_psy2(n = 100))

rfd <- radf(dta)

series_names(rfd) <- c("OneBubble", "TwoBubbles")
```
