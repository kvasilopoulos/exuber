# Retrieve/Replace the index

Retrieve or replace the index of an object.

## Usage

``` r
index(x, ...)

index(x) <- value
```

## Arguments

- x:

  An object.

- ...:

  Further arguments passed to methods.

- value:

  An ordered vector of the same length as the `index` attribute of x.

## Details

If the user does not specify an index for the estimation a pseudo-index
is generated which is a sequential numeric series. After the estimation,
the user can use `index()` to retrieve or `index<-()` to replace the
index. The index can be either numeric or Date.

## Examples

``` r
# \donttest{
# A plain numeric vector gets a pseudo-index (1, 2, 3, ...)
rsim <- radf(sim_data)
head(index(rsim))
#> [1] 1 2 3 4 5 6

# A data.frame with a Date column uses it as the index automatically
rsim_wdate <- radf(sim_data_wdate)
#> Using `date` as index variable.
head(index(rsim_wdate))
#> [1] "2000-01-01" "2000-02-01" "2000-03-01" "2000-04-01" "2000-05-01"
#> [6] "2000-06-01"
class(index(rsim_wdate))
#> [1] "Date"

# autoplot() uses index() internally for the x-axis
autoplot(rsim_wdate)
#> Using `radf_crit` for `cv`.


# Replace the index, e.g. with a custom Date sequence
index(rsim) <- seq(as.Date("2000-01-01"), by = "month", length.out = length(index(rsim)))
head(index(rsim))
#> [1] "2000-01-01" "2000-02-01" "2000-03-01" "2000-04-01" "2000-05-01"
#> [6] "2000-06-01"
autoplot(rsim)
#> Using `radf_crit` for `cv`.

# }
```
