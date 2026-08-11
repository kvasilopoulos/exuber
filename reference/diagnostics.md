# Diagnostics on hypothesis testing

Provides information on whether the null hypothesis of a unit root is
rejected against the alternative of explosive behaviour for each series
in a dataset.

## Usage

``` r
diagnostics(object, cv = NULL, ...)

# S3 method for class 'radf_obj'
diagnostics(object, cv = NULL, option = c("gsadf", "sadf"), ...)
```

## Arguments

- object:

  An object of class `obj`.

- cv:

  An object of class `cv`.

- ...:

  Further arguments passed to methods.

- option:

  Whether to apply the "gsadf" or "sadf" methodology (default =
  "gsadf").

## Value

Returns a list with the series that reject (positive) and the series
that do not reject (negative) the null hypothesis, and at what
significance level.

## Details

Diagnostics also stores a vector whose elements take the value of 1 when
there is a period of explosive behaviour and 0 otherwise.

## Examples

``` r

rsim_data <- radf(sim_data)
diagnostics(rsim_data)
#> Using `radf_crit` for `cv`.
#> 
#> ── Diagnostics (option = gsadf) ───────────────────────────────── Monte Carlo ──
#> 
#> psy1:     Rejects H0 at the 1% significance level
#> psy2:     Rejects H0 at the 1% significance level
#> evans:    Rejects H0 at the 1% significance level
#> div:      Cannot reject H0 
#> blan:     Rejects H0 at the 1% significance level
#> 

diagnostics(rsim_data, option = "sadf")
#> Using `radf_crit` for `cv`.
#> 
#> ── Diagnostics (option = sadf) ────────────────────────────────── Monte Carlo ──
#> 
#> psy1:     Rejects H0 at the 1% significance level
#> psy2:     Rejects H0 at the 1% significance level
#> evans:    Rejects H0 at the 1% significance level
#> div:      Rejects H0 at the 10% significance level
#> blan:     Rejects H0 at the 1% significance level
#> 
```
