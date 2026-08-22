# Deprecated functions in package exuber.

The functions listed below are deprecated and will be defunct in the
near future. When possible, alternative functions with similar
functionality are also mentioned. Help pages for deprecated functions
are available at `help("exuber-deprecated")`.

## Usage

``` r
col_names(x)

mc_cv(n, minw = NULL, nrep = 1000L, seed = NULL)

wb_cv(data, minw = NULL, nboot = 1000L, seed = NULL)

sb_cv(data, minw = NULL, nboot = 1000L, seed = NULL)

radf_wb_cv2(
  data,
  minw = NULL,
  nboot = 500L,
  adflag = 0,
  type = c("fixed", "aic", "bic"),
  tb = NULL,
  seed = NULL
)

radf_wb_distr2(
  data,
  minw = NULL,
  nboot = 500L,
  adflag = 0,
  type = c("fixed", "aic", "bic"),
  tb = NULL,
  seed = NULL
)
```
