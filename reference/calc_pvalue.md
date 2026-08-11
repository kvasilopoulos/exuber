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

## Examples

``` r
if (FALSE) { # \dontrun{
radf_psy1 <- radf(sim_psy1(100))

calc_pvalue(radf_psy1)

# Using the Wild-Bootstrapped
wb_psy1 <- radf_wb_distr(sim_psy1(100))

calc_pvalue(radf_psy1, wb_psy1)

sb_psy1 <- radf_sb_distr(sim_data)

calc_pvalue(radf(sim_data), sb_psy1)
} # }
```
