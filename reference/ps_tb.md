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
