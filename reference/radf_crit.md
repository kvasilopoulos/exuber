# Stored Monte Carlo Critical Values

A dataset containing Monte Carlo critical values for up to 600
observations generated using the default minimum window. The critical
values have been simulated and stored as data to save computation time
for the user. The stored critical values can be obtained with the
[`radf_mc_cv`](https://kvasilopoulos.github.io/exuber/reference/radf_mc_cv.md)
function, using nrep = `2000` and the `seed = 123`.

## Usage

``` r
radf_crit
```

## Format

A list with lower level lists that contain

- adf_cv::

  Augmented Dickey-Fuller

- badf_cv::

  Backward Augmented Dickey-Fuller

- sadf_cv::

  Supremum Augmented Dickey-Fuller

- bsadf_cv::

  Backward Supremum Augmented Dickey-Fuller

- gsadf_cv::

  Generalized Supremum Augmented Dickey Fuller

## Source

Simulated from exuber package function
[`radf_mc_cv`](https://kvasilopoulos.github.io/exuber/reference/radf_mc_cv.md).

## Examples

``` r
if (FALSE) { # \dontrun{
all.equal(radf_crit[[50]], radf_mc_cv(50, nrep = 2000, seed = 123))
} # }
```
