# Monte Carlo Critical Values for Reverse-Regression Recovery Dating

Computes critical values for the reverse-regression BSADF statistic used
by
[`radf_recovery`](https://kvasilopoulos.github.io/exuber/reference/radf_recovery.md),
calibrated to its own (Phillips & Shi 2014's Theorem 1) null limiting
distribution rather than the standard forward
[`radf_mc_cv`](https://kvasilopoulos.github.io/exuber/reference/radf_mc_cv.md)
boundary – reversing the simulated null path before running the
recursive computation, since reversal induces an endogeneity with no
forward-regression analogue (see
[`radf_recovery`](https://kvasilopoulos.github.io/exuber/reference/radf_recovery.md)'s
Details).

## Usage

``` r
radf_recovery_cv(n, minw = NULL, nrep = 1000L, seed = NULL, lag = 0)
```

## Arguments

- n:

  A positive integer. The sample size.

- minw:

  A positive integer. The minimum window size (default = \\(0.01 +
  1.8/\sqrt(T))T\\, where T denotes the sample size).

- nrep:

  A positive integer. The number of Monte Carlo simulations.

- seed:

  An object specifying if and how the random number generator (rng)
  should be initialized. Either NULL or an integer will be used in a
  call to `set.seed` before simulation. If set, the value is saved as
  "seed" attribute of the returned value. The default, NULL, will not
  change rng state, and return .Random.seed as the "seed" attribute.
  Results are reproducible across the parallel and non-parallel option
  when the same seed is used.

- lag:

  A non-negative integer. Number of lags in the auxiliary regression, as
  in [`radf`](https://kvasilopoulos.github.io/exuber/reference/radf.md).

## Value

A list of class `radf_cv` with a single element, `bsadf_cv`: a matrix of
critical values (columns `90%`, `95%`, `99%`), one row per reverse-time
position, aligned the same way
[`radf_mc_cv`](https://kvasilopoulos.github.io/exuber/reference/radf_mc_cv.md)'s
own `bsadf_cv` aligns to `radf()$bsadf`.

## Status

**\[experimental\]**

## See also

[`radf_recovery`](https://kvasilopoulos.github.io/exuber/reference/radf_recovery.md),
[`radf_mc_cv`](https://kvasilopoulos.github.io/exuber/reference/radf_mc_cv.md)
