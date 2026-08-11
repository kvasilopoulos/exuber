# Monte Carlo critical values for the time-transformed test (STADF/GSTADF)

Simulates the asymptotic null distribution of the GLS-demeaned recursive
sup-ADF statistic used by
[`radf_tt`](https://kvasilopoulos.github.io/exuber/reference/radf_tt.md).
Per Theorem 1 of Kurozumi, Skrobotov & Tsarev, this distribution is free
of the volatility process (pivotal), so – unlike
[`radf_wb_cv`](https://kvasilopoulos.github.io/exuber/reference/radf_wb_cv.md)
– it does not need to be recomputed per dataset: a large `n` with
default `nrep` well approximates the T -\> Inf limit used in the paper.

## Usage

``` r
radf_tt_cv(n, minw = NULL, nrep = 2000L, seed = NULL)
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

## Details

The `sadf_cv` column (STADF, i.e. `r1 = 0` fixed) can be checked against
Whitehouse (2019)'s published asymptotic values, quoted in Kurozumi,
Skrobotov & Tsarev's footnote 4: for `minw/n = 0.1`, (10\\ 5\\ STADF,
not GSTADF (`gsadf_cv`) – the paper's own GSTADF critical values are not
given as literal numbers in the text, only as "easily computed from" the
authors' R code.

## References

Kurozumi, E., Skrobotov, A., & Tsarev, A. (2024). Time-Transformed Test
for Bubbles under Non-stationary Volatility. Journal of Financial
Econometrics.
[doi:10.1093/jjfinec/nbae026](https://doi.org/10.1093/jjfinec/nbae026)
