# Simulation of a bivariate co-explosive pair

Simulation of Evripidou, Harvey, Leybourne & Sollis (2022)'s
co-explosive DGP: an explosive series `x` (from
[`sim_psy1`](https://kvasilopoulos.github.io/exuber/reference/sim_psy1.md))
and a second series `y` linked to a lead/lagged copy of it (and
optionally a third, independent explosive series `z`), \\y_t = \mu_y +
\phi_x x\_{t-i} + \phi_z z_t + \epsilon\_{y,t}\\. `i > 0` means `x`'s
explosive episode leads `y`'s; `i < 0` means it lags.

## Usage

``` r
sim_coexplosive(
  n,
  lag = 0,
  phi_x = 1,
  phi_z = 0,
  mu_y = 0,
  sigma_y = 6.79,
  x_args = list(),
  z_args = list(),
  seed = NULL
)
```

## Arguments

- n:

  A positive integer specifying the length of the simulated output
  series.

- lag:

  Integer lead (`> 0`) or lag (`< 0`) of `x` relative to `y`; 0 =
  contemporaneous.

- phi_x, phi_z, mu_y:

  Linkage coefficients and intercept for `y`.

- sigma_y:

  A non-negative scalar, the standard deviation of `y`'s own noise.

- x_args, z_args:

  Named lists of extra arguments passed to the
  [`sim_psy1`](https://kvasilopoulos.github.io/exuber/reference/sim_psy1.md)
  calls generating `x` and (if `phi_z != 0`) `z`.

- seed:

  An object specifying if and how the random number generator (rng)
  should be initialized. Either NULL or an integer will be used in a
  call to `set.seed` before simulation. If set, the value is saved as
  "seed" attribute of the returned value. The default, NULL, will not
  change rng state, and return .Random.seed as the "seed" attribute.
  Results are reproducible across the parallel and non-parallel option
  when the same seed is used.

## Value

A `data.frame` with columns `x` and `y` (length `n`).

## References

Evripidou, C., Harvey, D.I., Leybourne, S.J. & Sollis, R. (2022).
"Co-explosive behaviour in explosive financial bubbles." OBES.

## See also

[`sim_psy1`](https://kvasilopoulos.github.io/exuber/reference/sim_psy1.md)

## Examples

``` r
sim_coexplosive(n = 100, lag = 5, seed = 123)
#>             x         y
#> 1   100.00000        NA
#> 2    96.19437        NA
#> 3    94.63147        NA
#> 4   105.21509        NA
#> 5   105.69385        NA
#> 6   106.57171  93.53851
#> 7   118.21700  95.88863
#> 8   121.34662  89.30196
#> 9   112.75686  93.88977
#> 10  108.09313 103.11211
#> 11  105.06708 112.81170
#> 12  113.37860 114.31040
#> 13  115.82173 125.47470
#> 14  118.54297 101.77143
#> 15  119.29451 107.71586
#> 16  115.52035 108.59386
#> 17  127.65349 115.42343
#> 18  131.03389 116.53927
#> 19  117.68056 114.19258
#> 20  122.44277 113.52501
#> 21  119.23251 108.56651
#> 22  111.98199 128.45231
#> 23  110.50194 124.60054
#> 24  103.53537 114.34967
#> 25   98.58620 120.70390
#> 26   94.34218 131.75234
#> 27   82.88953 107.55525
#> 28   88.57811 112.10021
#> 29   89.61951 104.06472
#> 30   81.89156  92.05519
#> 31   90.40496  93.85800
#> 32   93.30066  92.69803
#> 33   91.29712  91.64382
#> 34   97.37502  89.89948
#> 35  103.33755  79.02281
#> 36  108.91609  76.46342
#> 37  113.59195 100.98244
#> 38  117.35305  81.37938
#> 39  116.93267 102.39927
#> 40  122.23314 116.30036
#> 41  127.36213  99.11205
#> 42  130.68108 118.35707
#> 43  137.51474 115.57273
#> 44  137.59929 106.25782
#> 45  161.00843 111.94855
#> 46  179.36944 116.48770
#> 47  183.06098 127.07622
#> 48  191.87576 127.58942
#> 49  200.81371 142.27025
#> 50  218.78016 175.26817
#> 51  232.01818 170.63050
#> 52  248.37757 188.40972
#> 53  263.85530 197.09755
#> 54  280.21236 203.06936
#> 55  307.18537 211.93328
#> 56  120.70016 231.20710
#> 57  130.99699 246.47369
#> 58  120.48096 267.67800
#> 59  124.45049 277.68350
#> 60  125.29146 313.81902
#> 61  126.75770 118.15675
#> 62  129.33545 138.14490
#> 63  125.92468 113.35705
#> 64  123.66220 115.89403
#> 65  116.74607 147.29812
#> 66  109.46861 123.92724
#> 67  111.52957 131.36042
#> 68  114.57291 130.24698
#> 69  114.93281 120.37733
#> 70  121.19501 120.25556
#> 71  135.11508 111.97388
#> 72  131.78098 110.06713
#> 73  116.10172 115.01625
#> 74  122.93069 114.70150
#> 75  118.11522 135.64720
#> 76  113.44364 130.08141
#> 77  120.40727 124.33917
#> 78  118.47366 116.35831
#> 79  110.18499 125.03885
#> 80  111.41604 121.07921
#> 81  110.47296 110.33134
#> 82  110.51210 113.18728
#> 83  113.12816 127.05069
#> 84  110.61137 107.81086
#> 85  114.98669 105.53920
#> 86  113.48959 108.86863
#> 87  115.74239 109.17328
#> 88  123.18992 120.66451
#> 89  126.14481 111.18674
#> 90  123.93173 120.10672
#> 91  131.73213 110.09939
#> 92  138.47803 117.19847
#> 93  142.20164 120.98531
#> 94  143.82263 126.78703
#> 95  139.55915 117.85221
#> 96  148.79798 122.83179
#> 97  144.72221 152.03910
#> 98  159.57421 146.28045
#> 99  169.98063 135.32650
#> 100 168.38023 135.40933
```
