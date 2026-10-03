# exuber: Econometric Analysis of Explosive Time Series

Testing for and dating periods of explosive dynamics (exuberance) in
time series using the univariate and panel recursive unit root tests
proposed by Phillips et al. (2015)
[doi:10.1111/iere.12132](https://doi.org/10.1111/iere.12132) and
Pavlidis et al. (2016)
[doi:10.1007/s11146-015-9531-2](https://doi.org/10.1007/s11146-015-9531-2)
. The recursive least-squares algorithm uses the matrix inversion lemma,
so no matrix has to be inverted at each step, which makes the tests much
faster to compute. The package also simulates a variety of periodically
collapsing bubble processes. Details can be found in Vasilopoulos et al.
(2022)
[doi:10.18637/jss.v103.i10](https://doi.org/10.18637/jss.v103.i10) .

## Package options

`exuber.show_progress`

- Should lengthy operations such as
  [`radf_mc_cv()`](https://kvasilopoulos.github.io/exuber/reference/radf_mc_cv.md)
  show a progress bar? Default: TRUE

`exuber.parallel`

- Should lengthy operations use parallel computation? Default: TRUE in
  an interactive session and FALSE otherwise (scripts, knitr and R CMD
  check), because starting workers costs a few seconds. Set it to TRUE
  in a script to opt in. The worker cluster is started once per session
  and reused. The `radf_*_cv()` and `radf_*_distr()` simulation engines
  honor the option
  ([`radf_mc_cv()`](https://kvasilopoulos.github.io/exuber/reference/radf_mc_cv.md),
  [`radf_wb_cv()`](https://kvasilopoulos.github.io/exuber/reference/radf_wb_cv.md),
  [`radf_sb_cv()`](https://kvasilopoulos.github.io/exuber/reference/radf_sb_cv.md),
  [`radf_recovery_cv()`](https://kvasilopoulos.github.io/exuber/reference/radf_recovery_cv.md)
  and
  [`radf_common_cv()`](https://kvasilopoulos.github.io/exuber/reference/radf_common_cv.md)).
  The standalone tests and monitors run serially regardless.

`exuber.ncores`

- How many cores to use for parallel computation. Default: the number of
  system cores minus 1 (2 in a non-interactive session), capped by the
  `MC_CORES` environment variable when it is set.

`exuber.global_seed`

- When set, the seed feeds automatically into all functions that
  generate random numbers. Default: NA

## See also

Useful links:

- <https://kvasilopoulos.github.io/exuber/>

- <https://github.com/kvasilopoulos/exuber>

- Report bugs at <https://github.com/kvasilopoulos/exuber/issues>

## Author

**Maintainer**: Kostas Vasilopoulos <k.vasilopoulo@gmail.com>

Authors:

- Kostas Vasilopoulos <k.vasilopoulo@gmail.com>

- Efthymios Pavlidis <e.pavlidis@lancaster.ac.uk>

- Enrique Martínez-García <emg.economics@gmail.com>

- Simon Spavound <simon.spavound@googlemail.com>
