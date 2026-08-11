# exuber: Econometric Analysis of Explosive Time Series

Testing for and dating periods of explosive dynamics (exuberance) in
time series using the univariate and panel recursive unit root tests
proposed by Phillips et al. (2015)
[doi:10.1111/iere.12132](https://doi.org/10.1111/iere.12132) and
Pavlidis et al. (2016)
[doi:10.1007/s11146-015-9531-2](https://doi.org/10.1007/s11146-015-9531-2)
.The recursive least-squares algorithm utilizes the matrix inversion
lemma to avoid matrix inversion which results in significant speed
improvements. Simulation of a variety of periodically-collapsing bubble
processes. Details can be found in Vasilopoulos et al. (2022)
[doi:10.18637/jss.v103.i10](https://doi.org/10.18637/jss.v103.i10) .

## Package options

`exuber.show_progress`

- Should lengthy operations such as
  [`radf_mc_cv()`](https://kvasilopoulos.github.io/exuber/reference/radf_mc_cv.md)
  show a progress bar? Default: TRUE

`exuber.parallel`

- Should lengthy operations use parallel computation? Default: TRUE

`exuber.ncores`

- How many cores to use for parallel computation. Default: system
  cores - 1

`exuber.global_seed`

- When chosen automatically feeds into all functions with random-number
  generation. Default: NA

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
