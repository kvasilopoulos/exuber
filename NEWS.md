# exuber (development version)

New methodologies from the `docs/enhancements/` research programme, each
independently validated against a published number (formula-exact check,
table lookup, or a direct Monte Carlo reproduction of the source paper's
own theorem) — see `docs/enhancements/README.md` for the full record of
what was checked and how.

### Volatility-robust tests

* `radf_sbz_cv()` — Herwartz & Siedenburg's WLS/kernel-volatility SBZ test.
* `radf_kp()` — kernel-purge heteroskedasticity test.
* `radf_wb_cv(..., dist_skew = TRUE)` — Hafner (2020) skewness-corrected
  wild bootstrap.
* `radf_sign()`/`radf_sign_cv()` — Harvey, Leybourne & Zu (2020) sign-based
  sGSADF, invariant to volatility with no bootstrap needed.
* `ssu_test()` — Kurozumi & Nishi (2025) stochastic explosive-coefficient
  test (minimum-viable subset).
* `radf_svadf()` — Sarkar & Wells (2026) SV-ADF asymmetric-threshold
  dating. **Caveat:** the source is a non-peer-reviewed preprint, flagged
  at call time and in `?radf_svadf`.

### Dating and root inference

* `dating_pdc()` — PDC/KS sequential sample-splitting dating, plus
  `type = "wls"` for Kurozumi & Skrobotov (2023)'s time-varying-volatility
  correction.
* `radf_recovery()`/`radf_recovery_cv()` — Phillips & Shi (2014)
  reverse-regression crisis-origination/recovery dating. **Caveat:** `f_c`
  and the overall false-detection rate are exploratory pending further
  validation, flagged at call time and in `?radf_recovery`.
* `dating_hls()` — Harvey, Leybourne & Sollis (2017) SSR/BIC single-bubble
  dating.
* `dating_hlw()` — Harvey, Leybourne & Whitehouse (2020) SSR/BIC
  multi-bubble two-step wrapper.
* `dating_knp()` — Kejriwal, Nguyen & Perron (2025) bias-corrected dating.

### Real-time monitoring

* `monitor_radf()` — Phillips & Shi (2020) training/monitoring
  orchestration (Family A), plus Kurozumi (2020) closed-form `SADF`/
  `GSADF_s0` boundaries and Homm & Breitung (2012)'s FLUC boundary.
* `monitor_cusum()` — Homm & Breitung (2012) CUSUM monitoring, plus Astill
  et al. (2023)'s volatility-robust CUSUMV kernel variant and HB's
  finite-sample boundary.
* `lbi_test()`/`monitor_lbi()` — Breitung & Diegel (2025) static LBI
  test and its sequential mCUSUM/wCUSUM extension.

### Multivariate / panel tests

* `radf_common()`/`radf_common_cv()` — Chen, Phillips & Shi common-bubble
  detection (PCA + PSY).
* `cobubble_test()` — Evripidou, Harvey, Leybourne & Sollis (2022)
  co-explosive test.
* `contagion_reg()` — Greenaway-McGrevy & Phillips (2016) bubble
  contagion regression (minimum-viable subset).

### Alternative paradigms

* `quantile_test()` — Wu, Shi & Wu (2025) quantile-based global test.
* `monitor_quantile()` — Wu, Shi & Wu (2025) QPWY recursive quantile
  monitoring.

### Naming

* 12 of the functions above (`cobubble_test`, `contagion_reg`,
  `monitor_cusum`, `dating_hls`, `dating_hlw`, `dating_knp`, `lbi_test`,
  `monitor_lbi`, `dating_pdc`, `monitor_quantile`, `quantile_test`,
  `ssu_test`) were named `radf_*` in earlier development snapshots of
  this unreleased version; renamed before release since none of them are
  actually recursive-ADF-based tests. No deprecated aliases were kept, as
  the old names never shipped in a CRAN release.
* `radf_monitor()` renamed to `monitor_radf()`, joining `monitor_cusum()`/
  `monitor_lbi()`/`monitor_quantile()` under one discoverable prefix for
  every real-time monitoring function, ADF-family internals
  notwithstanding — see `vignette("naming-and-analysis")`.
* `exuber_functions()` added: a queryable registry of every exported
  function's family (`adf`, `test`, `dating`, `monitor`, `root`,
  `regression`), so "what monitoring functions exist" is an actual
  function call, not a naming convention to memorize.

### Other

* `rootstamp()` — confidence interval and doubling time on the explosive
  root, via S3 dispatch: the default method fits a single sub-sample, the
  `radf_obj` method runs every `datestamp()` episode at once (previously
  three separate functions -- `explosive_root()`, `root_ci()`,
  `root_ci_datestamp()` -- consolidated before release).

# exuber 1.1.0

* Fixed \link{} targets not in the package itself nor in the base packages to
use package anchors, i.e., use \link[PKG]{FOO}

# exuber 1.0.1

Maintenance release to accommodate breaking changes in dplyr 1.1.0.

# exuber 1.0.0

This first major release accompanies the publication of an article in the Journal of Statistical Software:

Vasilopoulos, K., Pavlidis, E., & Martínez-García, E. (2022). exuber: Recursive Right-Tailed Unit Root Testing with R. Journal of Statistical Software, 103(1), 1–26. https://doi.org/10.18637/jss.v103.i10


### `augment` method for `radf_obj` and `radf_cv`
* New arg `trunc`
* Fixed inconsistencies among functions.

* Now radf stores the data that are later can be accessed with `mat`+
* Advanced features on datestamping: New columns that indicate:
  - Signal
  - Peak
  - Ongoing
  - Nonrejected
* New datestamping procedure `rev_radf` etc.
* New bootstrap procedure `radf_wb_cv2` and `radf_wb_distr2`
* New coloring convention for plotting `ds` and `obj` classes

## Bug Fixes

* Now autoplot can include periods that have an ongoing bubble

# exuber 0.4.2

* Include printing methods for `radf_obj` and `radf_cv`.
* Removed unused class definitions.
* Using `progress` package for progress_bar.

# exuber 0.4.1

Maintenance release for compatibility with dplyr v1.0.0.

# exuber 0.4.0

## Design

We have the following design in mind for future scalability. If you want make inference about `radf` models, then the estimation can be achieved with `radf()` function and return an object of class `radf_obj`, and the critical values can be achieved with `radf_*_cv()` and return an object of class `radf_cv`.

## Breaking changes

* `autoplot()` for `radf` models has been refactored and new features have been added for more flexibility and conformity with the {ggplot} mindset.
* Because of the change in `autoplot`, `ggarrange()` is now defunct.
* `fortify()` methods have been replaced by `tidy()`, `augment()`, `tidy_join()` and `glance_join()` methods. `fortify()` methods are now defunct.
* Also `glance()` is now defunct. The user can use `tidy()` with `panel=TRUE` instead.
* Changed the names of:
  - `mc_cv()` to `radf_mc_cv()`. `mc_cv()` is now deprecated.
  - `mc_distr()` to `radf_mc_distr()`. `mc_distr()` is now deprecated.
  - `wb_cv()` to `radf_wb_cv()`. `wb_cv()` is now deprecated.
  - `wb_distr()` to `radf_wb_distr()`. `wb_distr()` is now deprecated.
  - `sb_cv()` to `radf_sb_cv()`. `sb_cv()` is now deprecated.
  - `sb_distr()` to `radf_sb_distr()`. `sb_distr()` is now deprecated.
  - `crit` dataset to `radf_crit`.
  - `col_names()` to `series_names()`.  `col_names()` is now deprecated.

## exuberdata

* We created a new package called `exuberdata` that accommodates critical values for up to 2000 observations. Critical values can be examined with `exuberdata::radf_crit2`. The package is created through `drat` R archive Template, and can be easily installed with `install.packages('exuberdata', repos = 'https://kvasilopoulos.github.io/drat/', type = 'source')` or through `install_exuberdata` wrapper function that is provided in `exuber`.

## Improvements

* The package `zoo` has been used as a dependency to import the method `index()`. 
We made the decision to remove `zoo` and create a new method `index()` internally.

# exuber 0.3.0

## Breaking changes

* Changed `opt_bsadf = conservative` for the simulated critical values (`crit`),
also reduced the size of the `crit` from 700 to 600 due to package size restrictions.
* `sim_dgp1()` and `sim_dgp2()` have been renamed to `sim_psy1()` and `sim_psy2()` 
to better describe the origination of the dgp. 
* `sim_dgp1()` and `sim_dgp2()` have been soft-deprecated.
* `autoplot_radf()` arranges automatically multiple graphs, to return to previous
behavior we included the optional argument `arrange` which is set to TRUE by default.

Three new functions have been added to simulate empirical distributions for:

* `mc_dist()`: Monte Carlo 
* `wb_dist()`: Wild Bootstrap 
* `sb_dist()`: Sieve Bootstrap 

and a function that can calculate the p-values `calc_pvalue()` given the above 
distributions as argument.

Also methods `tidy()` and `autoplot()` have been added to turn the object into
a tidy tibble and draw a particular plot with ggplot2, respectively.

## New features

* `tidy()` methods for objects of class `radf`, `cv`.
* `augment()` methods for objects of class `radf` and `cv`.
* `augment_join()` to combine object `radf` and `cv` into a single data.frame.
* `glance()` method for objects of class `radf`.

## Improvements

* New printing output for the functions `summary()`, `diagnostics()` and 
`datestamp()`.
* New improved progressbar with more succinct printing for `wb_cv()`
* `seed` argument to functions that are using rng. Also the option to declare
a global seed for reproducibility with the `option(exuber.global_seed = ###)`

## Bug Fixes

* `sb_cv()` and `wb_cv()`now can parse data that contain a date-column. Similarly,
to what `radf()` is doing.


# exuber 0.2.1.9000

* Website development

# exuber 0.2.1

* Changed DESCRIPTION to include `sb_cv` reference.
* Renamed boolean to dummy from `datestamp` and `diagnostics`.
* `datestamp` dummy is now an attribute.

# exuber 0.2.0

## Options

Some of the arguments in the functions were included as options, you can
set the package options with e.g. `options(exuber.show_progress = TRUE)`.

* `parallel` option boolean, allows for parallel in critical values computation.
* `ncores` option numeric, sets the number of cores, defaults to max - 1.
* `show_progress` option boolean, allows you to disable the progress bar, defaults to TRUE.

## New features

* Panel estimation in `radf()`
* Added `sb_cv()` function: Panel Sieve Bootstrapped critical values
* Default critical values are supplied directly into `summary()`, `diagnostics`,
  `datestamp()` and `autoplot()`, without having to specify argument cv. The 
  critical values have been simulated from `mc_cv()` function and stored as data.
  Custom critical values should be provided by the user with the option `cv`.
* Added `ggarrange()` function, that can arrange a list of ggplot objects into a single grob.
* Added `fortify` to arrange a data.frame from `radf()` function.

## Improvements

* Parallel and ncores arguments are now set as options.
* Ability to remove progressbar from package options.
* `radf()` can parse date from `ts` objects.
* `report()` has been renamed into `summary()`.
* `plot()` has been renamed into `autoplot()`.
* `plot()` and `report()` are soft deprecated.

## Bug Fixes

* Progressbar appears in the beginning of the iteration
* Plotting date now works without having to to include any additional plotting option
