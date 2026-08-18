# Changelog

## exuber (development version)

New methodologies from the `docs/enhancements/` research programme, each
independently validated against a published number (formula-exact check,
table lookup, or a direct Monte Carlo reproduction of the source paper’s
own theorem) — see `docs/enhancements/README.md` for the full record of
what was checked and how.

#### Volatility-robust tests

- [`radf_sbz_cv()`](https://kvasilopoulos.github.io/exuber/reference/radf_sbz_cv.md)
  — Herwartz & Siedenburg’s WLS/kernel-volatility SBZ test.
- [`radf_kp()`](https://kvasilopoulos.github.io/exuber/reference/radf_kp.md)
  — kernel-purge heteroskedasticity test.
- `radf_wb_cv(..., dist_skew = TRUE)` — Hafner (2020) skewness-corrected
  wild bootstrap.
- [`radf_sign()`](https://kvasilopoulos.github.io/exuber/reference/radf_sign.md)/[`radf_sign_cv()`](https://kvasilopoulos.github.io/exuber/reference/radf_sign_cv.md)
  — Harvey, Leybourne & Zu (2020) sign-based sGSADF, invariant to
  volatility with no bootstrap needed.
- [`ssu_test()`](https://kvasilopoulos.github.io/exuber/reference/ssu_test.md)
  — Kurozumi & Nishi (2025) stochastic explosive-coefficient test
  (minimum-viable subset).
- [`radf_svadf()`](https://kvasilopoulos.github.io/exuber/reference/radf_svadf.md)
  — Sarkar & Wells (2026) SV-ADF asymmetric-threshold dating.
  **Caveat:** the source is a non-peer-reviewed preprint, flagged at
  call time and in
  [`?radf_svadf`](https://kvasilopoulos.github.io/exuber/reference/radf_svadf.md).

#### Dating and root inference

- [`dating_pdc()`](https://kvasilopoulos.github.io/exuber/reference/dating_pdc.md)
  — PDC/KS sequential sample-splitting dating, plus `type = "wls"` for
  Kurozumi & Skrobotov (2023)’s time-varying-volatility correction.
- [`radf_recovery()`](https://kvasilopoulos.github.io/exuber/reference/radf_recovery.md)/[`radf_recovery_cv()`](https://kvasilopoulos.github.io/exuber/reference/radf_recovery_cv.md)
  — Phillips & Shi (2014) reverse-regression crisis-origination/recovery
  dating. **Caveat:** `f_c` and the overall false-detection rate are
  exploratory pending further validation, flagged at call time and in
  [`?radf_recovery`](https://kvasilopoulos.github.io/exuber/reference/radf_recovery.md).
- [`dating_hls()`](https://kvasilopoulos.github.io/exuber/reference/dating_hls.md)
  — Harvey, Leybourne & Sollis (2017) SSR/BIC single-bubble dating.
- [`dating_hlw()`](https://kvasilopoulos.github.io/exuber/reference/dating_hlw.md)
  — Harvey, Leybourne & Whitehouse (2020) SSR/BIC multi-bubble two-step
  wrapper.
- [`dating_knp()`](https://kvasilopoulos.github.io/exuber/reference/dating_knp.md)
  — Kejriwal, Nguyen & Perron (2025) bias-corrected dating.

#### Real-time monitoring

- [`monitor_radf()`](https://kvasilopoulos.github.io/exuber/reference/monitor_radf.md)
  — Phillips & Shi (2020) training/monitoring orchestration (Family A),
  plus Kurozumi (2020) closed-form `SADF`/ `GSADF_s0` boundaries and
  Homm & Breitung (2012)’s FLUC boundary.
- [`monitor_cusum()`](https://kvasilopoulos.github.io/exuber/reference/monitor_cusum.md)
  — Homm & Breitung (2012) CUSUM monitoring, plus Astill et al. (2023)’s
  volatility-robust CUSUMV kernel variant and HB’s finite-sample
  boundary.
- [`lbi_test()`](https://kvasilopoulos.github.io/exuber/reference/lbi_test.md)/[`monitor_lbi()`](https://kvasilopoulos.github.io/exuber/reference/monitor_lbi.md)
  — Breitung & Diegel (2025) static LBI test and its sequential
  mCUSUM/wCUSUM extension.

#### Multivariate / panel tests

- [`radf_common()`](https://kvasilopoulos.github.io/exuber/reference/radf_common.md)/[`radf_common_cv()`](https://kvasilopoulos.github.io/exuber/reference/radf_common_cv.md)
  — Chen, Phillips & Shi common-bubble detection (PCA + PSY).
- [`cobubble_test()`](https://kvasilopoulos.github.io/exuber/reference/cobubble_test.md)
  — Evripidou, Harvey, Leybourne & Sollis (2022) co-explosive test.
- [`contagion_reg()`](https://kvasilopoulos.github.io/exuber/reference/contagion_reg.md)
  — Greenaway-McGrevy & Phillips (2016) bubble contagion regression
  (minimum-viable subset).

#### Alternative paradigms

- [`quantile_test()`](https://kvasilopoulos.github.io/exuber/reference/quantile_test.md)
  — Wu, Shi & Wu (2025) quantile-based global test.
- [`monitor_quantile()`](https://kvasilopoulos.github.io/exuber/reference/monitor_quantile.md)
  — Wu, Shi & Wu (2025) QPWY recursive quantile monitoring.

#### Naming

- 12 of the functions above (`cobubble_test`, `contagion_reg`,
  `monitor_cusum`, `dating_hls`, `dating_hlw`, `dating_knp`, `lbi_test`,
  `monitor_lbi`, `dating_pdc`, `monitor_quantile`, `quantile_test`,
  `ssu_test`) were named `radf_*` in earlier development snapshots of
  this unreleased version; renamed before release since none of them are
  actually recursive-ADF-based tests. No deprecated aliases were kept,
  as the old names never shipped in a CRAN release.
- `radf_monitor()` renamed to
  [`monitor_radf()`](https://kvasilopoulos.github.io/exuber/reference/monitor_radf.md),
  joining
  [`monitor_cusum()`](https://kvasilopoulos.github.io/exuber/reference/monitor_cusum.md)/
  [`monitor_lbi()`](https://kvasilopoulos.github.io/exuber/reference/monitor_lbi.md)/[`monitor_quantile()`](https://kvasilopoulos.github.io/exuber/reference/monitor_quantile.md)
  under one discoverable prefix for every real-time monitoring function,
  ADF-family internals notwithstanding — see
  [`vignette("naming-and-analysis")`](https://kvasilopoulos.github.io/exuber/articles/naming-and-analysis.md).
- [`exuber_functions()`](https://kvasilopoulos.github.io/exuber/reference/exuber_functions.md)
  added: a queryable registry of every exported function’s family
  (`adf`, `test`, `dating`, `monitor`, `root`, `regression`), so “what
  monitoring functions exist” is an actual function call, not a naming
  convention to memorize.

#### Other

- [`rootstamp()`](https://kvasilopoulos.github.io/exuber/reference/rootstamp.md)
  — confidence interval and doubling time on the explosive root, via S3
  dispatch: the default method fits a single sub-sample, the `radf_obj`
  method runs every
  [`datestamp()`](https://kvasilopoulos.github.io/exuber/reference/datestamp.md)
  episode at once (previously three separate functions –
  `explosive_root()`, `root_ci()`, `root_ci_datestamp()` – consolidated
  before release).

## exuber 1.1.0

CRAN release: 2025-08-31

- Fixed targets not in the package itself nor in the base packages to
  use package anchors, i.e., use

## exuber 1.0.1

CRAN release: 2023-02-12

Maintenance release to accommodate breaking changes in dplyr 1.1.0.

## exuber 1.0.0

CRAN release: 2022-08-19

This first major release accompanies the publication of an article in
the Journal of Statistical Software:

Vasilopoulos, K., Pavlidis, E., & Martínez-García, E. (2022). exuber:
Recursive Right-Tailed Unit Root Testing with R. Journal of Statistical
Software, 103(1), 1–26. <https://doi.org/10.18637/jss.v103.i10>

#### `augment` method for `radf_obj` and `radf_cv`

- New arg `trunc`

- Fixed inconsistencies among functions.

- Now radf stores the data that are later can be accessed with `mat`+

- Advanced features on datestamping: New columns that indicate:

  - Signal
  - Peak
  - Ongoing
  - Nonrejected

- New datestamping procedure `rev_radf` etc.

- New bootstrap procedure `radf_wb_cv2` and `radf_wb_distr2`

- New coloring convention for plotting `ds` and `obj` classes

### Bug Fixes

- Now autoplot can include periods that have an ongoing bubble

## exuber 0.4.2

CRAN release: 2020-12-18

- Include printing methods for `radf_obj` and `radf_cv`.
- Removed unused class definitions.
- Using `progress` package for progress_bar.

## exuber 0.4.1

CRAN release: 2020-05-12

Maintenance release for compatibility with dplyr v1.0.0.

## exuber 0.4.0

CRAN release: 2020-05-04

### Design

We have the following design in mind for future scalability. If you want
make inference about `radf` models, then the estimation can be achieved
with
[`radf()`](https://kvasilopoulos.github.io/exuber/reference/radf.md)
function and return an object of class `radf_obj`, and the critical
values can be achieved with `radf_*_cv()` and return an object of class
`radf_cv`.

### Breaking changes

- [`autoplot()`](https://ggplot2.tidyverse.org/reference/autoplot.html)
  for `radf` models has been refactored and new features have been added
  for more flexibility and conformity with the {ggplot} mindset.
- Because of the change in `autoplot`,
  [`ggarrange()`](https://kvasilopoulos.github.io/exuber/reference/exuber-defunct.md)
  is now defunct.
- [`fortify()`](https://ggplot2.tidyverse.org/reference/fortify.html)
  methods have been replaced by
  [`tidy()`](https://generics.r-lib.org/reference/tidy.html),
  [`augment()`](https://generics.r-lib.org/reference/augment.html),
  [`tidy_join()`](https://kvasilopoulos.github.io/exuber/reference/tidy_join.md)
  and `glance_join()` methods.
  [`fortify()`](https://ggplot2.tidyverse.org/reference/fortify.html)
  methods are now defunct.
- Also `glance()` is now defunct. The user can use
  [`tidy()`](https://generics.r-lib.org/reference/tidy.html) with
  `panel=TRUE` instead.
- Changed the names of:
  - [`mc_cv()`](https://kvasilopoulos.github.io/exuber/reference/exuber-deprecated.md)
    to
    [`radf_mc_cv()`](https://kvasilopoulos.github.io/exuber/reference/radf_mc_cv.md).
    [`mc_cv()`](https://kvasilopoulos.github.io/exuber/reference/exuber-deprecated.md)
    is now deprecated.
  - `mc_distr()` to
    [`radf_mc_distr()`](https://kvasilopoulos.github.io/exuber/reference/radf_mc_cv.md).
    `mc_distr()` is now deprecated.
  - [`wb_cv()`](https://kvasilopoulos.github.io/exuber/reference/exuber-deprecated.md)
    to
    [`radf_wb_cv()`](https://kvasilopoulos.github.io/exuber/reference/radf_wb_cv.md).
    [`wb_cv()`](https://kvasilopoulos.github.io/exuber/reference/exuber-deprecated.md)
    is now deprecated.
  - `wb_distr()` to
    [`radf_wb_distr()`](https://kvasilopoulos.github.io/exuber/reference/radf_wb_cv.md).
    `wb_distr()` is now deprecated.
  - [`sb_cv()`](https://kvasilopoulos.github.io/exuber/reference/exuber-deprecated.md)
    to
    [`radf_sb_cv()`](https://kvasilopoulos.github.io/exuber/reference/radf_sb_cv.md).
    [`sb_cv()`](https://kvasilopoulos.github.io/exuber/reference/exuber-deprecated.md)
    is now deprecated.
  - `sb_distr()` to
    [`radf_sb_distr()`](https://kvasilopoulos.github.io/exuber/reference/radf_sb_cv.md).
    `sb_distr()` is now deprecated.
  - `crit` dataset to `radf_crit`.
  - [`col_names()`](https://kvasilopoulos.github.io/exuber/reference/exuber-deprecated.md)
    to
    [`series_names()`](https://kvasilopoulos.github.io/exuber/reference/series_names.md).
    [`col_names()`](https://kvasilopoulos.github.io/exuber/reference/exuber-deprecated.md)
    is now deprecated.

### exuberdata

- We created a new package called `exuberdata` that accommodates
  critical values for up to 2000 observations. Critical values can be
  examined with `exuberdata::radf_crit2`. The package is created through
  `drat` R archive Template, and can be easily installed with
  `install.packages('exuberdata', repos = 'https://kvasilopoulos.github.io/drat/', type = 'source')`
  or through `install_exuberdata` wrapper function that is provided in
  `exuber`.

### Improvements

- The package `zoo` has been used as a dependency to import the method
  [`index()`](https://kvasilopoulos.github.io/exuber/reference/index-rd.md).
  We made the decision to remove `zoo` and create a new method
  [`index()`](https://kvasilopoulos.github.io/exuber/reference/index-rd.md)
  internally.

## exuber 0.3.0

CRAN release: 2019-07-15

### Breaking changes

- Changed `opt_bsadf = conservative` for the simulated critical values
  (`crit`), also reduced the size of the `crit` from 700 to 600 due to
  package size restrictions.
- [`sim_dgp1()`](https://kvasilopoulos.github.io/exuber/reference/exuber-defunct.md)
  and
  [`sim_dgp2()`](https://kvasilopoulos.github.io/exuber/reference/exuber-defunct.md)
  have been renamed to
  [`sim_psy1()`](https://kvasilopoulos.github.io/exuber/reference/sim_psy1.md)
  and
  [`sim_psy2()`](https://kvasilopoulos.github.io/exuber/reference/sim_psy2.md)
  to better describe the origination of the dgp.
- [`sim_dgp1()`](https://kvasilopoulos.github.io/exuber/reference/exuber-defunct.md)
  and
  [`sim_dgp2()`](https://kvasilopoulos.github.io/exuber/reference/exuber-defunct.md)
  have been soft-deprecated.
- `autoplot_radf()` arranges automatically multiple graphs, to return to
  previous behavior we included the optional argument `arrange` which is
  set to TRUE by default.

Three new functions have been added to simulate empirical distributions
for:

- `mc_dist()`: Monte Carlo
- `wb_dist()`: Wild Bootstrap
- `sb_dist()`: Sieve Bootstrap

and a function that can calculate the p-values
[`calc_pvalue()`](https://kvasilopoulos.github.io/exuber/reference/calc_pvalue.md)
given the above distributions as argument.

Also methods [`tidy()`](https://generics.r-lib.org/reference/tidy.html)
and
[`autoplot()`](https://ggplot2.tidyverse.org/reference/autoplot.html)
have been added to turn the object into a tidy tibble and draw a
particular plot with ggplot2, respectively.

### New features

- [`tidy()`](https://generics.r-lib.org/reference/tidy.html) methods for
  objects of class `radf`, `cv`.
- [`augment()`](https://generics.r-lib.org/reference/augment.html)
  methods for objects of class `radf` and `cv`.
- [`augment_join()`](https://kvasilopoulos.github.io/exuber/reference/tidy_join.md)
  to combine object `radf` and `cv` into a single data.frame.
- `glance()` method for objects of class `radf`.

### Improvements

- New printing output for the functions
  [`summary()`](https://rdrr.io/r/base/summary.html),
  [`diagnostics()`](https://kvasilopoulos.github.io/exuber/reference/diagnostics.md)
  and
  [`datestamp()`](https://kvasilopoulos.github.io/exuber/reference/datestamp.md).
- New improved progressbar with more succinct printing for
  [`wb_cv()`](https://kvasilopoulos.github.io/exuber/reference/exuber-deprecated.md)
- `seed` argument to functions that are using rng. Also the option to
  declare a global seed for reproducibility with the
  `option(exuber.global_seed = ###)`

### Bug Fixes

- [`sb_cv()`](https://kvasilopoulos.github.io/exuber/reference/exuber-deprecated.md)
  and
  [`wb_cv()`](https://kvasilopoulos.github.io/exuber/reference/exuber-deprecated.md)now
  can parse data that contain a date-column. Similarly, to what
  [`radf()`](https://kvasilopoulos.github.io/exuber/reference/radf.md)
  is doing.

## exuber 0.2.1.9000

- Website development

## exuber 0.2.1

CRAN release: 2019-03-01

- Changed DESCRIPTION to include `sb_cv` reference.
- Renamed boolean to dummy from `datestamp` and `diagnostics`.
- `datestamp` dummy is now an attribute.

## exuber 0.2.0

CRAN release: 2019-02-04

### Options

Some of the arguments in the functions were included as options, you can
set the package options with
e.g. `options(exuber.show_progress = TRUE)`.

- `parallel` option boolean, allows for parallel in critical values
  computation.
- `ncores` option numeric, sets the number of cores, defaults to max -
  1.
- `show_progress` option boolean, allows you to disable the progress
  bar, defaults to TRUE.

### New features

- Panel estimation in
  [`radf()`](https://kvasilopoulos.github.io/exuber/reference/radf.md)
- Added
  [`sb_cv()`](https://kvasilopoulos.github.io/exuber/reference/exuber-deprecated.md)
  function: Panel Sieve Bootstrapped critical values
- Default critical values are supplied directly into
  [`summary()`](https://rdrr.io/r/base/summary.html), `diagnostics`,
  [`datestamp()`](https://kvasilopoulos.github.io/exuber/reference/datestamp.md)
  and
  [`autoplot()`](https://ggplot2.tidyverse.org/reference/autoplot.html),
  without having to specify argument cv. The critical values have been
  simulated from
  [`mc_cv()`](https://kvasilopoulos.github.io/exuber/reference/exuber-deprecated.md)
  function and stored as data. Custom critical values should be provided
  by the user with the option `cv`.
- Added
  [`ggarrange()`](https://kvasilopoulos.github.io/exuber/reference/exuber-defunct.md)
  function, that can arrange a list of ggplot objects into a single
  grob.
- Added `fortify` to arrange a data.frame from
  [`radf()`](https://kvasilopoulos.github.io/exuber/reference/radf.md)
  function.

### Improvements

- Parallel and ncores arguments are now set as options.
- Ability to remove progressbar from package options.
- [`radf()`](https://kvasilopoulos.github.io/exuber/reference/radf.md)
  can parse date from `ts` objects.
- [`report()`](https://kvasilopoulos.github.io/exuber/reference/exuber-defunct.md)
  has been renamed into
  [`summary()`](https://rdrr.io/r/base/summary.html).
- [`plot()`](https://rdrr.io/r/graphics/plot.default.html) has been
  renamed into
  [`autoplot()`](https://ggplot2.tidyverse.org/reference/autoplot.html).
- [`plot()`](https://rdrr.io/r/graphics/plot.default.html) and
  [`report()`](https://kvasilopoulos.github.io/exuber/reference/exuber-defunct.md)
  are soft deprecated.

### Bug Fixes

- Progressbar appears in the beginning of the iteration
- Plotting date now works without having to to include any additional
  plotting option
