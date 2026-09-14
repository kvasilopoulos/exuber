# exuber 2.0.0

New methodologies from the `docs/enhancements/` research programme, each
independently validated against a published number (formula-exact check,
table lookup, or a direct Monte Carlo reproduction of the source paper's
own theorem) — see `docs/enhancements/README.md` for the full record of
what was checked and how.

### Critical values

* Default critical values now come from a shared precomputed store instead
  of the bundled `radf_crit` dataset: lag 0-4, every `n` from the smallest
  the PSY window allows up to 4000, 2000 replications each. `radf()`
  followed by `summary()`/`datestamp()`/`autoplot()` fetches the one
  `(n, lag)` table it needs on first use and caches it on disk
  (`tools::R_user_dir("exuber", "cache")`), so a lagged specification no
  longer requires simulating your own `cv`. The tables are *nested* (every
  `n` of a lag from the same seeded paths); values differ from the old
  bundled ones by Monte Carlo noise. Needs network access the first time a
  given `(n, lag)` is used; `radf_mc_cv()`/`radf_wb_cv()` remain the
  offline route.
* `radf_crit` (the bundled lag-0, n <= 600 table) is removed, along with
  its `print.crit` method and `data-raw/sim-crit.R`; the simulation now
  lives in the sibling `exubercrit` repository.

### Volatility-robust tests

* `radf_sbz()`/`radf_sbz_cv()`/`radf_sbz_union()` — Herwartz & Siedenburg's
  WLS/kernel-volatility SBZ test, split into a statistic
  (`radf_sbz()`), its bootstrap critical values (`radf_sbz_cv()`, full
  `datestamp()`/`autoplot()` support), and the union-of-rejections test
  against classic `supDF` (`radf_sbz_union()`, renamed from the original
  bundled `radf_sbz_cv()` — see `vignette("volatility-robust-radf")`.
* `radf_kp()` — kernel-purge heteroskedasticity test.
* `radf_wb_cv(..., dist_skew = TRUE)` — Hafner (2020) skewness-corrected
  wild bootstrap.
* `radf_sign()`/`radf_sign_cv()` — Harvey, Leybourne & Zu (2020) sign-based
  sGSADF, invariant to volatility with no bootstrap needed.
* `ssu_test()` — Kurozumi & Nishi (2025) stochastic explosive-coefficient
  test (minimum-viable subset).
* `datestamp(..., option = "svadf")` — Sarkar & Wells (2026) SV-ADF
  asymmetric-threshold dating, folded into `datestamp()` rather than
  shipped as a separate `radf_svadf()` entry point. **Caveat:** the
  source is a non-peer-reviewed preprint, flagged at call time and in
  `?datestamp`, Caveats section.

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

* `monitor()` — Phillips & Shi (2020) training/monitoring
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
* `radf_monitor()` renamed to `monitor()` (via a brief intermediate
  `monitor_radf()`, never released), the flagship of the real-time
  monitoring family alongside `monitor_cusum()`/`monitor_lbi()`/
  `monitor_quantile()` — deliberately carrying no `radf`/`sadf` token so
  it can't be mistaken for a `radf_*()` variant, ADF-family internals
  notwithstanding — see `vignette("naming-and-analysis")`.
* `exuber_functions()` added: a queryable registry of every exported
  function's family (`adf`, `test`, `dating`, `monitor`, `root`,
  `regression`), so "what monitoring functions exist" is an actual
  function call, not a naming convention to memorize.
* `radf_wb_cv2()`/`radf_wb_distr2()` renamed to `radf_wb_ps_cv()`/
  `radf_wb_ps_distr()` -- the `2` suffix named nothing (just "the second
  wild bootstrap added"); `_ps` identifies it as Phillips & Shi (2020)'s
  wild bootstrap (fits a null AR model, resamples residuals, supports a
  `tb` training-window boundary), as opposed to `radf_wb_cv()`'s Harvey
  et al. (2016) non-parametric multiplier bootstrap, matching this file's
  own internal naming (`radf_wb_dgp_ps`/`radf_wb_ps` vs.
  `radf_wb_dgp_hlst`/`radf_wb_hlst`) and the package-wide
  `radf_<method>_<qualifier>_cv` pattern (`radf_sign_dm_cv()`). Unlike
  the renames above, `radf_wb_cv2()` shipped in the 1.0.0 release (the
  JSS paper), so this one keeps `radf_wb_cv2()`/`radf_wb_distr2()`
  as deprecated aliases (`.Deprecated()`, warn-and-forward, see
  `?exuber-deprecated`) rather than a clean break.
* `radf_tt_cv()`, `radf_sign_cv()`, and `radf_sign_dm_cv()` now all
  compute `badf_cv`/`bsadf_cv` (a time-varying boundary), not just the
  three scalar critical values — `radf_tt()`/`radf_sign()`/
  `radf_sign_dm()` results now work with the full `summary()`/
  `datestamp()`/`tidy()`/`autoplot()` pipeline, not just `summary()`/
  `tidy()`. Found first in `radf_tt_cv()` (a user-reported `datestamp()`
  crash on `radf_sign()` prompted checking all three GLS-demeaned-family
  functions), then confirmed the identical fix applies to the other two.
  Validated per function: `badf_cv`'s last row is bit-identical to
  `adf_cv` (a hard identity), empirical false-alarm rate at or below
  nominal (`radf_tt` 3.3%, `radf_sign` 5.5%, `radf_sign_dm` 3.5%, vs. 5%
  nominal), and detection power on an identical synthetic bubble in the
  same range as the established `radf()`/`radf_mc_cv()` baseline (16%):
  `radf_tt` 18%, `radf_sign` 20%, `radf_sign_dm` 8% (lower power a known,
  expected property of the sign-based tests' heteroskedasticity
  invariance, not a validation concern) — see
  `vignette("naming-and-analysis")`.

### Performance

* `radf()` is ~25x faster on typical sample sizes (exubercore v0.3.0): the
  recursive grid re-formed the full residual vector for every window, an
  O(n^3) total; it now keeps running cross-products and uses the
  closed-form `SSR = y'y - b'X'y`, making it O(n^2). n = 400 drops from
  ~140 ms to ~6 ms per path, so `radf_mc_cv()`, `radf_wb_cv()`,
  `radf_sb_cv()`, `monitor()`, `dating_hlw()`, `radf_recovery()` and every
  other loop over `radf()` speed up by the same factor. Results are
  numerically identical to ~1e-12.
* Parallel runs (`options(exuber.parallel = TRUE)`, the default) reuse one
  worker cluster per session instead of starting and stopping a fresh
  `future::multisession` on every call -- a few seconds of start-up
  overhead that used to dominate every small `radf_mc_cv()`/`radf_wb_cv()`
  job. The cluster is sized by `exuber.ncores` and stopped when the
  namespace unloads.

### API consistency

* One significance-level convention across the whole package: every
  function that took a `level` argument now takes `sig_lvl` on the
  0-100 scale already used by `datestamp()`/`autoplot()` (`sig_lvl = 95`
  = a 5% test / 95% confidence). Affected (all unreleased, so no shims):
  `lbi_test()`, `monitor_lbi()` (`0.95` -> `95`, `0.975` -> `97.5`, ...),
  `ssu_test()`, `monitor()`, `monitor_cusum()`, `rootstamp()` (was a
  `0.95`-style confidence level), `quantile_test()`/`monitor_quantile()`
  (already 0-100, renamed only), and `cobubble_test()` (was a *size*,
  `level = 0.05`; now `sig_lvl = 95`). A shared `assert_sig_lvl()` makes
  `sig_lvl = 0.95` an immediate error everywhere rather than a silently
  wrong quantile.
* `monitor(adflag = )` -> `lag`, matching `radf()`; `monitor_cusum(N = )`
  -> `h`, matching every other kernel-bandwidth argument;
  `cobubble_test(lags = )` -> `lag_grid`, so `lag`/`lags` can no longer be
  confused (mirrors `quantile_test()`'s `tau`/`tau_grid`).
* `cobubble_test()` and `radf_sbz_union()` now return `cobubble_test_obj`/
  `radf_sbz_union_obj`, the `_obj` suffix every other standalone class
  already carried.
* `dating_pdc()` and `radf_sbz()` gained their own `print()` methods (the
  former fell through to `print.data.frame`, hiding its attributes; the
  latter printed as a plain `radf`).
* `scale_exuber_manual(size_values = )` is deprecated in favor of
  `linewidth_values` (ggplot2 >= 3.4.0's `linewidth` aesthetic replaces
  `size` for lines; the deprecation warning ggplot2 emitted from every
  `autoplot()` is gone). `autoplot(include_negative = )`, deprecated since
  1.0.0, is now actually forwarded to `nonrejected` instead of ignored.
* `radf_tt()`/`radf_tt_cv()`/`monitor_quantile()` carry the same
  experimental badge as the other new methods; the "does not plug into
  `autoplot`" note on every standalone function was wrong (each has had
  its own `autoplot()` method) and now says so.

### Other

* `rootstamp()` — confidence interval and doubling time on the explosive
  root, via S3 dispatch: the default method fits a single sub-sample, the
  `radf_obj` method runs every `datestamp()` episode at once (previously
  three separate functions -- `explosive_root()`, `root_ci()`,
  `root_ci_datestamp()` -- consolidated before release).

* Documentation: every `autoplot()` and `augment()` method now has its own
  help page instead of sharing one with the function it plots/tidies
  (`?autoplot.monitor_cusum_obj`, `?augment.radf_obj`, ...). The pkgdown
  reference index is reorganized into per-function subsections so each
  function is listed next to the methods that consume its output.

* `sim_vol_break()` -- i.i.d. Gaussian innovations whose standard deviation
  shifts permanently at a chosen break fraction, the non-stationary
  volatility DGP (Cavaliere & Taylor 2007) the volatility-robust tests are
  actually built for, unlike stationary GARCH. `sim_ps1()` gained the same
  `e` innovation-injection argument `sim_psy1()` already had, and the
  `c`/`c1`/`c2` arguments of `sim_psy1()`/`sim_ps1()` now accept any
  positive scalar (as documented), so a fixed explosive root is expressible
  directly (`c = 0.04, alpha = 0`).

* Examples and vignettes now demonstrate each method on the DGP it targets
  rather than on `sim_data` throughout: volatility-robust tests
  (`radf_tt()`, `radf_kp()`, `radf_sign()`, `radf_sbz()`, `radf_wb_cv()`,
  `monitor_cusum(type = "kernel")`, `dating_pdc(type = "wls")`) on a
  volatility break, `cobubble_test()`/`contagion_reg()` on
  `sim_coexplosive()`, `radf_common()` on `sim_common()`, `ssu_test()` on a
  stochastic root, `quantile_test()`/`monitor_quantile()` on heavy-tailed
  innovations, `dating_*()`/`radf_recovery()` on `sim_ps1()`, and every
  monitor on a bubble that starts after its training window. Hand-rolled
  `cumsum(rnorm())` DGPs in the vignettes are replaced by the package's own
  generators.

### Bug fixes

* `datestamp()`/`autoplot()`/`autoplot2()`'s `sig_lvl` argument now
  actually controls whether a series counts as rejecting the null.
  Previously, `diagnostics.radf_obj()` (used internally to decide which
  series get dated/plotted at all) hard-coded the 95% critical value for
  that decision regardless of `sig_lvl`, so e.g. `datestamp(x, cv,
  sig_lvl = 90)` could throw `"Cannot reject H0 at the 5% significance
  level"` for a series that clearly rejects at the 10% level the caller
  asked for -- `sig_lvl` only ever reached the within-series episode
  threshold curve, never the series-eligibility gate. `diagnostics()`
  gained a `sig_lvl` argument (default 95, so default-call behavior is
  unchanged) and `datestamp()`/`autoplot()`/`autoplot2()` now thread
  their own `sig_lvl` through to it.

* `radf()` (and everything built on it) errored with `subscript out of
  bounds` on a *named* numeric vector, e.g. a `prcomp()` score column --
  which is exactly what `radf_common()` feeds it -- because the names
  leaked into the internal NA-edge bookkeeping.

* The default-`cv` range check said the store covers `n <= 5000`; it
  covers `n <= 4000` and `lag <= 4`, and now says so before trying the
  network.

* `sim_psy1()`'s `seed` argument now also covers a generator passed lazily
  to `e`/`coef_noise` (e.g. `sim_psy1(n, seed = 1, e = sim_vol_break(n - 1))`);
  previously those were forced before the seed was set.

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
