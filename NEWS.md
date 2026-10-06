# exuber 2.0.0

This release adds new methods from the `docs/` research programme. We checked each one against a published number, either by a formula-exact check, a table lookup or a direct Monte Carlo reproduction of a theorem in the source paper. `docs/README.md` records what was checked and how.

### Critical values

* Default critical values now come from a shared precomputed store and no longer from the bundled `radf_crit` dataset. The store covers lags 0 to 4 and every `n` from the smallest the PSY window allows up to 4000, with 2000 replications each. When you call `summary()`, `datestamp()` or `autoplot()` on a `radf()` result, the one `(n, lag)` table it needs is fetched on first use and cached on disk (`tools::R_user_dir("exuber", "cache")`). A lagged specification therefore no longer requires you to simulate your own `cv`. The tables are nested, which means that every `n` of a lag is built from the same seeded paths. The values differ from the old bundled ones by Monte Carlo noise. The first use of a given `(n, lag)` needs network access. `radf_mc_cv()` and `radf_wb_cv()` remain available offline.
* `radf_crit`, the bundled lag-0 table for n <= 600, is removed together with its `print.crit` method and `data-raw/sim-crit.R`. The simulation now lives in the sibling `exubercrit` repository.

### Volatility-robust tests

* `radf_sbz()`, `radf_sbz_cv()` and `radf_sbz_union()` implement the WLS/kernel-volatility SBZ test of Herwartz & Siedenburg. The test is split into a statistic (`radf_sbz()`), its bootstrap critical values (`radf_sbz_cv()`, with full `datestamp()` and `autoplot()` support) and the union-of-rejections test against the classic `supDF` (`radf_sbz_union()`). The union test was the original bundled `radf_sbz_cv()` and has been renamed. See `vignette("volatility-robust-radf")`.
* `radf_kp()` is a kernel-purge heteroskedasticity test.
* `radf_wb_cv(..., dist_skew = TRUE)` is the skewness-corrected wild bootstrap of Hafner (2020).
* `radf_sign()` and `radf_sign_cv()` implement the sign-based sGSADF of Harvey, Leybourne & Zu (2020), which is invariant to volatility and needs no bootstrap.
* `ssu_test()` implements the stochastic explosive-coefficient tests of Kurozumi & Nishi (2025): SSU, and GSSU (`type = "gssu"`, the double recursion over window starts). With `union = TRUE` it adds the union of rejections of the paper (UR/GUR) with SADF/GSADF. All critical values come from Table I of the paper.
* `cusum_test()` implements the retrospective CUSUM tests of Kurozumi & Nishi (2025) (`"cs"`, `"gcs"`) and the two-sided CUSUM-of-squares tests (`"cssq"`, `"gcssq"`).
* `datestamp(..., option = "svadf")` implements the SV-ADF asymmetric-threshold dating of Sarkar & Wells (2026). We added it as an option of `datestamp()` and not as a separate `radf_svadf()` function. The source is a preprint that has not been peer reviewed. This caveat is shown when the function is called and in the Caveats section of `?datestamp`.

### Dating and root inference

* `dating_pdc()` implements the PDC/KS sequential sample-splitting dating. `type = "wls"` adds the correction of Kurozumi & Skrobotov (2023) for time-varying volatility.
* `radf_recovery()` and `radf_recovery_cv()` implement the reverse-regression dating of crisis origination and recovery of Phillips & Shi (2014). `f_c` and the overall false-detection rate are exploratory until they are validated further. This is flagged when the function is called and in `?radf_recovery`.
* `dating_hls()` implements the SSR/BIC single-bubble dating of Harvey, Leybourne & Sollis (2017).
* `dating_hlw()` implements the two-step SSR/BIC multi-bubble wrapper of Harvey, Leybourne & Whitehouse (2020), including the run-joining rule of the paper for fragmented step-1 detections (`join = 3`; `0` disables it).
* `dating_knp()` implements the bias-corrected dating of Kejriwal, Nguyen & Perron (2025). The `breaks` argument dates several bubbles at once with the dynamic programme of the paper, which finds the exact global minimizer in `O(breaks * n^2)`.

### Real-time monitoring

* `monitor()` implements the training and monitoring procedure of Phillips & Shi (2020) (Family A). It adds the closed-form `SADF` and `GSADF_s0` boundaries of Kurozumi (2020) and the FLUC boundary of Homm & Breitung (2012).
* `monitor_cusum()` implements the CUSUM monitoring of Homm & Breitung (2012). It adds the volatility-robust CUSUMV kernel variant of Astill et al. (2023) and the finite-sample boundary of Homm & Breitung.
* `lbi_test()` and `monitor_lbi()` implement the static LBI test of Breitung & Diegel (2025) and its sequential mCUSUM/wCUSUM extension.

### Multivariate and panel tests

* `radf_common()` and `radf_common_cv()` implement the common-bubble detection of Chen, Phillips & Shi (PCA followed by PSY).
* `cobubble_test()` implements the co-explosivity test of Evripidou, Harvey, Leybourne & Sollis (2022).
* `contagion_reg()` implements the bubble contagion regression of Greenaway-McGrevy & Phillips (2016), as a minimal subset of the paper.

### Alternative paradigms

* `quantile_test()` implements the quantile-based global test of Wu, Shi & Wu (2025).
* `monitor_quantile()` implements the recursive quantile monitoring of Wu, Shi & Wu (2025): QPWY (expanding window) and QPSY (`type = "qpsy"`, supremum over window starts). The default boundary is the asymptotic one. It is well sized near the median, but away from the median it is oversized in small samples, and QPSY is badly oversized, with a false-alarm rate of 20% to 47% at a nominal 5% in our checks. A caveat message at call time says so. `boundary = "bootstrap"` applies Algorithm 1 of the paper to the whole path: it resamples the centred first differences, cumulates them and recomputes the QPWY or QPSY path, and it takes the quantile of the path maxima. It follows the finite-sample distribution of the statistic and brings the false-alarm rate back to about 5% in most of our checks, at the cost of one full statistic path for each replicate.

### Naming

* Twelve of the functions above were named `radf_*` in earlier development snapshots of this unreleased version: `cobubble_test`, `contagion_reg`, `monitor_cusum`, `dating_hls`, `dating_hlw`, `dating_knp`, `lbi_test`, `monitor_lbi`, `dating_pdc`, `monitor_quantile`, `quantile_test` and `ssu_test`. We renamed them before release because none of them is a recursive-ADF-based test. We kept no deprecated aliases, since the old names never shipped in a CRAN release.
* `radf_monitor()` is renamed to `monitor()`, through a brief intermediate `monitor_radf()` that was never released. It is the flagship of the real-time monitoring family, alongside `monitor_cusum()`, `monitor_lbi()` and `monitor_quantile()`. The name deliberately carries no `radf` or `sadf` token, so that it cannot be mistaken for a `radf_*()` variant despite its ADF-family internals. See `vignette("naming-and-analysis")`.
* `exuber_functions()` is new. It is a queryable registry of the family of every exported function (`adf`, `test`, `dating`, `monitor`, `root`, `regression`), so that finding the monitoring functions is a function call and does not rely on a naming convention.
* `radf_wb_cv2()` and `radf_wb_distr2()` are renamed to `radf_wb_ps_cv()` and `radf_wb_ps_distr()`. The `2` suffix meant only that this was the second wild bootstrap added. The `_ps` suffix identifies the wild bootstrap of Phillips & Shi (2020), which fits a null AR model, resamples the residuals and supports a `tb` training-window boundary. It differs from `radf_wb_cv()`, which uses the non-parametric multiplier bootstrap of Harvey et al. (2016). The new names match the internal naming in this file (`radf_wb_dgp_ps` and `radf_wb_ps`, against `radf_wb_dgp_hlst` and `radf_wb_hlst`) and the package-wide pattern `radf_<method>_<qualifier>_cv`, as in `radf_sign_dm_cv()`. Unlike the renames above, `radf_wb_cv2()` shipped in the 1.0.0 release (the JSS paper). For that reason `radf_wb_cv2()` and `radf_wb_distr2()` remain as deprecated aliases that warn and forward to the new functions (`.Deprecated()`, see `?exuber-deprecated`).
* `radf_tt_cv()`, `radf_sign_cv()` and `radf_sign_dm_cv()` now all compute `badf_cv` and `bsadf_cv`, a time-varying boundary, and not only the three scalar critical values. The results of `radf_tt()`, `radf_sign()` and `radf_sign_dm()` therefore work with the full `summary()`, `datestamp()`, `tidy()` and `autoplot()` pipeline, and no longer only with `summary()` and `tidy()`. We found the problem first in `radf_tt_cv()`, after a user reported a `datestamp()` crash on a `radf_sign()` result. That report led us to check all three functions of the GLS-demeaned family, and the same fix applied to the other two. We validated each function in three ways. The last row of `badf_cv` is bit-identical to `adf_cv`, which is an exact identity. The empirical false-alarm rate is at or below the nominal 5% (`radf_tt` 3.3%, `radf_sign` 5.5%, `radf_sign_dm` 3.5%). The detection power on an identical synthetic bubble is in the same range as the 16% of the established `radf()` and `radf_mc_cv()` baseline: `radf_tt` 18%, `radf_sign` 20% and `radf_sign_dm` 8%. The lower power of `radf_sign_dm` is an expected property of the sign-based tests, which pay for their invariance to heteroskedasticity, and it is not a validation concern. See `vignette("naming-and-analysis")`.

### Performance

* `radf()` is about 25 times faster on typical sample sizes (exubercore v0.3.1). The recursive grid used to re-form the full residual vector for every window, which cost O(n^3) in total. It now keeps running cross-products and uses the closed-form `SSR = y'y - b'X'y`, which costs O(n^2). At n = 400 the time per path drops from about 140 ms to about 6 ms. `radf_mc_cv()`, `radf_wb_cv()`, `radf_sb_cv()`, `monitor()`, `dating_hlw()`, `radf_recovery()` and every other loop over `radf()` speed up by the same factor. The regression is now parameterized as `dy` on `(1, y_{t-1}, dy lags)`, which gives the same t-statistic on `beta - 1`. It is also more accurate than before at large `n` with large levels: the difference from `lm()` is about 3e-11 at n = 2000, where the old recursion gave about 2e-8.
* Parallel runs (`options(exuber.parallel = TRUE)`) now reuse one worker cluster per session. Before, every call started and stopped a fresh `future::multisession`, and the start-up time of a few seconds dominated every small `radf_mc_cv()` or `radf_wb_cv()` job. `exuber.ncores` sets the size of the cluster, and the cluster is stopped when the namespace unloads. `exuber.parallel` now defaults to `interactive()`. Scripts, knitr and `R CMD check` therefore run serially unless they opt in, and a batch job no longer pays for worker start-up or leaves worker connections open for a handful of replications.
* The panel statistic of `radf()` used `apply(bsadf, 1, mean)`. The dispatch overhead of that call grows with the number of rows. It was 65 times slower than the equivalent `rowMeans(bsadf)` at n = 100 and 289 times slower at n = 1000, and it was the largest single part of the runtime of `radf()` at every sample size we tested. We replaced it with `rowMeans(bsadf)`, which gives an identical result. `tests/testthat` is unchanged, with 879 tests passing.

### API consistency

* The package now uses one significance-level convention. Every function that took a `level` argument now takes `sig_lvl` on the 0 to 100 scale that `datestamp()` and `autoplot()` already used, where `sig_lvl = 95` means a 5% test or 95% confidence. The affected functions were all unreleased, so we added no shims. They are `lbi_test()` and `monitor_lbi()` (`0.95` becomes `95`, `0.975` becomes `97.5`, and so on), `ssu_test()`, `monitor()`, `monitor_cusum()`, `rootstamp()` (which took a confidence level such as `0.95`), `quantile_test()` and `monitor_quantile()` (already on the 0 to 100 scale and only renamed), and `cobubble_test()` (which took a size, `level = 0.05`, and now takes `sig_lvl = 95`). A shared `assert_sig_lvl()` makes `sig_lvl = 0.95` an immediate error everywhere, so it can no longer produce a wrong quantile silently.
* `monitor(adflag = )` is now `lag`, matching `radf()`. `monitor_cusum(N = )` is now `h`, matching every other kernel-bandwidth argument. `cobubble_test(lags = )` is now `lag_grid`, so that `lag` and `lags` cannot be confused. This mirrors `tau` and `tau_grid` in `quantile_test()`.
* `cobubble_test()` and `radf_sbz_union()` now return `cobubble_test_obj` and `radf_sbz_union_obj`, which have the `_obj` suffix that every other standalone class already carried.
* `dating_pdc()` and `radf_sbz()` have their own `print()` methods. The former fell through to `print.data.frame`, which hid its attributes. The latter printed as a plain `radf`.
* `scale_exuber_manual(size_values = )` is deprecated in favor of `linewidth_values`, because the `linewidth` aesthetic of ggplot2 (3.4.0 and later) replaces `size` for lines. The deprecation warning that ggplot2 emitted from every `autoplot()` call is gone. `autoplot(include_negative = )`, deprecated since 1.0.0, is now forwarded to `nonrejected` and no longer ignored.
* `radf_tt()`, `radf_tt_cv()` and `monitor_quantile()` carry the same experimental badge as the other new methods. The note on every standalone function that it does not plug into `autoplot` was wrong, because each has had its own `autoplot()` method. The note now says so.

### Other

* `rootstamp()` gives a confidence interval and a doubling time for the explosive root, using S3 dispatch. The default method fits a single sub-sample, and the `radf_obj` method runs every `datestamp()` episode at once. It consolidates three separate functions, `explosive_root()`, `root_ci()` and `root_ci_datestamp()`, before release.

* Every `autoplot()` and `augment()` method now has its own help page and no longer shares one with the function it plots or tidies (`?autoplot.monitor_cusum_obj`, `?augment.radf_obj` and so on). The pkgdown reference index is reorganized into subsections for each function, so that each function is listed next to the methods that consume its output.

* `sim_vol_break()` is new. It generates i.i.d. Gaussian innovations whose standard deviation shifts permanently at a chosen break fraction. This is the non-stationary volatility process of Cavaliere & Taylor (2007), which the volatility-robust tests are built for, and stationary GARCH is not. `sim_ps1()` gained the same `e` argument for injecting innovations that `sim_psy1()` already had. The `c`, `c1` and `c2` arguments of `sim_psy1()` and `sim_ps1()` now accept any positive scalar, as documented, so a fixed explosive root can be set directly (`c = 0.04, alpha = 0`).

* Examples and vignettes now demonstrate each method on the data generating process it targets and no longer use `sim_data` throughout. The volatility-robust tests (`radf_tt()`, `radf_kp()`, `radf_sign()`, `radf_sbz()`, `radf_wb_cv()`, `monitor_cusum(type = "kernel")` and `dating_pdc(type = "wls")`) run on a volatility break. `cobubble_test()` and `contagion_reg()` run on `sim_coexplosive()`, `radf_common()` on `sim_common()`, `ssu_test()` on a stochastic root, and `quantile_test()` and `monitor_quantile()` on heavy-tailed innovations. `dating_*()` and `radf_recovery()` run on `sim_ps1()`, and every monitor runs on a bubble that starts after its training window. The vignettes now use the generators of the package in place of hand-written `cumsum(rnorm())` processes.

### Bug fixes

* `datestamp()`, `autoplot()`, `augment()` and `augment_join()` no longer fail for a sieve-bootstrap `cv` with `lag > 0`. The truncation offset kept a `+ 2` that compensated for the old off-by-one in `radf_sb_cv()` (also fixed in this release), so it over-padded by two rows.

* The panel critical values from `radf_sb_cv()` and `radf_sb_distr()` were wrong in every release since 0.1.0. The bootstrap loop overwrote the BSADF path of each series instead of summing it, so the panel null distribution was the BSADF of the last series divided by the number of series, and not the cross-sectional mean. The error is invisible for panels of about five series, where the two quantities happen to coincide. For narrow panels the test was oversized (8% at a nominal 5% for 2 series), and for wide panels it had no power (0% rejection under H0 and under the alternative for 10 series). This is fixed, and the empirical size is now 6%, 5% and 2.5% for 2, 5 and 10 series. A regression test pins the identity that a panel of identical copies has the same bootstrap distribution as the single series.

* `radf_sb_cv()` and `radf_sb_distr()` silently truncated their output by 2 rows for any `lag >= 1`. This covers a fixed `lag` and also `type = "aic"` or `"bic"` whenever the selected lag was nonzero, which is the usual case. The indexing `initmat[j, lag:1]` in the bootstrap process was one column short of the `lag + 1` values that `dy_boot` needs prepended. The drop rule for index 0 in R made it correct by accident at `lag = 0` only. It is fixed (`initmat[j, (lag + 1):1]`), and `bsadf_panel_cv` and `gsadf_panel_cv` now have the documented `nr - minw - lag` rows for every lag. A regression test pins the row count across `lag = 0:2`.

* The `sig_lvl` argument of `datestamp()`, `autoplot()` and `autoplot2()` now controls whether a series counts as rejecting the null. Before, `diagnostics.radf_obj()`, which decides internally which series get dated or plotted, used the 95% critical value for that decision whatever `sig_lvl` was. A call such as `datestamp(x, cv, sig_lvl = 90)` could therefore stop with `"Cannot reject H0 at the 5% significance level"` for a series that clearly rejects at the 10% level the caller asked for. `sig_lvl` only ever reached the episode threshold curve within a series and never the gate that decides which series are eligible. `diagnostics()` gained a `sig_lvl` argument, with a default of 95 so that default calls behave as before, and `datestamp()`, `autoplot()` and `autoplot2()` now pass their own `sig_lvl` to it.

* `radf()` and everything built on it failed with `subscript out of bounds` on a named numeric vector, for example a `prcomp()` score column, which is what `radf_common()` passes to it. The names leaked into the internal bookkeeping of NA edges.

* The range check for the default `cv` said that the store covers `n <= 5000`. It covers `n <= 4000` and `lag <= 4`, and the message now says so before the function tries the network.

* The `seed` argument of `sim_psy1()` now also covers a generator passed lazily to `e` or `coef_noise`, for example `sim_psy1(n, seed = 1, e = sim_vol_break(n - 1))`. Before, those arguments were evaluated before the seed was set.

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
