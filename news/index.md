# Changelog

## exuber 2.0.0

New methodologies from the `docs/enhancements/` research programme, each
independently validated against a published number (formula-exact check,
table lookup, or a direct Monte Carlo reproduction of the source paper’s
own theorem) — see `docs/enhancements/README.md` for the full record of
what was checked and how.

#### Critical values

- Default critical values now come from a shared precomputed store
  instead of the bundled `radf_crit` dataset: lag 0-4, every `n` from
  the smallest the PSY window allows up to 4000, 2000 replications each.
  [`radf()`](https://kvasilopoulos.github.io/exuber/reference/radf.md)
  followed by
  [`summary()`](https://rdrr.io/r/base/summary.html)/[`datestamp()`](https://kvasilopoulos.github.io/exuber/reference/datestamp.md)/[`autoplot()`](https://ggplot2.tidyverse.org/reference/autoplot.html)
  fetches the one `(n, lag)` table it needs on first use and caches it
  on disk (`tools::R_user_dir("exuber", "cache")`), so a lagged
  specification no longer requires simulating your own `cv`. The tables
  are *nested* (every `n` of a lag from the same seeded paths); values
  differ from the old bundled ones by Monte Carlo noise. Needs network
  access the first time a given `(n, lag)` is used;
  [`radf_mc_cv()`](https://kvasilopoulos.github.io/exuber/reference/radf_mc_cv.md)/[`radf_wb_cv()`](https://kvasilopoulos.github.io/exuber/reference/radf_wb_cv.md)
  remain the offline route.
- `radf_crit` (the bundled lag-0, n \<= 600 table) is removed, along
  with its `print.crit` method and `data-raw/sim-crit.R`; the simulation
  now lives in the sibling `exubercrit` repository.

#### Volatility-robust tests

- [`radf_sbz()`](https://kvasilopoulos.github.io/exuber/reference/radf_sbz.md)/[`radf_sbz_cv()`](https://kvasilopoulos.github.io/exuber/reference/radf_sbz_cv.md)/[`radf_sbz_union()`](https://kvasilopoulos.github.io/exuber/reference/radf_sbz_union.md)
  — Herwartz & Siedenburg’s WLS/kernel-volatility SBZ test, split into a
  statistic
  ([`radf_sbz()`](https://kvasilopoulos.github.io/exuber/reference/radf_sbz.md)),
  its bootstrap critical values
  ([`radf_sbz_cv()`](https://kvasilopoulos.github.io/exuber/reference/radf_sbz_cv.md),
  full
  [`datestamp()`](https://kvasilopoulos.github.io/exuber/reference/datestamp.md)/[`autoplot()`](https://ggplot2.tidyverse.org/reference/autoplot.html)
  support), and the union-of-rejections test against classic `supDF`
  ([`radf_sbz_union()`](https://kvasilopoulos.github.io/exuber/reference/radf_sbz_union.md),
  renamed from the original bundled
  [`radf_sbz_cv()`](https://kvasilopoulos.github.io/exuber/reference/radf_sbz_cv.md)
  — see
  [`vignette("volatility-robust-radf")`](https://kvasilopoulos.github.io/exuber/articles/volatility-robust-radf.md).
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
- `datestamp(..., option = "svadf")` — Sarkar & Wells (2026) SV-ADF
  asymmetric-threshold dating, folded into
  [`datestamp()`](https://kvasilopoulos.github.io/exuber/reference/datestamp.md)
  rather than shipped as a separate `radf_svadf()` entry point.
  **Caveat:** the source is a non-peer-reviewed preprint, flagged at
  call time and in
  [`?datestamp`](https://kvasilopoulos.github.io/exuber/reference/datestamp.md),
  Caveats section.

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

- [`monitor()`](https://kvasilopoulos.github.io/exuber/reference/monitor.md)
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
  [`monitor()`](https://kvasilopoulos.github.io/exuber/reference/monitor.md)
  (via a brief intermediate `monitor_radf()`, never released), the
  flagship of the real-time monitoring family alongside
  [`monitor_cusum()`](https://kvasilopoulos.github.io/exuber/reference/monitor_cusum.md)/[`monitor_lbi()`](https://kvasilopoulos.github.io/exuber/reference/monitor_lbi.md)/
  [`monitor_quantile()`](https://kvasilopoulos.github.io/exuber/reference/monitor_quantile.md)
  — deliberately carrying no `radf`/`sadf` token so it can’t be mistaken
  for a `radf_*()` variant, ADF-family internals notwithstanding — see
  [`vignette("naming-and-analysis")`](https://kvasilopoulos.github.io/exuber/articles/naming-and-analysis.md).
- [`exuber_functions()`](https://kvasilopoulos.github.io/exuber/reference/exuber_functions.md)
  added: a queryable registry of every exported function’s family
  (`adf`, `test`, `dating`, `monitor`, `root`, `regression`), so “what
  monitoring functions exist” is an actual function call, not a naming
  convention to memorize.
- [`radf_wb_cv2()`](https://kvasilopoulos.github.io/exuber/reference/exuber-deprecated.md)/[`radf_wb_distr2()`](https://kvasilopoulos.github.io/exuber/reference/exuber-deprecated.md)
  renamed to
  [`radf_wb_ps_cv()`](https://kvasilopoulos.github.io/exuber/reference/radf_wb_ps_cv.md)/
  [`radf_wb_ps_distr()`](https://kvasilopoulos.github.io/exuber/reference/radf_wb_ps_cv.md)
  – the `2` suffix named nothing (just “the second wild bootstrap
  added”); `_ps` identifies it as Phillips & Shi (2020)’s wild bootstrap
  (fits a null AR model, resamples residuals, supports a `tb`
  training-window boundary), as opposed to
  [`radf_wb_cv()`](https://kvasilopoulos.github.io/exuber/reference/radf_wb_cv.md)’s
  Harvey et al. (2016) non-parametric multiplier bootstrap, matching
  this file’s own internal naming (`radf_wb_dgp_ps`/`radf_wb_ps` vs.
  `radf_wb_dgp_hlst`/`radf_wb_hlst`) and the package-wide
  `radf_<method>_<qualifier>_cv` pattern
  ([`radf_sign_dm_cv()`](https://kvasilopoulos.github.io/exuber/reference/radf_sign_dm_cv.md)).
  Unlike the renames above,
  [`radf_wb_cv2()`](https://kvasilopoulos.github.io/exuber/reference/exuber-deprecated.md)
  shipped in the 1.0.0 release (the JSS paper), so this one keeps
  [`radf_wb_cv2()`](https://kvasilopoulos.github.io/exuber/reference/exuber-deprecated.md)/[`radf_wb_distr2()`](https://kvasilopoulos.github.io/exuber/reference/exuber-deprecated.md)
  as deprecated aliases
  ([`.Deprecated()`](https://rdrr.io/r/base/Deprecated.html),
  warn-and-forward, see `?exuber-deprecated`) rather than a clean break.
- [`radf_tt_cv()`](https://kvasilopoulos.github.io/exuber/reference/radf_tt_cv.md),
  [`radf_sign_cv()`](https://kvasilopoulos.github.io/exuber/reference/radf_sign_cv.md),
  and
  [`radf_sign_dm_cv()`](https://kvasilopoulos.github.io/exuber/reference/radf_sign_dm_cv.md)
  now all compute `badf_cv`/`bsadf_cv` (a time-varying boundary), not
  just the three scalar critical values —
  [`radf_tt()`](https://kvasilopoulos.github.io/exuber/reference/radf_tt.md)/[`radf_sign()`](https://kvasilopoulos.github.io/exuber/reference/radf_sign.md)/
  [`radf_sign_dm()`](https://kvasilopoulos.github.io/exuber/reference/radf_sign_dm.md)
  results now work with the full
  [`summary()`](https://rdrr.io/r/base/summary.html)/
  [`datestamp()`](https://kvasilopoulos.github.io/exuber/reference/datestamp.md)/[`tidy()`](https://generics.r-lib.org/reference/tidy.html)/[`autoplot()`](https://ggplot2.tidyverse.org/reference/autoplot.html)
  pipeline, not just [`summary()`](https://rdrr.io/r/base/summary.html)/
  [`tidy()`](https://generics.r-lib.org/reference/tidy.html). Found
  first in
  [`radf_tt_cv()`](https://kvasilopoulos.github.io/exuber/reference/radf_tt_cv.md)
  (a user-reported
  [`datestamp()`](https://kvasilopoulos.github.io/exuber/reference/datestamp.md)
  crash on
  [`radf_sign()`](https://kvasilopoulos.github.io/exuber/reference/radf_sign.md)
  prompted checking all three GLS-demeaned-family functions), then
  confirmed the identical fix applies to the other two. Validated per
  function: `badf_cv`‘s last row is bit-identical to `adf_cv` (a hard
  identity), empirical false-alarm rate at or below nominal (`radf_tt`
  3.3%, `radf_sign` 5.5%, `radf_sign_dm` 3.5%, vs. 5% nominal), and
  detection power on an identical synthetic bubble in the same range as
  the established
  [`radf()`](https://kvasilopoulos.github.io/exuber/reference/radf.md)/[`radf_mc_cv()`](https://kvasilopoulos.github.io/exuber/reference/radf_mc_cv.md)
  baseline (16%): `radf_tt` 18%, `radf_sign` 20%, `radf_sign_dm` 8%
  (lower power a known, expected property of the sign-based tests’
  heteroskedasticity invariance, not a validation concern) — see
  [`vignette("naming-and-analysis")`](https://kvasilopoulos.github.io/exuber/articles/naming-and-analysis.md).

#### Performance

- [`radf()`](https://kvasilopoulos.github.io/exuber/reference/radf.md)
  is ~25x faster on typical sample sizes (exubercore v0.3.0): the
  recursive grid re-formed the full residual vector for every window, an
  O(n^3) total; it now keeps running cross-products and uses the
  closed-form `SSR = y'y - b'X'y`, making it O(n^2). n = 400 drops from
  ~140 ms to ~6 ms per path, so
  [`radf_mc_cv()`](https://kvasilopoulos.github.io/exuber/reference/radf_mc_cv.md),
  [`radf_wb_cv()`](https://kvasilopoulos.github.io/exuber/reference/radf_wb_cv.md),
  [`radf_sb_cv()`](https://kvasilopoulos.github.io/exuber/reference/radf_sb_cv.md),
  [`monitor()`](https://kvasilopoulos.github.io/exuber/reference/monitor.md),
  [`dating_hlw()`](https://kvasilopoulos.github.io/exuber/reference/dating_hlw.md),
  [`radf_recovery()`](https://kvasilopoulos.github.io/exuber/reference/radf_recovery.md)
  and every other loop over
  [`radf()`](https://kvasilopoulos.github.io/exuber/reference/radf.md)
  speed up by the same factor. Results are numerically identical to
  ~1e-12.
- Parallel runs (`options(exuber.parallel = TRUE)`, the default) reuse
  one worker cluster per session instead of starting and stopping a
  fresh
  [`future::multisession`](https://future.futureverse.org/reference/multisession.html)
  on every call – a few seconds of start-up overhead that used to
  dominate every small
  [`radf_mc_cv()`](https://kvasilopoulos.github.io/exuber/reference/radf_mc_cv.md)/[`radf_wb_cv()`](https://kvasilopoulos.github.io/exuber/reference/radf_wb_cv.md)
  job. The cluster is sized by `exuber.ncores` and stopped when the
  namespace unloads.

#### API consistency

- One significance-level convention across the whole package: every
  function that took a `level` argument now takes `sig_lvl` on the 0-100
  scale already used by
  [`datestamp()`](https://kvasilopoulos.github.io/exuber/reference/datestamp.md)/[`autoplot()`](https://ggplot2.tidyverse.org/reference/autoplot.html)
  (`sig_lvl = 95` = a 5% test / 95% confidence). Affected (all
  unreleased, so no shims):
  [`lbi_test()`](https://kvasilopoulos.github.io/exuber/reference/lbi_test.md),
  [`monitor_lbi()`](https://kvasilopoulos.github.io/exuber/reference/monitor_lbi.md)
  (`0.95` -\> `95`, `0.975` -\> `97.5`, …),
  [`ssu_test()`](https://kvasilopoulos.github.io/exuber/reference/ssu_test.md),
  [`monitor()`](https://kvasilopoulos.github.io/exuber/reference/monitor.md),
  [`monitor_cusum()`](https://kvasilopoulos.github.io/exuber/reference/monitor_cusum.md),
  [`rootstamp()`](https://kvasilopoulos.github.io/exuber/reference/rootstamp.md)
  (was a `0.95`-style confidence level),
  [`quantile_test()`](https://kvasilopoulos.github.io/exuber/reference/quantile_test.md)/[`monitor_quantile()`](https://kvasilopoulos.github.io/exuber/reference/monitor_quantile.md)
  (already 0-100, renamed only), and
  [`cobubble_test()`](https://kvasilopoulos.github.io/exuber/reference/cobubble_test.md)
  (was a *size*, `level = 0.05`; now `sig_lvl = 95`). A shared
  `assert_sig_lvl()` makes `sig_lvl = 0.95` an immediate error
  everywhere rather than a silently wrong quantile.
- `monitor(adflag = )` -\> `lag`, matching
  [`radf()`](https://kvasilopoulos.github.io/exuber/reference/radf.md);
  `monitor_cusum(N = )` -\> `h`, matching every other kernel-bandwidth
  argument; `cobubble_test(lags = )` -\> `lag_grid`, so `lag`/`lags` can
  no longer be confused (mirrors
  [`quantile_test()`](https://kvasilopoulos.github.io/exuber/reference/quantile_test.md)’s
  `tau`/`tau_grid`).
- [`cobubble_test()`](https://kvasilopoulos.github.io/exuber/reference/cobubble_test.md)
  and
  [`radf_sbz_union()`](https://kvasilopoulos.github.io/exuber/reference/radf_sbz_union.md)
  now return `cobubble_test_obj`/ `radf_sbz_union_obj`, the `_obj`
  suffix every other standalone class already carried.
- [`dating_pdc()`](https://kvasilopoulos.github.io/exuber/reference/dating_pdc.md)
  and
  [`radf_sbz()`](https://kvasilopoulos.github.io/exuber/reference/radf_sbz.md)
  gained their own [`print()`](https://rdrr.io/r/base/print.html)
  methods (the former fell through to `print.data.frame`, hiding its
  attributes; the latter printed as a plain `radf`).
- `scale_exuber_manual(size_values = )` is deprecated in favor of
  `linewidth_values` (ggplot2 \>= 3.4.0’s `linewidth` aesthetic replaces
  `size` for lines; the deprecation warning ggplot2 emitted from every
  [`autoplot()`](https://ggplot2.tidyverse.org/reference/autoplot.html)
  is gone). `autoplot(include_negative = )`, deprecated since 1.0.0, is
  now actually forwarded to `nonrejected` instead of ignored.
- [`radf_tt()`](https://kvasilopoulos.github.io/exuber/reference/radf_tt.md)/[`radf_tt_cv()`](https://kvasilopoulos.github.io/exuber/reference/radf_tt_cv.md)/[`monitor_quantile()`](https://kvasilopoulos.github.io/exuber/reference/monitor_quantile.md)
  carry the same experimental badge as the other new methods; the “does
  not plug into `autoplot`” note on every standalone function was wrong
  (each has had its own
  [`autoplot()`](https://ggplot2.tidyverse.org/reference/autoplot.html)
  method) and now says so.

#### Other

- [`rootstamp()`](https://kvasilopoulos.github.io/exuber/reference/rootstamp.md)
  — confidence interval and doubling time on the explosive root, via S3
  dispatch: the default method fits a single sub-sample, the `radf_obj`
  method runs every
  [`datestamp()`](https://kvasilopoulos.github.io/exuber/reference/datestamp.md)
  episode at once (previously three separate functions –
  `explosive_root()`, `root_ci()`, `root_ci_datestamp()` – consolidated
  before release).

- Documentation: every
  [`autoplot()`](https://ggplot2.tidyverse.org/reference/autoplot.html)
  and [`augment()`](https://generics.r-lib.org/reference/augment.html)
  method now has its own help page instead of sharing one with the
  function it plots/tidies
  ([`?autoplot.monitor_cusum_obj`](https://kvasilopoulos.github.io/exuber/reference/autoplot.monitor_cusum_obj.md),
  [`?augment.radf_obj`](https://kvasilopoulos.github.io/exuber/reference/augment.radf_obj.md),
  …). The pkgdown reference index is reorganized into per-function
  subsections so each function is listed next to the methods that
  consume its output.

- [`sim_vol_break()`](https://kvasilopoulos.github.io/exuber/reference/sim_vol_break.md)
  – i.i.d. Gaussian innovations whose standard deviation shifts
  permanently at a chosen break fraction, the non-stationary volatility
  DGP (Cavaliere & Taylor 2007) the volatility-robust tests are actually
  built for, unlike stationary GARCH.
  [`sim_ps1()`](https://kvasilopoulos.github.io/exuber/reference/sim_ps1.md)
  gained the same `e` innovation-injection argument
  [`sim_psy1()`](https://kvasilopoulos.github.io/exuber/reference/sim_psy1.md)
  already had, and the `c`/`c1`/`c2` arguments of
  [`sim_psy1()`](https://kvasilopoulos.github.io/exuber/reference/sim_psy1.md)/[`sim_ps1()`](https://kvasilopoulos.github.io/exuber/reference/sim_ps1.md)
  now accept any positive scalar (as documented), so a fixed explosive
  root is expressible directly (`c = 0.04, alpha = 0`).

- Examples and vignettes now demonstrate each method on the DGP it
  targets rather than on `sim_data` throughout: volatility-robust tests
  ([`radf_tt()`](https://kvasilopoulos.github.io/exuber/reference/radf_tt.md),
  [`radf_kp()`](https://kvasilopoulos.github.io/exuber/reference/radf_kp.md),
  [`radf_sign()`](https://kvasilopoulos.github.io/exuber/reference/radf_sign.md),
  [`radf_sbz()`](https://kvasilopoulos.github.io/exuber/reference/radf_sbz.md),
  [`radf_wb_cv()`](https://kvasilopoulos.github.io/exuber/reference/radf_wb_cv.md),
  `monitor_cusum(type = "kernel")`, `dating_pdc(type = "wls")`) on a
  volatility break,
  [`cobubble_test()`](https://kvasilopoulos.github.io/exuber/reference/cobubble_test.md)/[`contagion_reg()`](https://kvasilopoulos.github.io/exuber/reference/contagion_reg.md)
  on
  [`sim_coexplosive()`](https://kvasilopoulos.github.io/exuber/reference/sim_coexplosive.md),
  [`radf_common()`](https://kvasilopoulos.github.io/exuber/reference/radf_common.md)
  on
  [`sim_common()`](https://kvasilopoulos.github.io/exuber/reference/sim_common.md),
  [`ssu_test()`](https://kvasilopoulos.github.io/exuber/reference/ssu_test.md)
  on a stochastic root,
  [`quantile_test()`](https://kvasilopoulos.github.io/exuber/reference/quantile_test.md)/[`monitor_quantile()`](https://kvasilopoulos.github.io/exuber/reference/monitor_quantile.md)
  on heavy-tailed innovations,
  `dating_*()`/[`radf_recovery()`](https://kvasilopoulos.github.io/exuber/reference/radf_recovery.md)
  on
  [`sim_ps1()`](https://kvasilopoulos.github.io/exuber/reference/sim_ps1.md),
  and every monitor on a bubble that starts after its training window.
  Hand-rolled `cumsum(rnorm())` DGPs in the vignettes are replaced by
  the package’s own generators.

#### Bug fixes

- [`datestamp()`](https://kvasilopoulos.github.io/exuber/reference/datestamp.md)/[`autoplot()`](https://ggplot2.tidyverse.org/reference/autoplot.html)/[`autoplot2()`](https://kvasilopoulos.github.io/exuber/reference/autoplot2.md)’s
  `sig_lvl` argument now actually controls whether a series counts as
  rejecting the null. Previously,
  [`diagnostics.radf_obj()`](https://kvasilopoulos.github.io/exuber/reference/diagnostics.md)
  (used internally to decide which series get dated/plotted at all)
  hard-coded the 95% critical value for that decision regardless of
  `sig_lvl`, so e.g. `datestamp(x, cv, sig_lvl = 90)` could throw
  `"Cannot reject H0 at the 5% significance level"` for a series that
  clearly rejects at the 10% level the caller asked for – `sig_lvl` only
  ever reached the within-series episode threshold curve, never the
  series-eligibility gate.
  [`diagnostics()`](https://kvasilopoulos.github.io/exuber/reference/diagnostics.md)
  gained a `sig_lvl` argument (default 95, so default-call behavior is
  unchanged) and
  [`datestamp()`](https://kvasilopoulos.github.io/exuber/reference/datestamp.md)/[`autoplot()`](https://ggplot2.tidyverse.org/reference/autoplot.html)/[`autoplot2()`](https://kvasilopoulos.github.io/exuber/reference/autoplot2.md)
  now thread their own `sig_lvl` through to it.

- [`radf()`](https://kvasilopoulos.github.io/exuber/reference/radf.md)
  (and everything built on it) errored with `subscript out of bounds` on
  a *named* numeric vector, e.g. a
  [`prcomp()`](https://rdrr.io/r/stats/prcomp.html) score column – which
  is exactly what
  [`radf_common()`](https://kvasilopoulos.github.io/exuber/reference/radf_common.md)
  feeds it – because the names leaked into the internal NA-edge
  bookkeeping.

- The default-`cv` range check said the store covers `n <= 5000`; it
  covers `n <= 4000` and `lag <= 4`, and now says so before trying the
  network.

- [`sim_psy1()`](https://kvasilopoulos.github.io/exuber/reference/sim_psy1.md)’s
  `seed` argument now also covers a generator passed lazily to
  `e`/`coef_noise`
  (e.g. `sim_psy1(n, seed = 1, e = sim_vol_break(n - 1))`); previously
  those were forced before the seed was set.

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
- [`augment_join()`](https://kvasilopoulos.github.io/exuber/reference/augment_join.md)
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
