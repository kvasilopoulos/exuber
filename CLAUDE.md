# exuber

R package (Rcpp/RcppArmadillo) for recursive unit root and explosive
time series testing. It has the standard `devtools`-based layout: `R/`,
`src/`, `tests/testthat/`, `man/` (generated, so do not edit it by hand)
and `vignettes/`.

## R on this machine

R is managed by `rig`, which keeps several versions installed.
`rig list` shows the current default. The directory
`/c/Program Files/R/bin` contains only `.bat` shims and no `R.exe` or
`Rscript.exe`.

- In PowerShell, `Rscript -e "..."` and `R` work as they are, because
  PATHEXT resolves the `.bat` shims.
- In the Bash tool, plain `R` and `Rscript` also work, through wrapper
  scripts at `~/.local/bin/R` and `~/.local/bin/Rscript`, which come
  first on PATH. Calling the `.bat` shims directly from Bash mangles
  complex quoted arguments. Parentheses in an `-e` script make cmd.exe
  reparse the batch arguments, and the call then fails with “system
  cannot find the file specified”. The wrappers avoid this. They read
  the current version target from `bin/R.bat` and `bin/Rscript.bat` and
  `exec` the real `.exe` directly, so they follow whatever `rig default`
  is set to.

The compiler toolchain is Rtools45 (`C:\rtools45`), which is already on
PATH. It is needed to build the Rcpp code in `src/`.

## Common tasks

Run these from the package root in PowerShell:

``` powershell
Rscript -e "devtools::load_all()"          # iterate without installing
Rscript -e "devtools::document()"          # regenerate NAMESPACE/man from roxygen comments
Rscript -e "devtools::test()"              # testthat suite
Rscript -e "devtools::check()"             # full R CMD check (quality gate)
Rscript -e "styler::style_pkg()"           # reformat
Rscript -e "lintr::lint_package()"         # static checks (no .lintr config yet, so lintr defaults apply)
Rscript -e "covr::package_coverage()"      # coverage, mirrors test-coverage.yaml
```

The `Makefile` wraps some of these (`make check`, `make build_site` and
so on), but it still calls `Rscript`, so run it from PowerShell as well.
From a plain cmd or bash shell, `make` cannot resolve R correctly
without the shim rule above.

After you change a roxygen `#'` comment or an `@export`, run
`devtools::document()` before `check()`. NAMESPACE and `man/*.Rd` are
generated and are not maintained by hand.

`DESCRIPTION` carries `Config/roxygen2/version: 8.1.0`, committed in
2026-09, which matches the roxygen2 installed here. `document()`
therefore no longer rewrites `DESCRIPTION`. Still diff `NAMESPACE` and
`man/*.Rd` after you run it, and revert any change that does not come
from a roxygen-comment edit you made. Never run
`git checkout -- DESCRIPTION` blindly, because it also reverts
intentional edits made in the same pass, such as a version bump or a
dependency floor.

**Concurrency.** `devtools::load_all()`, `run_examples()`, `covr` and
`pkgdown` all compile in place in `src/`. If two of them run at the same
time in the same tree, they corrupt `src/*.o` and `exuber.dll`. The
symptoms are a linker error “symbol not defined” or a segfault on load.
Run one R process against the tree at a time. If it does happen, run
`rm src/*.o src/*.dll` and call `load_all()` again.
[`pkgdown::check_pkgdown()`](https://pkgdown.r-lib.org/reference/check_pkgdown.html)
from `Rscript` needs
`Sys.setenv(RSTUDIO_PANDOC = "C:/Program Files/RStudio/resources/app/bin/quarto/bin/tools")`.
`build_reference_index()` writes an untracked `pkgdown/favicon/` into
the repository, and you should delete it.

`exuberdata` is a separate drat-hosted data package. No vignette,
example or test uses it any longer, and nothing here depends on it.

## CI and quality gates (already wired, do not duplicate)

- `.github/workflows/R-CMD-check.yaml` runs R CMD check on macOS,
  Windows and Ubuntu (release, devel and oldrel).
- `.github/workflows/test-coverage.yaml` runs covr and uploads to
  Codecov.
- `.github/workflows/pkgdown.yaml` builds and deploys the documentation
  site.
- `.github/workflows/rhub.yaml` runs the R-hub CRAN-platform checks
  manually.
- `.github/workflows/html-5-check.yaml` validates the Rd and HTML5
  output.

These are the gates that CRAN cares about. Match them locally with
`devtools::check()` before you push, and do not invent new lint or CI
configuration.

## Release process (CRAN)

`.claude/skills/cran-release/checklist.md` is the consolidated usethis,
r-pkgs.org and CRAN-policy checklist, with the status of each item from
the last release pass. The `cran-release` skill runs it. Update the
checklist in place and do not derive it again. It records two local
quirks. First, `devtools::check()` on this Windows machine always leaves
an empty `'NULL'` directory behind, which gives a local-only NOTE about
non-standard things in the check directory. Second, a worker cluster
that lives for the whole session in examples triggers the “connections
left open” note of `R CMD check`. This is why `exuber.parallel` defaults
to [`interactive()`](https://rdrr.io/r/base/interactive.html).

1.  Add NEWS.md entries under the `# exuber (development version)`
    heading as features and fixes ship. This is already the convention,
    as the file shows.
2.  Bump the version with `usethis::use_version()` or by editing the
    `Version:` field of `DESCRIPTION` by hand, and retitle the top
    heading of NEWS.md to match, for example `# exuber 1.2.0`.
3.  Refresh `cran-comments.md`, in particular the test environments and
    the R CMD check NOTEs, with the output of the current run and not
    that of the last release.
4.  Run the CRAN-facing gates before you submit. They are the same ones
    listed above and not new checks for release day:
    `devtools::check(cran = TRUE)` locally,
    `devtools::check_win_devel()` (win-builder), and a manual trigger of
    `.github/workflows/rhub.yaml` for the R-hub CRAN-platform checks.
5.  Submit with `devtools::release()`, which walks through the standard
    checklist, or upload directly at
    <https://cran.r-project.org/submit.html> with `cran-comments.md` as
    the covering note.
6.  After acceptance, tag the release commit, cut a GitHub release, and
    retitle the top heading of NEWS.md back to
    `# exuber (development version)` for the next cycle.

## Deprecation policy

The policy depends on whether the old name ever shipped in a CRAN
release. Check that first and do not rely on habit.

- If the name never shipped on CRAN (an unreleased development version,
  or a rename in the same PR before the merge), make a clean break with
  no shim. Most renames fall in this group (see “Naming” below, entries
  of 2026-08-13 and 2026-08-18).
- If the name already shipped on CRAN, keep a thin
  `.Deprecated(new = "...")` wrapper in `R/deprecate.R`, documented
  under `?exuber-deprecated`, that calls through to the new name. The
  precedents are
  [`col_names()`](https://kvasilopoulos.github.io/exuber/reference/exuber-deprecated.md),
  [`mc_cv()`](https://kvasilopoulos.github.io/exuber/reference/exuber-deprecated.md),
  [`wb_cv()`](https://kvasilopoulos.github.io/exuber/reference/exuber-deprecated.md)
  and
  [`sb_cv()`](https://kvasilopoulos.github.io/exuber/reference/exuber-deprecated.md),
  and
  [`radf_wb_cv2()`](https://kvasilopoulos.github.io/exuber/reference/exuber-deprecated.md)
  and
  [`radf_wb_distr2()`](https://kvasilopoulos.github.io/exuber/reference/exuber-deprecated.md)
  (2026-08-22, see “Naming” below, the first rename of a name released
  on CRAN in this project).

In both cases a rename or a removal updates the following in one commit:
the function, the registry in
[`exuber_functions()`](https://kvasilopoulos.github.io/exuber/reference/exuber_functions.md),
`_pkgdown.yml`, `NEWS.md`, this file and the naming-and-analysis
vignette.

## Naming: not everything is `radf_*` any more

**2026-08-13.** We renamed 12 exported functions that were never based
on the recursive ADF. They are dating and model-selection procedures,
monitoring boundaries, quantile-regression tests, a KPSS-type
co-explosivity test and a point-estimation regression. They lost the
`radf_` prefix in a clean break, with no
[`.Deprecated()`](https://rdrr.io/r/base/Deprecated.html) shims:
`radf_cobubble` became `cobubble_test`, `radf_contagion` became
`contagion_reg`, `radf_cusum` became `monitor_cusum`, `radf_hls` became
`dating_hls`, `radf_hlw` became `dating_hlw`, `radf_knp` became
`dating_knp`, `radf_lbi` became `lbi_test`, `radf_lbi_monitor` became
`monitor_lbi`, `radf_pdc` became `dating_pdc`, `radf_qpwy` became
`monitor_quantile`, `radf_quantile` became `quantile_test` and
`radf_ssu` became `ssu_test`. The convention for the new names is as
follows. A `_test` suffix marks a hypothesis test with a null
distribution or critical value. A `dating_` prefix marks dating by point
estimation or model selection, with no formal test. A `monitor_` prefix
marks real-time or sequential monitoring. Everything that reuses the
recursive-DF core of
[`radf()`](https://kvasilopoulos.github.io/exuber/reference/radf.md), or
its `badf` and `bsadf` output, correctly kept the `radf_` prefix and was
left alone. We renamed the source files to match with `git mv`, so
[`dating_hls()`](https://kvasilopoulos.github.io/exuber/reference/dating_hls.md)
now lives in `R/dating_hls.R` and no longer in `R/radf_hls.R`.

**2026-08-18.** `radf_monitor()` became `monitor_radf()` and then
[`monitor()`](https://kvasilopoulos.github.io/exuber/reference/monitor.md).
This is the one deliberate exception to the rule that ADF-family
functions keep `radf_`. The function is grouped with its fellow monitors
([`monitor_cusum()`](https://kvasilopoulos.github.io/exuber/reference/monitor_cusum.md),
[`monitor_lbi()`](https://kvasilopoulos.github.io/exuber/reference/monitor_lbi.md)
and
[`monitor_quantile()`](https://kvasilopoulos.github.io/exuber/reference/monitor_quantile.md))
as their flagship, the role that
[`radf()`](https://kvasilopoulos.github.io/exuber/reference/radf.md)
plays for the `radf_` family. The price is that its name no longer shows
its ADF-family internals. We went one step beyond the first landing name
`monitor_radf()`, because that name still read like a `radf_*()` variant
despite the reordered prefix.
[`monitor()`](https://kvasilopoulos.github.io/exuber/reference/monitor.md)
carries no `radf` or `sadf` token at all, so nobody can mistake it for a
member of that family. Naming conventions are fuzzy and easy to get
wrong from either direction: purity of the internal mechanism competes
with the discoverability of the behavior, and in this case even
discoverability needed a second pass. Do not rely on them for anything
programmatic. `exuber_functions(family = ...)` in `R/exuber_functions.R`
is the queryable registry, with the families `adf`, `test`, `dating`,
`monitor`, `root` and `regression`. Update it whenever you add or rename
an exported function, as you must also update `_pkgdown.yml`, `NEWS.md`,
this file and the `naming-and-analysis` vignette.

**2026-08-18.** We removed `radf_svadf()` and did not rename it. It is
now the option `option = "svadf"` of
[`datestamp()`](https://kvasilopoulos.github.io/exuber/reference/datestamp.md).
It was already a dating procedure and not a test, because it compares
against the thresholds `log(t)/10` and `log(t)/2` and needs no critical
value, so a `dating_` name would have fit the convention above. It had
been a separate function only because we had rejected an extension of
[`datestamp()`](https://kvasilopoulos.github.io/exuber/reference/datestamp.md)
earlier. The header comment of `R/svadf.R` gave the reason: the S3
dispatch of
[`datestamp()`](https://kvasilopoulos.github.io/exuber/reference/datestamp.md)
assumes one shared critical value throughout. We revisited that decision
and went ahead. The `option` argument of
[`datestamp.radf_obj()`](https://kvasilopoulos.github.io/exuber/reference/datestamp.md)
now dispatches to a `datestamp_svadf()` helper. The helper bypasses the
`cv` and `sig_lvl` path entirely and reuses the machinery of the
`"gsadf"` and `"sadf"` options (`stamp()`, `add_peak()`,
`stamp_to_index()` and `add_ongoing()`). The return shape is therefore
the same for all three options: a `ds_radf` list with `Start`, `Peak`,
`End`, `Duration`, `Signal` and `Ongoing` for each series. In the end
this was not a question about the `radf_`, `_test` or `dating_` naming
scheme. It is one option value on an existing generic, so there is no
new exported name to place in the table above.

**2026-08-22.**
[`radf_sbz_cv()`](https://kvasilopoulos.github.io/exuber/reference/radf_sbz_cv.md)
was split. The bundled union test of supDF, supBZ and U became
[`radf_sbz_union()`](https://kvasilopoulos.github.io/exuber/reference/radf_sbz_union.md),
and
[`radf_sbz_cv()`](https://kvasilopoulos.github.io/exuber/reference/radf_sbz_cv.md)
now returns plain bootstrap critical values for
[`radf_sbz()`](https://kvasilopoulos.github.io/exuber/reference/radf_sbz.md).
Also, `explosive_root()`, `root_ci()` and `root_ci_datestamp()` were
folded into
[`rootstamp()`](https://kvasilopoulos.github.io/exuber/reference/rootstamp.md)
(the default and `radf_obj` methods). All of these names were
unreleased, so the changes are clean breaks. There is no `root_` prefix
convention, and
[`rootstamp()`](https://kvasilopoulos.github.io/exuber/reference/rootstamp.md)
is the only member of the `root` family in the registry.

**2026-09-14.** We unified argument names in a clean break, since all
the names were unreleased. Every `level` argument became `sig_lvl` on
the 0 to 100 scale (`lbi_test`, `monitor_lbi`, `ssu_test`, `monitor`,
`monitor_cusum`, `rootstamp`, `quantile_test`, `monitor_quantile` and
`cobubble_test`). `assert_sig_lvl()` in `R/utils-defensive.R` validates
it, and any new function that takes a level should use it.
`monitor(adflag=)` became `lag`, `monitor_cusum(N=)` became `h` and
`cobubble_test(lags=)` became `lag_grid`. The classes `cobubble_test`
and `radf_sbz_union` became `cobubble_test_obj` and
`radf_sbz_union_obj`. New standalone classes need the `_obj` suffix, a
[`print()`](https://rdrr.io/r/base/print.html) method and an
[`autoplot()`](https://ggplot2.tidyverse.org/reference/autoplot.html)
method. `tests/testthat/test-methods-smoke.R` enumerates them.

**2026-08-22.**
[`radf_wb_cv2()`](https://kvasilopoulos.github.io/exuber/reference/exuber-deprecated.md)
and
[`radf_wb_distr2()`](https://kvasilopoulos.github.io/exuber/reference/exuber-deprecated.md)
became
[`radf_wb_ps_cv()`](https://kvasilopoulos.github.io/exuber/reference/radf_wb_ps_cv.md)
and
[`radf_wb_ps_distr()`](https://kvasilopoulos.github.io/exuber/reference/radf_wb_ps_cv.md).
The `2` suffix only recorded that this was the second wild bootstrap
added to the file, and it described nothing. The `_ps` suffix (Phillips
& Shi 2020) matches the internal naming of the DGPs in this file
(`radf_wb_dgp_ps` and `radf_wb_ps`, against `radf_wb_dgp_hlst` and
`radf_wb_hlst`, already split into `# DGP_PS` and `# DGP_HLST` sections
in `R/radf_wb.R`) and the package-wide pattern
`radf_<method>_<qualifier>_cv`, as in
[`radf_sign_dm_cv()`](https://kvasilopoulos.github.io/exuber/reference/radf_sign_dm_cv.md).
This is the first rename in the history of the project that keeps a
deprecated alias and does not make a clean break. Every earlier rename
above was justified by the fact that the name had never shipped in a
CRAN release, either because it came from this unreleased development
version or because it was renamed in the same PR before the merge.
[`radf_wb_cv2()`](https://kvasilopoulos.github.io/exuber/reference/exuber-deprecated.md)
shipped in 1.0.0, the release of the JSS paper, so user code may already
call it. We kept it as a thin wrapper that warns through
[`.Deprecated()`](https://rdrr.io/r/base/Deprecated.html) in
`R/deprecate.R` (`?exuber-deprecated`), which is the mechanism already
used for
[`col_names()`](https://kvasilopoulos.github.io/exuber/reference/exuber-deprecated.md),
[`mc_cv()`](https://kvasilopoulos.github.io/exuber/reference/exuber-deprecated.md),
[`wb_cv()`](https://kvasilopoulos.github.io/exuber/reference/exuber-deprecated.md)
and
[`sb_cv()`](https://kvasilopoulos.github.io/exuber/reference/exuber-deprecated.md).
Check whether a rename target already shipped on CRAN before you reach
for the clean-break precedent above, because only unreleased names get
that treatment.

## Implementing items from docs/

`../docs/` is the methodology and replication record for the whole
workspace, shared with exubercore and pyexuber. `docs/README.md` is the
map. The record evaluates papers for whether and how to add their
method. It is organized by methodological family
(`volatility-robustness.md`, `dating-and-root-inference.md`,
`monitoring.md`, `multivariate.md`, `alternative-paradigms.md`,
`open-research-directions.md` and `practitioner-guidance.md`). It also
has a narrative summary in `SUMMARY.md`, a taxonomy and status table in
`README.md`, and `parity.md`, which records which implementation ships
each method. Working through this backlog established the workflow
below. Follow it for every new item. Apply it again to items that are
marked “evaluated, not implemented” or “more expensive”, and do not
trust those verdicts until you have, because the verdicts have
repeatedly turned out to be wrong.

### Before implementing: triage again and do not trust the existing cost note

A cost note written after reading only an abstract is often too
pessimistic. Each time we checked an existing verdict of “needs new
simulation” or “needs new machinery” by rendering the pages of the
primary source and reading the exact equations, one of the following
turned out to be true.

- The critical value is a published table or a closed-form formula that
  the paper already computed. Examples are the `SADF` and `GSADF_{s0}`
  boundaries of Kurozumi (2020), the FLUC and CUSUM tables of Homm &
  Breitung, Table 1 of Breitung & Diegel, Table I of Kurozumi & Nishi
  and the `log(n)`-based thresholds of Sarkar & Wells. No new Monte
  Carlo simulation is needed on our side.
- The new regression or statistic reduces to the same closed-form window
  pattern that the package already uses elsewhere. The functions
  `hls_prefix_sums()`, `hls_segment_ssr()` and `hls_segment_coef()` in
  `R/dating_hls.R` implement a generic closed-form OLS for an `(x, z)`
  pair over a segment, using differences of
  [`cumsum()`](https://rdrr.io/r/base/cumsum.html). The new method then
  needs only a different `(x, z)` choice
  ([`ssu_test()`](https://kvasilopoulos.github.io/exuber/reference/ssu_test.md)
  and
  [`contagion_reg()`](https://kvasilopoulos.github.io/exuber/reference/contagion_reg.md))
  or a different input transform fed into the same machinery
  ([`radf_sign()`](https://kvasilopoulos.github.io/exuber/reference/radf_sign.md)
  feeding `gls_dfstat_grid()`).
- An item described as bigger bundled several sub-cases under one
  verdict without separating them. The cheap sub-case is worth shipping
  even if the expensive one stays out of scope. Examples are the `SADF`
  and `GSADF_{s0}` cases of `monitor(boundary = "kurozumi", s0 = ...)`,
  and
  [`monitor_quantile()`](https://kvasilopoulos.github.io/exuber/reference/monitor_quantile.md)
  against the `O(T^2)` double recursion of `QPSY`.
- A statistic that looks new is exactly an existing one that the package
  already computes. Check this explicitly before you write any
  estimation code. The `SADF(k)` of Kurozumi equals `radf()$badf`, the
  feasible statistic of SV-ADF equals `radf()$badf`, and the `Q` of
  [`quantile_test()`](https://kvasilopoulos.github.io/exuber/reference/quantile_test.md)
  has the distribution of `radf()$adf`. Check the whole limit, though.
  The boundary of
  [`monitor_quantile()`](https://kvasilopoulos.github.io/exuber/reference/monitor_quantile.md)
  first reused `radf()$badf` for the `Q` part and treated the other
  component of the limit as one `z ~ N(0,1)` for each path. For a path
  functional that component is also a process over windows, and the
  single `z` made the test oversized (see “Validate” below).

When you triage again, render the PDF pages with PyMuPDF
(`fitz.Matrix(2.5-2.8, 2.5-2.8)`) and read them as images. Do not rely
on the raw text of `pdftotext -layout`. The extraction scrambles
subscripts, summation and fraction notation and Greek letters, and
trusting it has caused real transcription errors: the garbled eq. 18 in
Wu, Shi & Wu, eq. 4-6 in KNP, the window formulas in HLW, the `σ̃` in
Breitung & Diegel and Table I in Kurozumi & Nishi, among others.
`pdftotext -layout` is fine for bulk navigation and for searching for
keywords and equation numbers. Switch to rendered images before you
transcribe any formula that will ship.

### What makes an item well scoped

Prefer an item, or a sub-case of an item, that meets three conditions.

1.  It is a single recursion (`O(T)`) and not a double recursion
    (`O(T^2)`). That means it has the shape of `badf` and not of
    `bsadf`, unless the double-recursion case also reduces to a bounded
    closed-form band. `GSADF_{s0}` did, because its range of window
    starts is capped at a fixed fraction of the training length and does
    not grow with the current point.
2.  It reuses an existing statistic or an existing closed-form pattern
    and does not invent new estimation machinery from scratch.
3.  It comes with a published critical value, table or formula, so that
    no new Monte Carlo calibration is required.

When the main procedure of a paper is larger than this (a union of
rejections, a double recursion, a second or third family of statistics),
ship the minimal subset that is well scoped and document exactly what
you leave out and why. The project’s own precedent is to scope down
explicitly, as with
[`dating_hlw()`](https://kvasilopoulos.github.io/exuber/reference/dating_hlw.md)
without the fragmentation-joining heuristic and
[`contagion_reg()`](https://kvasilopoulos.github.io/exuber/reference/contagion_reg.md)
without the automatic delay search. It does not rush the whole procedure
and it does not skip the item. Triage the follow-ups again as well.
GSSU, CUSUM and the union, the multi-bubble dynamic programme of KNP and
QPSY were all scoped out in this way, and all of them turned out to be
cheap on a second reading (2026-09-29). The reasons were published
critical values for every statistic including the union constants,
`O(1)` prefix sums for each window, and a boundary simulation that needs
no QR fits.

### Validate before shipping, and be willing not to ship

Every implemented item needs the following, in this order.

1.  **Formula-exact check.** Compare the closed-form or vectorized
    version with an independent brute-force reimplementation
    ([`lm()`](https://rdrr.io/r/stats/lm.html), nested loops, a manual
    residual computation). They must agree to numerical precision,
    typically below `1e-8`. This check has found real bugs and has not
    only confirmed correct code. It found a window-width off-by-one in
    [`contagion_reg()`](https://kvasilopoulos.github.io/exuber/reference/contagion_reg.md),
    a matrix-orientation bug (`K %*% v` against `crossprod(K, v)`) in
    the LOOCV helper of the same file, and a wrong AR order in the
    bootstrap process of an abandoned `radf_qar()`.
2.  **Table and formula lookup check.** Match every published constant
    that the code uses exactly, and give a clean error for an
    unsupported level or parameter.
3.  **Monte Carlo size.** The empirical false-alarm rate under `H0`
    should be close to the nominal level or conservative relative to it.
    A rate that is a couple of points above nominal is not automatically
    Monte Carlo noise. For
    [`monitor_quantile()`](https://kvasilopoulos.github.io/exuber/reference/monitor_quantile.md)
    we recorded 6.7% at a nominal 5% as noise, and it turned out to be
    the single-`z` bug described above. Simulating the limiting process
    directly is cheap and tells you which case you are in. Also check
    away from the easy case, with non-central quantiles and heavy tails.
    QPSY has a false-alarm rate of 4% at the median but 35% at
    `tau = 0.9` even with Gaussian innovations (21% to 44% at
    `tau = 0.8` to `0.9` with `t3` innovations), and it now ships with a
    caveat that says so. A marginal quantile for each point, used as the
    boundary of a first-crossing or monitoring test, can look plausible
    from the formula and still be badly miscalibrated in practice. The
    boundary bug in
    [`monitor_quantile()`](https://kvasilopoulos.github.io/exuber/reference/monitor_quantile.md)
    gave a false-alarm rate of 50% against a nominal 5% until we
    calibrated it against the supremum of each simulated path. That
    matches how the `sadf_cv` of
    [`radf_mc_cv()`](https://kvasilopoulos.github.io/exuber/reference/radf_mc_cv.md)
    is built: it is the quantile of the simulated path maxima and not a
    quantile for each point.
4.  **Power check** against a true alternative, ideally compared with an
    existing statistic on the identical process, so that a power gap, if
    there is one, can be reported honestly and not hidden.
5.  Where possible, **reproduce the published Monte Carlo table of the
    paper** (`radf_qar()` attempted this against Table 2 of Pavlidis
    2025). This is the strongest check available, because it validates
    the whole pipeline and not only one piece.

If an item fails its own validation and you cannot find and fix the root
cause with confidence, do not ship it. Remove the half-validated code
instead of committing something that does not hold up. `radf_qar()` (the
quantile-AR `Un` and `QKS` tests of Pavlidis 2025) is the precedent. It
was implemented, and one real bug was found and fixed. A separate
diagnostic of oracle against bootstrap then found an unresolved
bootstrap-calibration problem at low and middle quantiles, which
persisted even at `nboot = 1999`, so simple Monte Carlo noise was ruled
out. We deleted the code and did not commit it. We wrote up the
diagnostic trail in `alternative-paradigms.md` in enough detail that a
future attempt starts from the remaining gap and does not repeat the
investigation.

### Documentation update pattern (five files and a replication script)

Every shipped item touches the following in `docs/`.

1.  The relevant taxonomy file: the status line at the top, the row in
    the taxonomy table, and either a new `### Implementation` subsection
    or a rewrite of the item’s own “not implemented” or
    cost-and-feasibility section. The update records what turned out to
    be wrong in the original assessment when you triaged again, and not
    only the final verdict.
2.  The relevant “Bundle N” section of `SUMMARY.md`: a narrative bullet
    with what was done, the main structural finding, concrete validation
    numbers, and what is still not implemented and why.
3.  The taxonomy table of `README.md` and its table of cross-checks for
    each item (one row with the item, the file, a description of the
    cross-check, and “clean” or “bug found and fixed: …”).
4.  The bullet list for each folder in `docs/replication/README.md`,
    pointing at a new replication script.
5.  `docs/parity.md`: a row for the new method, with the exuber column
    filled in and a dash for pyexuber, so that the Python side sees the
    gap.

Add a standalone replication script that can be rerun, in
`docs/replication/<taxonomy-folder>/<function>_validation.R`, which
reproduces every number quoted in the docs. Run the archived script
itself before you finalize the docs. The numbers from an ad hoc
validation script can drift from those of the final cleaned-up version,
for example through a different seeding order or process parameters
copied in by hand. The numbers in the `.md` files must match what the
archived script prints when you run it again, and not what an earlier
interactive exploration produced.

### Commit workflow

``` powershell
Rscript -e "devtools::document()"        # regenerate NAMESPACE/man
```

Then, from Bash or PowerShell in this directory:

    git status --short                       # confirm only intended files changed
    git add <intended files only>            # never `git add -A`; leave unrelated untracked
                                              # in-progress work alone

Write the commit message to a scratch file first, which avoids
shell-quoting problems with apostrophes in prose, and then run
`git commit -F <file>`. By the standing preference of the user, commits
carry no `Co-Authored-By` or AI-attribution trailers unless the user
asks for them. Make one semantic commit for each shipped item and do not
batch several items into one commit, even when you implemented them in
the same pass.

**Roxygen placement.** Define a new non-exported helper function before
the neighboring `#'` roxygen block, and not between that block and the
function it documents. If you insert a helper in between, roxygen
attaches the whole documentation block to the helper and not to the
exported function.

### Reusable low-level patterns

- `hls_prefix_sums(y)`, `hls_segment_ssr(ps, lo, hi, fit)` and
  `hls_segment_coef(ps, lo, hi)` in `R/dating_hls.R` hold the generic
  closed-form OLS over a segment. `ps$cx`, `cz` and so on are
  `c(0, cumsum(...))` vectors, and a segment `(lo, hi]` has the sum
  `ps$cx[hi+1] - ps$cx[lo+1]`. Reuse this pattern, or these functions
  themselves, before you write a new loop over windows by hand.
- `gls_dfstat_grid(y, minw)` in `R/radf_tt.R` computes the full
  `(r1, r2)` grid of no-intercept recursive-DF t-statistics, vectorized
  with [`outer()`](https://rdrr.io/r/base/outer.html) over prefix sums.
  It is the template for anything that needs the entire double-recursive
  grid and not a single-recursion path.
- `psy_minw(n)` and `psy_ds(n)` compute
  `floor((0.01 + 1.8/sqrt(n)) * n)` and `round(delta * log(n))`. Reuse
  these `log(n)`-based conventions and do not invent new rules for the
  minimum window or the minimum duration. They have repeatedly turned
  out to be exactly what the recommended formula of a paper reduces to,
  for example `r0 = 0.01 + 1.8/sqrt(T)` in SSU and the minimum duration
  requirement for consolidation in SV-ADF.
- `stamp(x)` in `R/radf-methods.R` converts a vector of TRUE or breach
  indices into contiguous `Start`, `End` and `Duration` runs. Reuse it
  for any new first-crossing or minimum-duration dating logic and do not
  write run-length detection by hand.
- [`radf_mc_cv()`](https://kvasilopoulos.github.io/exuber/reference/radf_mc_cv.md)
  shows the pattern for the boundary of a monitoring or first-crossing
  test. Simulate full null paths, take the supremum of each path, and
  then take the quantile of those maxima across replicates. Do not use a
  marginal quantile for each point at each recursion step. If you get
  this wrong, a test looks fine in a formula or structural check and is
  badly miscalibrated in practice (see the validation history of
  [`monitor_quantile()`](https://kvasilopoulos.github.io/exuber/reference/monitor_quantile.md)
  above).
- `cat_caveat(x)` and `get_caveat(x)` in `R/utils-attrs.R` serve
  functions whose source or validation status is not clean, for example
  a preprint that has not been peer reviewed or a known validation gap
  that is unresolved. Set `add_attr(..., caveat = <string>)`, emit the
  same string with `message_glue(caveat)` when the function is called,
  and call `cat_caveat(x)` in the `print.*_obj` method. One string is
  then kept in sync in three places and there are not three independent
  copies. For the established pattern, see `datestamp_svadf()` in
  `R/radf-methods.R` (a caveat on one `option` of a shared generic and
  not on its own class, so the `caveat` attribute is absent for the
  other options and `cat_caveat()` and `print.ds_radf()` do nothing for
  them) and
  [`radf_recovery()`](https://kvasilopoulos.github.io/exuber/reference/radf_recovery.md)
  (its own class).

## Writing style (all user-facing text)

Applies to READMEs, vignettes, the website, `docs/`, NEWS/CHANGELOG,
roxygen and docstrings, and any prose a reader sees. Code comments and
CLAUDE.md files follow it too.

**Voice.** An applied economist writing for colleagues who also want
ordinary readers to be able to run the test. Precise, sober, a little
plain-spoken. Define a term at first use (what “explosive” means, what a
critical value is for) and give the idea in words before the formula.

**Rewrite, do not substitute.** Swapping an em dash for a comma, colon
or hyphen keeps the machine-written rhythm and is not acceptable. If a
sentence needed a dash, it was carrying two thoughts: split it into two
sentences, or fold the aside into the grammar (a relative clause, a
parenthesis only for a true aside, or a separate sentence). No U+2014
and no spaced hyphen standing in for one. En dashes stay for numeric
ranges and joint names (Phillips–Shi–Yu).

**Patterns to remove at the sentence level.** - Fragments stacked for
effect, and “X, not Y” or “not just X, but Y” framings. State the claim
directly. - Triplets used for rhythm, and sentences that announce what
they are about to say (“Importantly,”, “It is worth noting that”, “In
essence”). - Telegraphic notes (dropped articles, arrows, semicolon
chains, “confirmed, zero new code”). Write full sentences with a subject
and a verb. - Status-report voice: “genuinely”, “confirmed”, “now done”,
“picked clean”, “the most topical candidate”. Say what is true and give
the date if it matters. - Marketing and filler words: seamlessly, robust
(unless a statistical sense is stated), leverage, delve, comprehensive,
powerful, crucial, landscape, journey, “under the hood”, “a rich set
of”. - Hedge stacks, and bold used as emphasis inside running prose. -
Self-reference to the writing process (“this resolves the question this
file flagged”, “an earlier pass”). Keep history in dated notes, not in
the body of explanations.

**Do keep.** Formulas, numbers, citations, function names and every
fact. This is a change of language, not of content. Vary sentence
length. Prefer “we” or the imperative to the passive, and say what a
function does and when to use it before how it works.
