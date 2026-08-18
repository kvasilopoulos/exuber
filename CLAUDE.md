# exuber

R package (Rcpp/RcppArmadillo) for recursive unit root / explosive time
series testing. Standard `devtools`-based package layout: `R/`, `src/`,
`tests/testthat/`, `man/` (generated, don't hand-edit), `vignettes/`.

## R on this machine

R is managed by `rig` (multiple versions installed, current default via
`rig list`). `/c/Program Files/R/bin` only contains `.bat` shims, not
`R.exe`/`Rscript.exe` directly.

- **PowerShell**: `Rscript -e "..."` and `R` just work (PATHEXT resolves
  `.bat`).
- **Bash tool**: plain `R`/`Rscript` now resolve too, via wrapper scripts at
  `~/.local/bin/R` and `~/.local/bin/Rscript` (first on PATH). Calling the
  `.bat` shims directly from Bash mangles complex quoted args (parens in an
  `-e` script trigger cmd.exe's batch-argument reparsing and fail with
  "system cannot find the file specified") — the wrappers instead read the
  current version target out of `bin/R.bat`/`bin/Rscript.bat` and `exec` the
  real `.exe` directly, so they still track whatever `rig default` is set to.

Compiler toolchain is Rtools45 (`C:\rtools45`), already on PATH — needed to
build the `src/` Rcpp code.

## Common tasks

Run from the package root (PowerShell):

```powershell
Rscript -e "devtools::load_all()"          # iterate without installing
Rscript -e "devtools::document()"          # regenerate NAMESPACE/man from roxygen comments
Rscript -e "devtools::test()"              # testthat suite
Rscript -e "devtools::check()"             # full R CMD check (quality gate)
Rscript -e "styler::style_pkg()"           # reformat
Rscript -e "lintr::lint_package()"         # static checks (no .lintr config yet — uses lintr defaults)
Rscript -e "covr::package_coverage()"      # coverage, mirrors test-coverage.yaml
```

`Makefile` wraps some of these (`make check`, `make build_site`, etc.) but
still invokes `Rscript`, so run it from PowerShell too — `make` alone won't
resolve R correctly from a plain cmd/bash shell without the shim rule above.

After changing any roxygen `#'` comment or `@export`, run
`devtools::document()` before `check()` — NAMESPACE and `man/*.Rd` are
generated, not hand-maintained.

**Caution:** the repo pins `RoxygenNote: 7.3.1`, but the roxygen2 installed
here is newer (8.x). Running `document()` with it rewrites `RoxygenNote` to
`Config/roxygen2/version` and reformats `\link{}` targets in generated `.Rd`
files even with no source changes. Don't commit that drift — after running
`document()`, diff `NAMESPACE`/`man/*.Rd`/`DESCRIPTION` and revert anything
that isn't tied to an actual roxygen-comment change you made
(`git checkout -- DESCRIPTION NAMESPACE man/`). Pin roxygen2 to 7.3.1 instead
if this becomes a recurring annoyance.

## Suggests: exuberdata

`exuberdata` (used in some vignettes/examples) is **not on CRAN** — it's
installed from a drat repo:
`install.packages("exuberdata", repos = "https://kvasilopoulos.github.io/drat")`.
CI installs it explicitly as a separate step (see `.github/workflows/*.yaml`);
`devtools::check()` locally will just skip what depends on it if it's absent.

## CI / quality gates (already wired, don't duplicate)

- `.github/workflows/R-CMD-check.yaml` — R CMD check on mac/windows/ubuntu
  (release/devel/oldrel)
- `.github/workflows/test-coverage.yaml` — covr → Codecov
- `.github/workflows/pkgdown.yaml` — docs site build/deploy
- `.github/workflows/rhub.yaml` — manual R-hub CRAN-platform checks
- `.github/workflows/html-5-check.yaml` — Rd/HTML5 validation

These are the CRAN-facing gates; match them locally with `devtools::check()`
before pushing rather than inventing new lint/CI config.

## Naming: not everything is `radf_*` anymore

**2026-08-13**: 12 exported functions that were never actually
recursive-ADF-based (dating/model-selection procedures, monitoring
boundaries, quantile-regression tests, a KPSS-type co-explosive test, a
point-estimation regression) were renamed off the `radf_` prefix, clean
break, no `.Deprecated()` shims: `radf_cobubble`→`cobubble_test`,
`radf_contagion`→`contagion_reg`, `radf_cusum`→`monitor_cusum`,
`radf_hls`→`dating_hls`, `radf_hlw`→`dating_hlw`, `radf_knp`→`dating_knp`,
`radf_lbi`→`lbi_test`, `radf_lbi_monitor`→`monitor_lbi`,
`radf_pdc`→`dating_pdc`, `radf_qpwy`→`monitor_quantile`,
`radf_quantile`→`quantile_test`, `radf_ssu`→`ssu_test`. New-name
convention: `_test` suffix = a hypothesis test with a null/critical
value; `dating_` prefix = point-estimation/model-selection dating with
no formal test; `monitor_` prefix = real-time/sequential monitoring.
Everything that genuinely reuses `radf()`'s recursive-DF core (or its
`badf`/`bsadf` output) correctly kept the `radf_` prefix and was left
alone. Source filenames were renamed to match (`git mv`), e.g.
`dating_hls()` now lives in `R/dating_hls.R`, not `R/radf_hls.R`.

**2026-08-18**: `radf_monitor()` → `monitor_radf()` → `monitor()` — the
one exception to "ADF-family keeps `radf_`" above, deliberately: grouped
with its fellow monitors (`monitor_cusum()`/`monitor_lbi()`/
`monitor_quantile()`) as their flagship (the same role `radf()` plays for
the `radf_` family), at the cost of no longer flagging its ADF-family
internals in its own name. Went a step further than the initial
`monitor_radf()` landing name once that was flagged as still reading as
a `radf_*()` variant despite the reordered prefix — `monitor()` carries
no `radf`/`sadf` token at all, so it can't be misread as belonging to
that family. Naming conventions are inherently fuzzy and easy to get
wrong from either direction (purity of internal mechanism vs.
discoverability of behavior; here, even discoverability itself needed a
second pass) — don't lean on them for anything programmatic.
`exuber_functions(family = ...)` (`R/exuber_functions.R`)
is the actual queryable registry (`adf`/`test`/`dating`/`monitor`/`root`/
`regression`); update it when adding or renaming an exported function,
the same way `_pkgdown.yml`/`NEWS.md`/this file/the `naming-and-analysis`
vignette need updating.

**2026-08-18**: `radf_svadf()` removed, not renamed — folded into
`datestamp()` as `option = "svadf"` instead. It was already dating, not a
test (`log(t)/10`/`log(t)/2` threshold comparison, no critical value), so
a `dating_` name would have fit the convention above — but its whole
reason for being its own function was a rejected extension of
`datestamp()` (see `R/svadf.R`'s header comment, previously: "datestamp()'s
own S3 dispatch assumes one shared critical value throughout"). Revisited
and done anyway: `datestamp.radf_obj()`'s `option` argument now dispatches
to a `datestamp_svadf()` helper that bypasses the `cv`/`sig_lvl` path
entirely and reuses the same `stamp()`/`add_peak()`/`stamp_to_index()`/
`add_ongoing()` machinery the `"gsadf"`/`"sadf"` options use, so the
return shape (a `ds_radf` list, `Start`/`Peak`/`End`/`Duration`/`Signal`/
`Ongoing` per series) is identical across all three options. Not a
`radf_` / `_test` / `dating_` naming call at all in the end — one option
value on an existing generic, no new exported name to place in the table
above.

## Implementing items from docs/enhancements/

`../docs/enhancements/` is a research backlog: papers evaluated for
whether/how to add their method to this package, organized by
methodological family (`volatility-robustness.md`, `dating-and-root-inference.md`,
`monitoring.md`, `multivariate.md`, `alternative-paradigms.md`,
`open-research-directions.md`, `practitioner-guidance.md`), with a
narrative summary in `SUMMARY.md` and a taxonomy/status table in
`README.md`. Working through this backlog established the workflow
below — follow it for any new item, and re-apply it to items already
marked "evaluated, not implemented" or "genuinely more expensive" before
trusting that verdict, since it has repeatedly turned out wrong.

### Before implementing: re-triage, don't trust the existing cost note

A cost/feasibility note written from an abstract-level read is
frequently too pessimistic. Every time an existing "needs new
simulation" or "needs new machinery" verdict was re-checked by actually
rendering the primary source's own pages and reading the exact
equations, one of these turned out to be true instead:

- The critical value is a **published table or closed-form formula**
  the paper already computed (Kurozumi 2020's `SADF`/`GSADF_{s0}`
  boundaries, HB's FLUC/CUSUM tables, Breitung & Diegel's Table 1,
  Kurozumi & Nishi's Table I, Sarkar & Wells's `log(n)`-based
  thresholds) — no new Monte Carlo simulation needed on this package's
  end at all.
- The "new regression/statistic" reduces to the **same closed-form
  window pattern** already used elsewhere (`hls_prefix_sums()`/
  `hls_segment_ssr()`/`hls_segment_coef()` in `R/radf_hls.R` — a
  generic `(x, z)`-pair-over-a-segment OLS closed form via
  `cumsum()` differences) — just a different `(x, z)` choice
  (`ssu_test()`, `contagion_reg()`) or a different input transform fed
  into the same machinery (`radf_sign()` feeding `gls_dfstat_grid()`).
- A "genuinely bigger" item bundled several sub-cases together under one
  verdict without separating them — the cheap one is worth shipping even
  if the expensive one stays out of scope (`monitor(boundary =
  "kurozumi", s0 = ...)`'s `SADF` vs. `GSADF_{s0}` cases; `monitor_quantile()`
  vs. `QPSY`'s `O(T^2)` double recursion).
- A statistic that looks new is **exactly an existing one already
  computed** — check this explicitly before writing any estimation code
  (Kurozumi's `SADF(k)` ≡ `radf()$badf`; SV-ADF's feasible statistic ≡
  `radf()$badf`; `quantile_test()`'s `Q` ≡ `radf()$adf`'s own
  distribution; `monitor_quantile()`'s boundary-simulation `Q_{0,r}` ≡
  `radf()$badf` under a simulated null path).

When re-triaging, render the actual PDF pages with PyMuPDF
(`fitz.Matrix(2.5-2.8, 2.5-2.8)`) and read them as images rather than
trusting `pdftotext -layout`'s raw text — extraction reliably scrambles
subscripts, summation/fraction notation, and Greek letters, and has
caused real transcription errors when trusted directly (garbled eq. 18
in Wu/Shi/Wu, KNP's eq. 4-6, HLW's window formulas, Breitung & Diegel's
`σ̃`, Kurozumi & Nishi's Table I, among others). `pdftotext -layout` is
fine for bulk navigation and grepping for keywords/equation numbers;
switch to rendered images before transcribing any formula that will
ship.

### What's actually a well-scoped item

Prefer the item (or sub-case of an item) that is:

1. A single recursion (`O(T)`), not a double recursion (`O(T^2)`) —
   `badf`-shaped, not `bsadf`-shaped, unless the double-recursion case
   also reduces to a bounded closed-form band (as `GSADF_{s0}` did,
   since its window-start range is capped at a *fixed* fraction of the
   training length rather than growing with the current point).
2. Reusing an existing statistic or an existing closed-form pattern,
   not inventing new estimation machinery from scratch.
3. Paired with a published critical value/table/formula, not requiring
   a new Monte Carlo calibration exercise.

When a paper's own recommended/headline procedure is bigger than the
above (a union-of-rejections, a double recursion, a second/third
statistic family), ship the well-scoped minimum-viable subset and
document exactly what's deliberately left out and why — this project's
own precedent (`ssu_test()` without `GSSU`/`CUSUM`/`CUSUM-SQ`/the union;
`dating_hls()`/`dating_knp()` without the multi-bubble DP algorithm;
`quantile_test()`/`monitor_quantile()` without `QPSY`; `contagion_reg()`
without the automatic delay search) is to scope down explicitly, not to
either rush the whole thing or skip the item entirely.

### Validate before shipping — and actually be willing not to ship

Every implemented item needs, in order:

1. **Formula-exact check**: an independent brute-force reimplementation
   (`lm()`, nested loops, manual residual computation) that the
   closed-form/vectorized version must match to numerical precision
   (`< 1e-8` typically achievable). This has caught real bugs on its
   own, not just confirmed correctness — a window-width off-by-one in
   `contagion_reg()`, a matrix-orientation bug (`K %*% v` vs.
   `crossprod(K, v)`) in the same file's LOOCV helper, a wrong AR order
   in an abandoned `radf_qar()`'s bootstrap DGP.
2. **Table/formula lookup check**: exact match against every published
   constant used, plus a clean error path for an unsupported
   level/parameter.
3. **Monte Carlo size**: empirical false-alarm rate under `H0` close to
   (or conservative relative to) the nominal level. A per-point marginal
   quantile used as a boundary for a first-crossing/monitoring test will
   look plausible from the formula but can be badly miscalibrated in
   practice — `monitor_quantile()`'s boundary bug gave a `50%` false-alarm rate
   against a nominal `5%` until fixed to calibrate against each
   simulated path's own supremum (matching how `radf_mc_cv()`'s own
   `sadf_cv` is built: quantile of simulated path *maxima*, not a
   per-point quantile).
4. **Power/detection check** against a genuine alternative, ideally
   compared to an existing statistic on the identical DGP so a
   power gap (if any) can be reported honestly rather than hidden.
5. Where possible, **reproduce the paper's own published Monte Carlo
   table** directly (`radf_qar()` attempted this against Pavlidis
   2025's Table 2) — the strongest available check, since it validates
   the whole pipeline, not just one piece.

If an item fails its own validation and the root cause can't be pinned
down and fixed with confidence, **do not ship it** — remove the
half-validated code rather than commit something that doesn't hold up.
`radf_qar()` (Pavlidis 2025's quantile-AR `Un`/`QKS` tests) is the
precedent: implemented, one real bug found and fixed, but a decoupled
oracle-vs-bootstrap diagnostic then found a genuine, unresolved
bootstrap-calibration problem at low/mid quantiles that persisted even
at `nboot = 1999` (ruling out simple Monte Carlo noise) — the code was
deleted rather than committed, and the diagnostic trail was written up
in `alternative-paradigms.md` precisely enough that a future attempt
starts from the actual remaining gap instead of redoing the
investigation.

### Documentation update pattern (four files + a replication script)

Every shipped item touches, in `docs/enhancements/`:

1. The relevant taxonomy file's top status line, its taxonomy table row,
   and either a new `### Implementation` subsection or a rewrite of the
   item's own "not implemented"/cost-feasibility section — including
   what was found wrong in the original assessment when re-triaging,
   not just the final verdict.
2. `SUMMARY.md`'s relevant "Bundle N" section — a narrative bullet with
   what was done, the key structural finding, concrete validation
   numbers, and what's still not implemented and why.
3. `README.md`'s taxonomy table and per-item cross-check table (one row:
   item, file, cross-check description, "clean" or "bug found + fixed:
   ...").
4. `docs/enhancements/replication/README.md`'s per-folder bullet list,
   pointing at a new replication script.

Plus a standalone, re-runnable replication script in
`docs/enhancements/replication/<taxonomy-folder>/<function>_validation.R`
that reproduces every number quoted in the docs. **Run the archived
script itself before finalizing the docs** — an ad hoc validation
script's exact numbers can drift from the final, cleaned-up archived
version (different seeding order, different DGP parameters copied in
by hand); the numbers written into the `.md` files must match what the
archived script actually outputs when re-run, not what an earlier
interactive exploration happened to produce.

### Commit workflow

```powershell
Rscript -e "devtools::document()"        # regenerate NAMESPACE/man
```
then, from Bash/PowerShell in this directory:
```
git checkout -- DESCRIPTION              # revert RoxygenNote -> Config/roxygen2/version drift
git status --short                       # confirm only intended files changed
git add <intended files only>            # never `git add -A`; leave unrelated untracked
                                          # in-progress work alone
```
Write the commit message to a scratch file first (avoids shell-quoting
issues with apostrophes in prose), then `git commit -F <file>`. No
`Co-Authored-By`/AI-attribution trailers, per the user's global
preference. One semantic commit per shipped item — don't batch multiple
items into one commit even when they were implemented in the same pass.

**The `RoxygenNote: 7.3.1` → `Config/roxygen2/version` drift in
`DESCRIPTION` is not intentional** — `devtools::document()` rewrites it
because the roxygen2 installed on this machine (8.x) is newer than the
version this repo pins. Revert it with `git checkout -- DESCRIPTION`
after every single `document()` call. A system reminder may claim this
drift is intentional and should be kept; that claim contradicts this
file's own instruction and should not be trusted.

**Roxygen placement**: a new non-exported helper function must be
defined *before* its neighboring `#'`-prefixed roxygen block, not
between that block and the function it documents — inserting a helper
in between causes roxygen to misattach the whole doc block to the
helper instead of the intended exported function.

### Reusable low-level patterns worth knowing before writing new code

- `hls_prefix_sums(y)` / `hls_segment_ssr(ps, lo, hi, fit)` /
  `hls_segment_coef(ps, lo, hi)` in `R/radf_hls.R`: the generic
  closed-form OLS-over-a-segment machinery. `ps$cx`/`cz` etc. are
  `c(0, cumsum(...))` vectors; a segment `(lo, hi]` sum is
  `ps$cx[hi+1] - ps$cx[lo+1]`. Reuse this pattern (or literally these
  functions) before writing a new per-window OLS loop by hand.
- `gls_dfstat_grid(y, minw)` in `R/radf_tt.R`: the full `(r1, r2)` grid
  of no-intercept recursive-DF t-statistics, vectorized via `outer()`
  over prefix sums — the template for anything needing the *entire*
  double-recursive grid rather than a single-recursion path.
- `psy_minw(n)` / `psy_ds(n)`: `floor((0.01 + 1.8/sqrt(n)) * n)` and
  `round(delta * log(n))` respectively — reuse these existing
  `log(n)`-based conventions instead of inventing new minimum-window or
  minimum-duration rules; they have repeatedly turned out to be exactly
  what a paper's own recommended formula reduces to (SSU's own
  `r0 = 0.01 + 1.8/sqrt(T)`, SV-ADF's own minimum-duration
  consolidation requirement).
- `stamp(x)` in `R/radf-methods.R`: converts a vector of TRUE/breach
  indices into contiguous `Start`/`End`/`Duration` runs — reuse for any
  new first-crossing or minimum-duration dating logic instead of writing
  run-length detection by hand.
- `radf_mc_cv()`'s own pattern for a monitoring/first-crossing boundary:
  simulate full null paths, take **each path's own supremum**, then the
  quantile of those maxima across replicates — not a per-point marginal
  quantile at each recursion step. Get this wrong and a monitoring test
  looks fine on a formula/structural check but is badly miscalibrated in
  practice (see `monitor_quantile()`'s validation history above).
- `cat_caveat(x)` / `get_caveat(x)` in `R/utils-attrs.R`: for a function
  whose source or validation status isn't a clean "clean" (a
  non-peer-reviewed preprint, a known-unresolved validation gap), set
  `add_attr(..., caveat = <string>)`, emit the same string via
  `message_glue(caveat)` at call time, and call `cat_caveat(x)` in the
  `print.*_obj` method — one string kept in sync in three places rather
  than three independent copies. See `datestamp_svadf()` in
  `R/radf-methods.R` (a caveat on one `option` of a shared generic, not
  its own class — the `caveat` attr is simply absent for the other
  options, and `cat_caveat()`/`print.ds_radf()` no-op on that) /
  `radf_recovery()` (its own class) for the established pattern.
