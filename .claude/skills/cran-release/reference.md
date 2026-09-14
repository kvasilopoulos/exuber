# CRAN submission rules — reference (checked 2026-09-14)

Condensed from the CRAN Repository Policy (rev. 6875), the CRAN Cookbook
(contributor.r-project.org/cran-cookbook, written by the CRAN team), Davis
Vaughan's `extrachecks`, *R Packages* ch. "Releasing to CRAN", Writing R
Extensions, and `usethis::use_release_issue()` (usethis 3.2.1, devtools
2.5.2). Sections 1–5 are what CRAN checks or reviewers ask for; 6–8 are the
workflow other packages (tidyverse, r-lib) follow.

## 1. DESCRIPTION

| Field | Rule |
|---|---|
| `Package` | can't clash with any current/past CRAN or current Bioconductor package; can't be renamed later. |
| `Title` | Title Case (`tools::toTitleCase()`), ≤ 65 chars, no trailing period, no "package"/the package's own name, software names in 'single quotes'. |
| `Description` | one paragraph, ≥ 2 sentences, what it does *and* why; not "This package…"/"Functions for…"; software/package/API names in 'single quotes' (case-sensitive; also suppresses the spell-check NOTE); expand non-obvious acronyms; cite methods as `Authors (year) <doi:10.xxx>`, `Authors (year, ISBN:…)` or `<https://…>` — no space after `doi:`/`https:`, angle brackets for auto-linking; publication titles in double quotes only. |
| `Authors@R` | required form; exactly one `cre` who is a person (not a list) with a monitored email; a `cph` (copyright holder) — reviewers ask for it on first submission; `comment = c(ORCID = "…")` welcome. Never hand-write `Author:`/`Maintainer:` (auto-derived; mismatch = automatic rejection). Maintainer change → explain in comments, confirm from old address. |
| `License` | from R's license DB (`R.home("share/licenses/license.db")`). `+ file LICENSE` only for templates that need it (MIT, BSD-2/3): file holds just `YEAR:`/`COPYRIGHT HOLDER:` and the year must be current. GPL/LGPL/Apache: no file, no `+ file LICENSE`. License change must be highlighted at update. Bundled third-party code: its license compatible and credited (`cph` or `inst/COPYRIGHTS`). |
| `Depends` | `R (>= 4.0)` if `tools::R_user_dir()` used; otherwise only what's needed. |
| `Imports`/`LinkingTo` | CRAN/Bioconductor only, not orphaned/archived (check <https://cran.r-project.org/web/checks/check_results_PKG.html> of each). |
| `Suggests`/`Enhances` | used conditionally (`requireNamespace()`, `@examplesIf`, `skip_if_not_installed()`); non-CRAN ones need `Additional_repositories:` with a working repo. |
| `SystemRequirements` | external libraries named; installation must first look for an installed version; source preferred over download; never pre-compiled binaries without CRAN agreement. |
| `Version` | increased on every submission; no `.9000` dev suffix ("Version contains large components" NOTE). |
| `URL`/`BugReports` | HTTPS, final destinations (no redirects), all reachable — `urlchecker`. |

## 2. Documentation (Rd)

- Every `.Rd` for an exported object has `\value` describing class/structure
  and meaning; side-effect functions: `\value{No return value, called for
  side effects}`. Datasets (`\docType{data}`) are exempt. With roxygen:
  `@return`, then `devtools::document()` — fixing `man/` by hand gets
  rejected again on resubmission.
- Every exported function with a meaningful result has `@examples`.
  Unexported functions with examples → `@noRd` (or call via `:::`).
- Example wrappers, in order of preference:
  - unwrapped: small toy data, < 5 s, ideally an edge case too;
  - `\donttest{}`: > 5 s or downloads data; still run by
    `--as-cran --run-donttest` on incoming, so must *work*;
  - `\dontrun{}`: only when genuinely unexecutable (missing credentials,
    external software); reviewers reject it otherwise;
  - `if (interactive()) {}`: shiny, viewers, browsers;
  - `try()`: examples meant to error;
  - `if (requireNamespace("pkg", quietly = TRUE)) {}` / `@examplesIf`: Suggests;
  - `\dontshow{}`: setup/cleanup (restoring `par`/`options`, `unlink()`).
- No commented-out code in examples ("remove or use \donttest").
- Rd must validate as HTML5 (`_R_CHECK_RD_VALIDATE_RD2HTML_=TRUE`, needs
  `tidy`); math needs `\eqn`/`\deqn` that KaTeX renders.
- Vignettes: build time counts toward the 10-min total; precompute heavy
  results or use `eval = FALSE` chunks sparingly; `VignetteBuilder` present.
- `NEWS.md` top heading = the version being submitted (tidyverse style:
  user-facing, grouped, `@user`/`#issue` credits).

## 3. Code behaviour (policy "malicious or anti-social")

| Rule | Detail |
|---|---|
| Console output | no `print()`/`cat()`/`writeLines()` in functions except print/summary/format methods and interactive tools; use `message()`/`warning()`/`stop()` or a `verbose =` argument. |
| State | never change `options()`, `par()`, `setwd()`, `Sys.setenv()`, `Sys.setlocale()`, RNG seed without restoring: `old <- par(...); on.exit(par(old), add = TRUE)` in functions; explicit restore lines in examples/vignettes. No `set.seed()` inside functions (offer `seed = NULL` argument). No `options(warn = -1)`. |
| Files | write only to `tempdir()`/`tempfile()` or (R ≥ 4.0) `tools::R_user_dir(pkg, "cache"|"config"|"data")` kept small and actively managed; no default output paths in the working directory or home; clean tempdir in examples/tests (`unlink()`, `withr::local_tempfile()`) — "detritus in the temp directory" NOTE. No clipboard writes. No installing into R's own dirs. |
| Global env | no `<<-` to `.GlobalEnv`, no `assign(..., envir = globalenv())`, no `rm(list = ls())` in examples; don't touch `.Random.seed`. |
| Packages | never `install.packages()` from code/examples/tests; no `installed.packages()` (slow) — use `requireNamespace()`; no modifying other namespaces. |
| Cores | ≤ 2 threads/cores in examples, tests, vignettes (`parallel::detectCores()` default is a rejection); honour `_R_CHECK_LIMIT_CORES_`/`MC_CORES`; OpenMP/BLAS threads included. |
| Internet | must "fail gracefully with an informative message" — a `message()`/`warning()` and a clean return, not an `stop()` that ERRORs the check when offline; HTTPS only, certificates verified, requests minimal and cached; avoid 429/403; check machines usually *have* network but the incoming test can be run offline. |
| External programs | never launch viewers/browsers/PDF readers in examples/tests unless closed again; `system()` calls portable. |
| Compiled code | no `exit()`/`abort()`/`printf` to stdout — `Rf_error()`, `Rprintf()`; compiles with `-Wall -pedantic` without warnings on gcc and clang; no disabling diagnostics or stack checking; no stripping symbols; portable (no `-march=native`, no `-O3` overrides in Makevars); registered native routines (`useDynLib(pkg, .registration = TRUE)`); passes ATLAS/MKL/OpenBLAS numeric tolerance, valgrind, ASAN/UBSAN (CRAN's "additional issues" — reproduce with R-hub containers). Static libs only on Windows/macOS. |
| API | public R API only: no `.Internal()`, no `.Call()` into base packages, no `:::` into other packages, no undeclared entry points. |
| Style asks | `TRUE`/`FALSE` not `T`/`F` (and not as variable names). |

## 4. Files and size

- Tarball ≤ 10 MB (built with `R CMD build` on current R release/patched,
  named `PKG_VERSION.tar.gz`); installed data ≤ 5 MB and docs ≤ 5 MB; > 5 MB
  installed size gives a NOTE that must be justified, and every future
  submission gets flagged. Big data → separate data package or external
  download with `Additional_repositories`.
- No object files, binaries or hidden dirs in the tarball; `src/Makevars`
  needs a `clean:` target if subdirectories produce `.o` files.
- `.Rbuildignore` (regex, anchored): `cran-comments.md`, `CRAN-SUBMISSION`,
  `CRAN-RELEASE`, `README.Rmd`, `.github`, `_pkgdown.yml`, `docs`,
  `pkgdown`, `codecov.yml`, `.claude`, `CLAUDE.md`, `*.Rproj`, `data-raw`,
  `revdep`, checklists. "Non-standard file/directory found at top level"
  NOTE = something missing here.
- `inst/WORDLIST` for `spelling`; `inst/CITATION` optional but checked for
  syntax if present.

## 5. R CMD check gates

- Run on the *tarball* with current R-devel, `--as-cran`: 0 ERROR, 0
  WARNING, no "significant" NOTE. Acceptable NOTEs: "New submission", "New
  maintainer", installed size for a data package, "Possibly misspelled" for
  proper names already in WORDLIST — each explained in `cran-comments.md`.
  Anything else: fix it ("almost always easier than convincing CRAN").
- Incoming feasibility check also reports: days since last update (< 7 d
  is questioned unless CRAN asked), version format, maintainer mismatch,
  archived status, non-FOSS license, `Suggests`/`Imports` not on CRAN,
  DOIs/URLs, Title/Description spelling.
- Timing: each example ≤ 5 s ("Examples with CPU (user + system) or
  elapsed time > 5s"), whole check ≤ 10 min ("Overall checktime NOTE").
  CRAN's Windows and Linux pretest run examples, `\donttest`, tests and
  vignettes; `skip_on_cran()` for slow/network tests (NOT_CRAN is unset on
  CRAN).
- Platforms: at least two major OSes must work. Before submitting: local
  `devtools::check(remote = TRUE, manual = TRUE)`, `check_win_devel()` (email
  in ~30 min), R-hub v2 `rhub_check()` — GitHub-Actions based, needs
  `rhub::rhub_setup()` once + a GitHub PAT; useful platforms: `linux`,
  `windows`, `macos`, `macos-arm64`, `atlas`, `mkl`, `nosuggests`, `valgrind`,
  `gcc-asan`/`clang-asan`, `ubuntu-next`, `clang20`. `devtools::check_mac_release()`
  → macbuilder for arm64 issues CRAN mentions.
- After a new R x.y.0: packages that ERROR are archived unless fixed by the
  emailed deadline; WARNINGs/significant NOTEs draw "please fix" emails.

## 6. Submission mechanics

- Web form only: <https://cran.r-project.org/submit.html> (or
  `devtools::submit_cran()`, which builds, uploads, writes `CRAN-SUBMISSION`
  with the SHA for `use_github_release()` later). Never email the tarball.
- Confirmation email to the maintainer must be clicked; then status emails:
  "pretest" (auto) → for new packages or flagged updates, human review ("Dear
  maintainer, ... please fix and resubmit") → "on its way to CRAN" → "on
  CRAN". One submission in flight at a time.
- Cadence: not more than one release per 1–2 months once established; wait
  ≥ 48 h after publication before checking `check_results_PKG.html`; consult
  that page before every update.
- Reverse dependencies: run `revdepcheck::revdep_check(num_workers = 4)`
  (or `tools::check_packages_in_dir()`); an API break needs maintainers
  notified ≥ 2 weeks ahead; report results in `cran-comments.md`.
- Optional comment / `cran-comments.md` contents (tidyverse template,
  `usethis::use_cran_comments()`):
  1. `## R CMD check results` — `0 errors | 0 warnings | 1 note` + each NOTE
     explained; 2. `## Test environments` (local, win-builder, R-hub,
     GH Actions with R versions); 3. `## revdepcheck results` (count checked,
     new problems, or "no reverse dependencies"); 4. anything reviewers
     need: internet use, large size, license change, maintainer change,
     acronyms, special build needs; 5. `## Resubmission` — for each
     reviewer comment, the change made. Keep it short; reviewers read it.
- Rejection handling: fix at the source (`R/`, not `man/`), re-document,
  bump patch, re-run all checks, resubmit with the `## Resubmission`
  section; answer the reviewer's questions by replying to the email with
  `cran-submissions@r-project.org` in CC.

## 7. usethis / tidyverse release workflow (what other packages do)

`usethis::use_release_issue(version)` opens a GitHub issue with this
checklist; run it in this order.

First release only: `use_cran_comments()`; aspirational install
instructions in README; proofread Title/Description; every export has
`@return` + `@examples`; `cph` in `Authors@R`; bundled-file licensing;
review `extrachecks`.

Prepare: `git pull` → check current CRAN results → advance `lifecycle`
deprecations → polish NEWS → `urlchecker::url_check()` →
`devtools::build_readme()` → `devtools::check(remote = TRUE, manual = TRUE)`
→ `devtools::check_win_devel()` → (revdeps) → update `cran-comments.md` →
`git push` → (draft blog post).

Submit: `usethis::use_version('major'|'minor'|'patch')` →
`devtools::submit_cran()` → approve email.

After acceptance: `usethis::use_github_release()` →
`usethis::use_dev_version(push = TRUE)` (→ `x.y.z.9000`, NEWS heading back
to "(development version)") → pkgdown news link / announcement.

Related tooling packages: `devtools`, `usethis`, `rcmdcheck`, `rhub` (v2),
`urlchecker`, `spelling`, `revdepcheck` (r-lib, GitHub), `checkhelper`
(ThinkR: `find_missing_tags()`, `check_clean_userspace()`), `goodpractice`,
`lifecycle`, `withr` (state restoration), `testthat` (`skip_on_cran()`,
`skip_if_offline()`, `skip_if_not_installed()`), r-lib/actions
`check-r-package` for CI on release/devel/oldrel.

## 8. Compiled / Rcpp packages specifically

- `LinkingTo: Rcpp, RcppArmadillo`; `Imports: Rcpp`; `useDynLib(pkg,
  .registration = TRUE)`; `RcppExports` regenerated (`Rcpp::compileAttributes()`
  via `devtools::document()`).
- `src/Makevars`: no `-O` overrides, no `-march`; `PKG_CXXFLAGS` only for
  standards/defines; `CXX_STD = CXX17` if needed; `clean:` target for
  nested dirs.
- Vendored C++ (e.g. a core library under `src/vendor/`): its license in
  `inst/COPYRIGHTS` or `Authors@R` `cph`; no prebuilt objects; sources
  compile on gcc, clang, and Rtools MinGW; no compiler warnings under
  `-Wall -pedantic`.
- Numerics: tests/examples use tolerances that survive ATLAS/MKL/OpenBLAS
  and Apple Accelerate; R-hub `atlas`/`mkl`/`macos-arm64` before release.
- Memory: run `valgrind`/`gcc-asan` containers once per release touching
  C++; CRAN's "Additional issues" column will otherwise catch it after
  publication.
- Threads: Armadillo/OpenMP/BLAS threads count toward the 2-core limit;
  set `ARMA_DONT_USE_OPENMP` or cap `OMP_NUM_THREADS` during check if
  the library multithreads by default.

## Sources

- https://cran.r-project.org/web/packages/policies.html
- https://contributor.r-project.org/cran-cookbook/ (general, docs, code, DESCRIPTION chapters)
- https://github.com/DavisVaughan/extrachecks
- https://r-pkgs.org/release.html
- https://cran.r-project.org/doc/manuals/r-release/R-exts.html
- https://r-hub.github.io/rhub/
- https://usethis.r-lib.org/reference/use_release_issue.html
- https://style.tidyverse.org/news.html#news-release
- https://lifecycle.r-lib.org/articles/communicate.html
- https://cran.r-project.org/submit.html
