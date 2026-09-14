---
name: cran-release
description: Audit an R package against the CRAN Repository Policy, CRAN reviewer conventions (CRAN Cookbook, extrachecks) and the usethis release workflow, and drive a CRAN submission (checks on CRAN platforms, cran-comments.md, submit, post-acceptance). Use when asked to submit to CRAN, prepare/cut an R release, check CRAN readiness, answer a CRAN reviewer email, or fix an R CMD check NOTE/WARNING.
---

# CRAN release

Two modes; default to **audit** if unclear.

- **audit** – read the package, compare against `reference.md`, report gaps as
  a table (file:line, what, fix, severity). Don't edit unless asked.
- **release** – if the repo has its own checklist (`cran-release-checklist.md`,
  `CLAUDE.md` "Release process", `usethis::use_release_issue()` issue), run
  that and update its status column in place. Fill gaps from this skill;
  never invent a parallel process.

## Audit procedure

1. Read `DESCRIPTION`, `NEWS.md`, `cran-comments.md`, `.Rbuildignore`,
   `LICENSE*`, `inst/WORDLIST`, `.github/workflows/*`, `R/zzz.R`, every
   `@examples` block (`rg -n "@examples|\\\\dontrun|\\\\donttest" R/`),
   `tests/testthat/` for `skip_*` usage.
2. Run the mechanical checks (PowerShell on this machine; see repo
   `CLAUDE.md` for the R/Rscript quirks):

```r
devtools::document()                      # Rd in sync with roxygen (reviewers reject stale man/)
devtools::check(cran = TRUE, remote = TRUE, manual = TRUE,
                env_vars = c(NOT_CRAN = "false"))   # what CRAN runs; manual=FALSE if no LaTeX
urlchecker::url_check(); urlchecker::url_update()
spelling::spell_check_package()
checkhelper::find_missing_tags()          # every exported Rd has \value + examples
tools::package_dependencies("PKG", reverse = TRUE)   # revdeps → revdepcheck::revdep_check()
```

3. Check each row against the code; `reference.md` has the rule text.

| Area | Must hold |
|---|---|
| DESCRIPTION | `Title` in Title Case, ≤ 65 chars, no "package"/pkg name; `Description` ≥ 2 sentences, not starting "This package"/"Functions for", software names in 'single quotes', acronyms expanded, methods cited `Authors (year) <doi:...>` with no space after `doi:`; `Authors@R` with one `cre` (real, monitored email) and a `cph`; `License` from R's license DB, `+ file LICENSE` only for MIT/BSD-style templates (year current); `Depends: R (>= 4.0)` if `tools::R_user_dir()` used; no `Maintainer:`/`Author:` hand-edits. |
| Rd / roxygen | Every exported function: `@return` (or "No return value, called for side effects") and runnable `@examples`. `\dontrun{}` only if truly unexecutable; `\donttest{}` for > 5 s or network; `@examplesIf` / `requireNamespace()` for Suggests; `if (interactive())` for shiny/viewers; `try()` for expected errors. No commented-out example code. Unexported functions with examples: `@noRd`. |
| Timing | each example ≤ 5 s; whole check ≤ 10 min; ≤ 2 cores/threads by default in examples/tests/vignettes. |
| Side effects | no `print()`/`cat()` outside print/summary methods (use `message()`/`verbose=`); `options()`/`par()`/`setwd()` restored via `on.exit()` in functions and explicitly in examples/vignettes; no `set.seed()` inside functions; no writes outside `tempdir()`/`R_user_dir()`; no tempdir detritus; no `.GlobalEnv`/`<<-`; no `installed.packages()`; no `options(warn=-1)`; no `q()`/`exit()`; no `:::`/`.Internal()`; no `T`/`F`. |
| Internet | fails gracefully with a `message()`/`warning()` (not an ERROR under check); HTTPS; SSL not bypassed; minimal calls, no 429/403. |
| Deps | strong deps CRAN/Bioc only, none orphaned/archived; Suggests used conditionally; nothing from GitHub without `Additional_repositories`. |
| Files | tarball ≤ 10 MB, data+docs ≤ 5 MB each (NOTE > 5 MB installed); no `.o`/binaries; `.Rbuildignore` covers dev files (`cran-comments.md`, `CRAN-SUBMISSION`, `.claude`, `CLAUDE.md`, workflows, pkgdown). |
| Platforms | check clean (0 E / 0 W / no "significant" NOTEs) on R-devel Windows (win-builder) **and** Linux (R-hub `linux` at minimum, plus `macos`, `atlas`/`mkl` for BLAS-touching code, `gcc-asan`/`valgrind` for compiled code); macbuilder for arm64 if any doubt. |
| Cadence | ≥ 48 h since last publication, ≤ 1 release per 1–2 months once established; CRAN check page consulted; revdep maintainers notified ≥ 2 weeks before an API break. |
| Comments | `cran-comments.md`: check environments, `0 errors \| 0 warnings \| N notes` with each NOTE explained, revdep results, internet/size/special-treatment notes, and a `## Resubmission` section listing every reviewer point if this is one. |

4. Output: findings table with severity (`will be rejected` / `reviewer will
   ask` / `cosmetic`) and one-line fix each.

## Submission procedure (usethis order)

```r
usethis::use_version("major|minor|patch")   # bumps DESCRIPTION + NEWS heading
devtools::build_readme()
devtools::check(cran = TRUE, remote = TRUE, manual = TRUE)
devtools::check_win_devel(); devtools::check_win_release()   # results by email
rhub::rhub_check(platforms = c("linux", "windows", "macos"))  # GH Actions; add atlas/valgrind if compiled
# revdepcheck::revdep_check(num_workers = 4)  if revdeps exist
# update cran-comments.md with THIS run's results
devtools::submit_cran()      # builds tarball, uploads, writes CRAN-SUBMISSION
# → click the confirmation link in the maintainer email; do nothing else while pending
```

Manual alternative: `R CMD build .` then upload at
<https://cran.r-project.org/submit.html> with `cran-comments.md` pasted as
the optional comment.

On a reviewer reply: fix in `R/` (never in `man/`), `devtools::document()`,
bump patch, add `## Resubmission` to `cran-comments.md` quoting each point
and the change, re-run the checks, resubmit. Reply to the email only if the
reviewer asked a question; CC `cran-submissions@r-project.org`.

After "on CRAN" email: `usethis::use_github_release()` (reads
`CRAN-SUBMISSION`), `usethis::use_dev_version(push = TRUE)`, then watch
<https://cran.r-project.org/web/checks/check_results_PKG.html> for a week
(new flavours appear over several days; an ERROR there → fix within the
deadline CRAN emails or the package is archived).

## Failure playbook

| Symptom | Cause / fix |
|---|---|
| "Please add \value to .Rd files" | missing `@return`; `checkhelper::find_missing_tags()` |
| "Please replace \dontrun with \donttest" / "unwrap the examples" | examples are executable; use `\donttest` only for > 5 s or network |
| "Examples with CPU or elapsed time > 5s" | shrink `n`/`nboot`/`nrep` in the example; move the long form to `\donttest` |
| "Possibly misspelled words in DESCRIPTION" NOTE | add to `inst/WORDLIST` if a name, else single-quote software names |
| "Non-standard file/directory found at top level" | add to `.Rbuildignore` |
| "no visible binding for global variable" | NSE columns → `utils::globalVariables()` or `.data$col` |
| "Package has a VignetteBuilder field but no prebuilt vignette index" | `devtools::build_vignettes()` before build, or check from tarball |
| "connections left open" / "detritus in the temp directory" | stop clusters / `unlink()` in examples & tests (`\dontshow{}` for cleanup) |
| "Found the following (possibly) invalid URLs" | `urlchecker::url_update()`; use final HTTPS destination |
| "checking CRAN incoming feasibility ... Days since last update: N" | too soon; wait unless it's a fix CRAN asked for |
| "Version contains large components" | dev version `x.y.z.9000` left in DESCRIPTION |
| "Maintainer field differs from that derived from Authors@R" | delete hand-written `Maintainer:`/`Author:` |
| "compiled code ... calls to: abort/exit/printf" | replace with `Rf_error()`/`Rprintf()`; `-Wall` warnings must be clean |
| "Additional issues: ATLAS / MKL / valgrind / gcc-ASAN" | numeric tolerance or memory bug; reproduce on R-hub `atlas`/`valgrind` |
| "Overall checktime > 10 min" | cut test iterations, precompute vignette results, `skip_on_cran()` for slow tests |
