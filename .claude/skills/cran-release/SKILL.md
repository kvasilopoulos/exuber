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

## Who does what

Claude runs the release end to end **except three human gates**. Do
everything else without asking; at a gate, hand over the exact artifact or
command, mark the checklist row `[!]`, and wait.

| Human gate | What Claude hands over |
|---|---|
| **1. DESCRIPTION check** — `Title:` and `Description:` wording (Title Case, quotes, references, acronyms) | proposed text as a diff; commit only after approval. Every other DESCRIPTION field (Version, deps, Authors@R roles, License) Claude edits directly |
| **2. Release comments** — `cran-comments.md` (and a `## Resubmission` section if a reviewer replied) | a complete draft filled with *this* run's environments, NOTEs and revdep results; the human edits/approves before the tarball is built |
| **3. Final submission** — `devtools::submit_cran()` (or the web form) and clicking the CRAN confirmation link; also any reply email to a CRAN reviewer | the built tarball path, the approved `cran-comments.md`, and — for a reviewer reply — a drafted email body. Never call `submit_cran()`, never send mail |

Claude owns: `usethis::use_version()`, NEWS heading, `devtools::document()`,
all checks (local `check(cran = TRUE, …)`, `check_win_devel()` /
`check_win_release()` uploads, `gh workflow run rhub.yaml`, revdeps),
fixing what they find in `R/`/`src/`/`tests/`, `urlchecker`, `spelling`,
`.Rbuildignore`, `R CMD build`, watching results (below), and after
acceptance `usethis::use_github_release()`, `use_dev_version(push = TRUE)`,
and the website/CHANGELOG follow-ups the repo checklist lists.

**Monitoring by email.** win-builder, macbuilder and CRAN only report by
mail, to the `cre` address in `DESCRIPTION`; the claude.ai Gmail
connector is on that account. Use `mcp__claude_ai_Gmail__search_threads`
(`query`, Gmail syntax), then `mcp__claude_ai_Gmail__get_thread` with
`messageFormat: PLAIN_TEXT`. Before trusting a hit, check its
`toRecipients` is the `cre` address — if it isn't, the connector is on a
different account: say so and ask the user to paste the mail.
Sender/subject shapes as actually received:

| Stage | From | Subject |
|---|---|---|
| win-builder result (one mail per R version, ~15–20 min after upload) | `ligges@statistik.tu-dortmund.de` | `winbuilder: Package PKG_X.Y.Z.tar.gz has been checked and built` — body has `Status: OK` / `Status: 1 ERROR`, the R version, and a results URL; fetch `<url>/00check.log` with curl/WebFetch. Several uploads share one thread — match by R version and time, not by thread |
| submission confirmation link | `cransubmit@r-project.org` | `CRAN Submission of PKG X.Y.Z - Confirmation Link` (human clicks it — gate 3) |
| submission receipt | `cransubmit@xmbombadil.wu.ac.at` (to cran-submissions, maintainer CC'd) | `CRAN Submission PKG X.Y.Z` |
| pretest verdict | `ligges@statistik.tu-dortmund.de` | `[CRAN-pretest-publish] CRAN Submission PKG X.Y.Z` ("on its way to CRAN") or `[CRAN-pretest-archived] …` (rejected, log in body) or `[CRAN-pretest-inspect] …` (human review) |
| reviewer comments / on CRAN | a CRAN team member / `CRAN-submissions@R-project.org` | `CRAN Submission PKG X.Y.Z` thread / `CRAN package PKG X.Y.Z published` |
| post-publication problems | `CRAN@R-project.org` / a CRAN team member | `CRAN package PKG` … "check problems", "will be archived", "please correct before <date>" |

```
in:anywhere PKG (winbuilder OR "win-builder" OR from:statistik.tu-dortmund.de) newer_than:2d
PKG (from:cransubmit OR from:r-project.org OR subject:"CRAN Submission" OR subject:pretest) newer_than:14d
PKG (from:r-project.org OR subject:"CRAN package") ("check problems" OR archived OR "please correct") newer_than:30d
```

Cadence: win-builder every 15 min until the mail lands (two mails if both
devel and release were uploaded); CRAN pretest every 30 min for the first
4 h, then every few hours; human review can take days — check daily and
tell the user when a reviewer reply arrives (drafting the fix and the
`## Resubmission` section is Claude's job, the reply itself is gate 3).
Confirm a hit's recipient matches `Authors@R`'s `cre` email before acting
on it. Never mark a check "passed" from memory of an upload — only from
the mail or the results page.

R-hub v2 results are GitHub Actions runs, not mail: `gh run list
--workflow=rhub.yaml`, `gh run view <id> --log-failed`.

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
# Claude
usethis::use_version("major|minor|patch")   # bumps DESCRIPTION + NEWS heading
devtools::build_readme()
devtools::check(cran = TRUE, remote = TRUE, manual = TRUE)
devtools::check_win_devel(); devtools::check_win_release()   # results by email → monitor
# gh workflow run rhub.yaml (or rhub::rhub_check(platforms = c("linux","windows","macos")); add atlas/valgrind if compiled)
# revdepcheck::revdep_check(num_workers = 4)  if revdeps exist
# draft cran-comments.md from THIS run's results            → gate 2
# gate 1 (DESCRIPTION wording) must be approved before the next line
devtools::build()                            # tarball in ../PKG_X.Y.Z.tar.gz
# Human
devtools::submit_cran()      # uploads, writes CRAN-SUBMISSION           → gate 3
# → click the confirmation link in the maintainer email; nothing else while pending
```

Web-form alternative for the human: upload the tarball at
<https://cran.r-project.org/submit.html> with `cran-comments.md` pasted as
the optional comment.

On a reviewer reply (found by the email monitor): Claude fixes in `R/`
(never in `man/`), `devtools::document()`, bumps patch, adds
`## Resubmission` to `cran-comments.md` quoting each point and the change,
re-runs the checks, rebuilds → gates 2 and 3 again. If the reviewer asked
a question, Claude drafts the reply (CC `cran-submissions@r-project.org`);
the human sends it.

After the "on CRAN" mail: Claude runs `usethis::use_github_release()`
(reads `CRAN-SUBMISSION`), `usethis::use_dev_version(push = TRUE)`, then
watches <https://cran.r-project.org/web/checks/check_results_PKG.html>
(WebFetch) daily for a week — new flavours appear over several days; an
ERROR there, or a "please correct" mail, → fix within the stated deadline
or the package is archived.

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
