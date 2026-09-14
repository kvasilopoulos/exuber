# exuber CRAN release checklist

Consolidated from `usethis::use_release_issue()` (usethis 3.2.1,
`release_checklist("2.0.0", on_cran = TRUE)`), the *R Packages* release
chapter (<https://r-pkgs.org/release.html>), the CRAN Repository Policy
(<https://cran.r-project.org/web/packages/policies.html>), Writing R
Extensions, and this repo's own `CLAUDE.md` "Release process (CRAN)".
Dev-only file: listed in `.Rbuildignore`, not shipped.

Status column: `[x]` done and verified this release, `[ ]` pending,
`[-]` not applicable, `[!]` needs a human decision.

## 1. Prepare (usethis / r-pkgs.org)

| # | Item | Status | Notes (2.0.0, 2026-09-14) |
|---|------|--------|---------------------------|
| 1.1 | `git pull` — tree clean, in sync with `origin/master` | [x] | |
| 1.2 | Check [current CRAN check results](https://cran.r-project.org/web/checks/check_results_exuber.html) for the released version | [x] | 1.1.0: OK on all 12 flavours, nothing to fix retroactively |
| 1.3 | Advance deprecations (`lifecycle` "communicate" article) | [x] | `radf_wb_cv2()`/`radf_wb_distr2()` newly `.Deprecated()`; `col_names()`/`mc_cv()`/`wb_cv()`/`sb_cv()` stay as-is (no removal this cycle) |
| 1.4 | Polish `NEWS.md` (tidyverse style: user-facing, grouped, top heading = release version) | [x] | heading retitled `# exuber 2.0.0`; dev-cycle dates dropped from section titles |
| 1.5 | `urlchecker::url_check()` | [x] | one moved Springer URL fixed in `vignettes/references.bib`; now "All URLs are correct" |
| 1.6 | `devtools::build_readme()` | [x] | |
| 1.7 | `spelling::spell_check_package()` | [x] | en-GB spellings fixed (`favour`, `recognise`, `reorganised`); identifiers added to `inst/WORDLIST` |
| 1.8 | `devtools::check(remote = TRUE, manual = TRUE)` locally | [x] | see §4; `manual = FALSE` locally (no LaTeX here) — PDF manual built on win-builder/CI |
| 1.9 | `devtools::check_win_devel()` + `check_win_release()` | [x] | f8088a2: R-devel (2026-09-13 r90534) `Status: OK`, R 4.6.1 `Status: OK`, 0 NOTEs (win-builder mails 2026-09-14 07:54/08:00 UTC, logs 403y43561UTg / p0cS6xhPvG61). The two earlier `1 ERROR` mails are the d17d275 upload (`rootstamp` example, pre parallel fix) — ignore |
| 1.10 | R-hub CRAN platforms (`.github/workflows/rhub.yaml`, manual dispatch) | [x] | run 34820330290 on 2ff80f1: linux / windows / macos R-devel all `Status: OK` (0 NOTEs). Workflow had a non-existent `run-check@v2` ref and a hand-added `R -e` step PowerShell can't parse — both removed |
| 1.11 | Reverse dependencies: `revdepcheck::revdep_check()` | [-] | no reverse dependencies on CRAN (`tools::package_dependencies(reverse = TRUE)`) |
| 1.12 | Update `cran-comments.md` with *this* run's environments and NOTEs | [x] | stale `doSNOW`/`exuberdata` notes removed |
| 1.13 | `git push`, CI green (R-CMD-check / test-coverage / pkgdown / html-5-check) | [x] | all green on 2ff80f1 and 4330594 (R-CMD-check: macOS, Windows, Ubuntu devel/release/oldrel-1); html-5-check had failed on every run since it was added (no deps installed → `LinkingTo` NOTE, no V8 → math-rendering NOTE, job errors on NOTEs); fixed |
| 1.14 | Draft blog post | [-] | not part of this package's release practice |

## 2. Submit

| # | Item | Status | Notes |
|---|------|--------|-------|
| 2.1 | `usethis::use_version('major')` — `1.1.0` → `2.0.0` | [x] | major: bundled `radf_crit` dataset removed, default `cv` now fetched from the store (needs network first use), `R >= 4.0` |
| 2.2 | `devtools::submit_cran()` (or upload at <https://cran.r-project.org/submit.html> with `cran-comments.md`) | [!] | **not run — submission is the maintainer's call** |
| 2.3 | Approve the confirmation email | [ ] | |
| 2.4 | `CRAN-SUBMISSION` file is created by `submit_cran()`; already in `.Rbuildignore` | [x] | |

## 3. After acceptance

| # | Item | Status | Notes |
|---|------|--------|-------|
| 3.1 | `usethis::use_github_release()` (tag `v2.0.0`, notes from NEWS) | [ ] | |
| 3.2 | `usethis::use_dev_version(push = TRUE)` → `2.0.0.9000`, NEWS heading back to `# exuber (development version)` | [ ] | |
| 3.3 | Update `website/` critical-values/suite pages if they quote the CRAN version | [ ] | |

## 4. R CMD check gates (CRAN policy: no ERRORs/WARNINGs, no "significant" NOTEs)

Local: `devtools::check(cran = TRUE, remote = TRUE, manual = FALSE, env_vars = c(NOT_CRAN = "false"))`, R 4.6.1, Windows 11, Rtools45.

| # | Item | Status | Notes |
|---|------|--------|-------|
| 4.1 | 0 ERRORs, 0 WARNINGs | [x] | |
| 4.2 | NOTEs explained in `cran-comments.md` | [x] | |
| 4.3 | Examples each "no more than a few seconds"; none flagged > 5s | [x] | first run flagged `rootstamp` (6.5s) and `scale_exuber_manual` (5.6s) at ~1.5s CPU: every `*_cv()` call was starting and stopping two `multisession` workers (~4s). Fixed at the root: one reused cluster per session, and `exuber.parallel` defaults to `interactive()` — a session-long cluster trips `R CMD check`'s "connections left open" in examples |
| 4.4 | `--run-donttest` examples pass (CRAN incoming runs them) | [x] | |
| 4.5 | Tests pass with `NOT_CRAN=false` (what CRAN runs) and `NOT_CRAN=true` (CI) | [x] | |
| 4.6 | No stray object files in the source tarball (`src/vendor/**/*.o`) | [x] | `clean:` target added to `src/Makevars{,.win}` — `R CMD build` only sweeps top-level `src/*.o` |
| 4.7 | No non-standard top-level files/hidden dirs in the tarball | [x] | `.claude`, `CLAUDE.md`, `CITATION.cff`, `Rplots.pdf`, this file → `.Rbuildignore` |
| 4.8 | "no visible binding for global variable" NOTE | [x] | NSE column names registered in `R/zzz.R` `globalVariables()` |
| 4.9 | Unstated test dependency (`pkgload`) | [x] | added to `Suggests` |

## 5. CRAN Repository Policy items (package-specific audit)

| # | Policy | Status | Notes |
|---|--------|--------|-------|
| 5.1 | Maintainer is a single person with a working email | [x] | `Authors@R`, `cre` = Kostas Vasilopoulos |
| 5.2 | `Authors@R` roles: `cph` for copyright holders | [!] | no `cph` role declared (same as every prior CRAN release; CRAN has accepted it). Optional: add `role = c("cre", "aut", "cph")` |
| 5.3 | License permits CRAN distribution (`GPL-3`) | [x] | |
| 5.4 | DOIs in `Description` as `<doi:...>`, references in Title Case, no "package" in Title | [x] | |
| 5.5 | Strong deps only from CRAN; Suggests used conditionally | [x] | `requireNamespace()` guards in place; `exuberdata`/`Additional_repositories` gone |
| 5.6 | No Depends/Imports on archived packages | [x] | all Imports on CRAN as of 2026-09-14 |
| 5.7 | Source tarball ≤ 10 MB; data + docs ≤ 5 MB | [x] | `exuber_2.0.0.tar.gz` is 0.56 MB |
| 5.8 | ≤ 2 threads/cores by default in examples/tests | [x] | `exuber.parallel` defaults to `interactive()` → serial under check; `exuber.ncores` capped at 2 non-interactively and by `MC_CORES` |
| 5.9 | Writes only to `tempdir()` or `tools::R_user_dir()`; **`R_user_dir` use requires `R (>= 4.0)`** | [x] | crit cache at `R_user_dir("exuber", "cache")` (one ~2–20 KB `.bin.xz` per `(n, lag)` used); `Depends: R (>= 4.0)` raised from 3.2 |
| 5.10 | Cache contents "actively managed", size kept small | [x] | a corrupt file is refetched; `crit_cache_dir()` documents the location |
| 5.11 | Internet resources fail gracefully with an informative message, no check WARNING/ERROR | [!] | see §6 — the default-`cv` fetch is the one open CRAN risk |
| 5.12 | HTTPS for downloads, adequate timeout | [x] | `https://exuber.up.railway.app/crit2/<lag>/<n>`; `download.file()` default timeout |
| 5.13 | No `.Internal()`, `:::` on other packages, `q()`/`exit()` in compiled code | [x] | `rg` audit clean |
| 5.14 | No global environment / options modification without restore | [x] | `.onLoad` only sets unset `exuber.*` options |
| 5.15 | No viewers/browsers launched from examples/tests | [x] | |
| 5.16 | Runs on ≥ 2 major platforms | [x] | CI: macOS, Windows, Ubuntu (release/devel/oldrel) |
| 5.17 | Submission cadence: no more than every 1–2 months; wait 48 h after publication before resubmitting | [x] | 1.1.0 is long-published |
| 5.18 | Breaking API change: notify affected maintainers 2 weeks ahead | [-] | no reverse dependencies |

## 6. Open decision: examples/tests that fetch critical values

`summary()`/`datestamp()`/`autoplot()` on a `radf_obj` with no explicit
`cv` fetch one table from the store on first use. Examples, vignettes and
tests exercise that path, so on a check machine without network access
they would ERROR (an informative `stop()`, but still an ERROR — the policy
asks for a graceful *message*). The check in §4 records which `(n, lag)`
tables were actually pulled: exactly one, `lag0-n100.bin.xz` (1.3 KB). The
test suite itself passes `cv` everywhere and fetches nothing.

Options, cheapest first:

1. **Accept and disclose** in `cran-comments.md` (the check farm has
   network; many API-client packages ship this way). Zero code change.
2. **Ship the handful of tables the checks need as `inst/extdata/crit/`**
   and have `fetch_crit_bucket()` look there before the network. Keeps
   the "no bundled `radf_crit` object" decision, makes every example,
   vignette and test offline-safe. ~3 lines in `R/crit-bucket.R` plus one
   1.3 KB file.
3. **Guard** every network-dependent example/test with
   `\donttest{}`/`skip_if_offline()` — largest diff, and `--as-cran` runs
   `\donttest` anyway, so it only helps tests.
