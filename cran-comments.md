## Release summary

exuber 2.0.0 is a major release. It adds new tests, dating procedures and monitoring procedures. It also replaces the bundled `radf_crit` critical-value dataset with a precomputed store that is fetched on first use and cached with `tools::R_user_dir()`, which is why the package now has `Depends: R (>= 4.0)`. `radf_wb_cv2()` and `radf_wb_distr2()` are deprecated and keep forwarding aliases. NEWS.md has the full details.

## Test environments

* local Windows 11, R 4.6.1 (Rtools45), using `devtools::check(cran = TRUE, remote = TRUE)`
* GitHub Actions: macOS (release), Windows (release), Ubuntu (devel, release, oldrel-1)
* win-builder (R-devel, R-release)
* R-hub: linux, windows, macos

## R CMD check results

0 errors | 0 warnings | 0 notes

The local Windows run also reports "non-standard things in the check directory: 'NULL'". This is an empty directory that the check process itself creates on this machine, and it does not appear on GitHub Actions or win-builder.

## Internet access

The default critical values are downloaded over HTTPS from the package's own store, as one small file for each sample size and lag. They are cached in `tools::R_user_dir("exuber", "cache")`, and a corrupt or missing cache entry is fetched again. When the store cannot be reached, the functions stop with a message that points to the offline alternatives (`radf_mc_cv()` and `radf_wb_cv()`). Examples and tests use small sample sizes, so each fetch is a few KB.

## Reverse dependencies

There are no reverse dependencies.
