## Release summary

exuber 2.0.0. A major release: new tests, dating and monitoring
procedures, and the bundled `radf_crit` critical-value dataset is replaced
by a precomputed store fetched on first use and cached with
`tools::R_user_dir()` (hence `Depends: R (>= 4.0)`). `radf_wb_cv2()` and
`radf_wb_distr2()` are deprecated with forwarding aliases. Full details in
NEWS.md.

## Test environments

* local Windows 11, R 4.6.1 (Rtools45) -- `devtools::check(cran = TRUE, remote = TRUE)`
* GitHub Actions: macOS (release), Windows (release), Ubuntu (devel, release, oldrel-1)
* win-builder (R-devel)
* R-hub: linux, windows, macos

## R CMD check results

0 errors | 0 warnings | 0 notes

The local Windows run additionally reports "non-standard things in the
check directory: 'NULL'", an empty directory the check process itself
creates on this machine; it does not appear on GitHub Actions or
win-builder.

## Internet access

Default critical values are downloaded (HTTPS, one small file per sample
size / lag) from the package's own store and cached in
`tools::R_user_dir("exuber", "cache")`; a corrupt or missing cache entry is
re-fetched. When the store is unreachable the functions stop with an
informative message pointing to the offline alternatives
(`radf_mc_cv()`, `radf_wb_cv()`). Examples and tests keep sample sizes
small so each fetch is a few KB.

## Reverse dependencies

There are no reverse dependencies.
