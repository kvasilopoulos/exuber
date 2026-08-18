# Queryable function registry -- added 2026-08-18 in response to feedback
# that naming conventions alone (radf_/_test/dating_/monitor_/root_, see
# vignette("naming-and-analysis")) are too easy to misremember or disagree
# about (monitor_radf() vs. radf_monitor() being exactly that argument).
# This is the actual, checkable source of truth: update this table whenever
# an exported test/procedure function is added, renamed, or reclassified --
# same as _pkgdown.yml/NEWS.md/CLAUDE.md/the vignette already need updating.
#
# `family` is a comma-separated string, not a list-column, so a function
# belonging to more than one family (monitor_radf() is both "adf" and
# "monitor") is one row, filtered with grepl() rather than needing a second
# join table for what is, so far, exactly one multi-family case.
exuber_registry <- function() {
  tibble::tribble(
    ~name, ~family, ~description,
    "radf", "adf", "The recursive ADF/SADF/GSADF/BSADF statistic (Phillips, Shi & Yu 2015).",
    "radf_mc_cv", "adf", "Monte Carlo critical values for radf().",
    "radf_wb_cv", "adf", "Wild bootstrap critical values for radf() (heteroskedasticity-robust).",
    "radf_wb_cv2", "adf", "Wild bootstrap critical values with a training-window boundary, used by monitor_radf().",
    "radf_sb_cv", "adf", "Panel sieve bootstrap critical values for radf().",
    "radf_tt", "adf", "Time-transformed test (STADF/GSTADF), bootstrap-free heteroskedasticity robustness.",
    "radf_tt_cv", "adf", "Pivotal asymptotic critical values for radf_tt().",
    "radf_sign", "adf", "Sign-based sPWY/sPSY test, exactly invariant to heteroskedasticity.",
    "radf_sign_cv", "adf", "Critical values for radf_sign().",
    "radf_sign_dm", "adf", "Recursively demeaned sign-based test, robust to deterministic level shifts too.",
    "radf_sign_dm_cv", "adf", "Critical values for radf_sign_dm().",
    "radf_common", "adf", "Common-bubble detection via PCA + radf() on the leading factor.",
    "radf_common_cv", "adf", "Panel-width-specific critical values for radf_common().",
    "radf_kp", "adf", "Kernel-purge heteroskedasticity-robust test; purges volatility, then plain radf().",
    "radf_recovery", "adf", "Reverse-regression crisis-origination/recovery dating.",
    "radf_recovery_cv", "adf", "Critical values for radf_recovery() (its own null, not radf_mc_cv()'s).",
    "monitor_radf", "adf,monitor", "Real-time monitoring (Family A); reuses radf()'s badf/bsadf directly.",
    "lbi_test", "test", "Locally best invariant test for a bubble spanning the whole sample (Breitung & Diegel 2025).",
    "ssu_test", "test", "Stochastic explosive-coefficient test on squared first differences (Kurozumi & Nishi 2025).",
    "quantile_test", "test", "Quantile-regression global test, an alternative to the mean-regression ADF family.",
    "cobubble_test", "test", "KPSS-type co-explosive test between two series (Evripidou, Harvey, Leybourne & Sollis 2022).",
    "radf_sbz_cv", "test", "SBZ WLS/kernel-volatility test with union-of-rejections; bundles statistic and critical value.",
    "dating_hls", "dating", "SSR/BIC single-bubble dating (Harvey, Leybourne & Sollis 2017); no critical value needed.",
    "dating_hlw", "dating", "SSR/BIC multi-bubble dating, wraps dating_hls() per detected episode.",
    "dating_knp", "dating", "Bias-corrected SSR dating (Kejriwal, Nguyen & Perron 2025).",
    "dating_pdc", "dating", "Sequential sample-splitting regime dating.",
    "monitor_cusum", "monitor", "CUSUM/CUSUMV real-time monitoring, closed-form boundary, no bootstrap.",
    "monitor_lbi", "monitor", "Sequential extension of lbi_test(), constant-boundary mCUSUM/wCUSUM.",
    "monitor_quantile", "monitor", "QPWY recursive quantile-regression monitoring, expanding window.",
    "rootstamp", "root", "Confidence interval + doubling time on the magnitude of the explosive root; default method fits a single sub-sample, radf_obj method runs every datestamp() episode.",
    "contagion_reg", "regression", "Bubble contagion regression (Greenaway-McGrevy & Phillips 2016); point estimation, no test."
  )
}

#' Look Up exuber's Test/Procedure Functions by Family
#'
#' Naming conventions (\code{radf_}/\code{_test}/\code{dating_}/
#' \code{monitor_}/\code{root_}, see \code{vignette("naming-and-analysis")})
#' are a guide, not a contract -- easy to misremember, and occasionally
#' traded off deliberately (\code{monitor_radf()} is ADF-family internally
#' but named for what it does). This is the actual, queryable source of
#' truth: which of the package's test/dating/monitoring/root-inference
#' functions belong to which family.
#'
#' @param family One of \code{"adf"} (built on the recursive-ADF core),
#' \code{"test"} (a standalone hypothesis test), \code{"dating"}
#' (point-estimation/model-selection, no formal test), \code{"monitor"}
#' (real-time/sequential), \code{"root"} (confidence-interval inference on
#' the explosive root), \code{"regression"} (point estimation, no test), or
#' \code{NULL} (default) for every function. A function can belong to more
#' than one family (\code{monitor_radf()} is both \code{"adf"} and
#' \code{"monitor"}).
#'
#' @return A tibble with columns \code{name}, \code{family}, and
#' \code{description}, one row per function.
#'
#' @seealso \code{vignette("naming-and-analysis", package = "exuber")} for
#' the full naming scheme and which functions plug into
#' \code{summary()}/\code{\link{datestamp}}/\code{tidy}/\code{autoplot}.
#'
#' @examples
#' exuber_functions()
#' exuber_functions(family = "monitor")
#' exuber_functions(family = "test")
#'
#' @export
exuber_functions <- function(family = NULL) {
  reg <- exuber_registry()
  if (is.null(family)) {
    return(reg)
  }
  valid <- c("adf", "test", "dating", "monitor", "root", "regression")
  if (!family %in% valid) {
    stop_glue("'family' must be one of {paste(valid, collapse = ', ')}, not '{family}'.")
  }
  reg[grepl(family, reg$family, fixed = TRUE), ]
}
