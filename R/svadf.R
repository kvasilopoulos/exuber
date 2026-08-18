# Sarkar, A. & Wells, M.T. (2026, arXiv:2604.12062, "Is There an AI
# Bubble? Robust Date-Stamping for Periods of Exuberance"; "SW"). See
# docs/enhancements/volatility-robustness.md, "SV-ADF", for the full
# evaluation this implements.
#
# A non-peer-reviewed preprint (flagged explicitly, a different bar than
# every other source implemented in this project). The point statistic
# is radf()'s own `badf` sequence (proof appendix eq. A.13-A.14: the
# "feasible" variance estimator is literally the standard within-window
# OLS residual variance radf()'s own recursive ADF t-statistic already
# uses), compared against two closed-form, sample-size-only thresholds
# from the paper's own applied methodology (Section 5.1):
#
#   origination threshold: log(t)/10  (their own 1,000-rep-per-n
#     calibration exercise under H0, approximated by this formula and
#     adopted directly)
#   collapse threshold:    log(t)/2   (same exercise under H1 averaged
#     over randomly drawn nuisance parameters)
#
# where `t` is the current recursive window's own sample size. The
# genuinely new feature relative to PWY/PSY's own dating (`datestamp()`)
# is that origination and collapse compare against DIFFERENT thresholds
# (SW's own Remark 1) -- 2026-08-18: folded into `datestamp.radf_obj()`
# as `option = "svadf"` instead of shipping as a separate `radf_svadf()`
# entry point, reusing `stamp()`/`add_peak()`/`stamp_to_index()`/
# `add_ongoing()` the same way the `"gsadf"`/`"sadf"` options do (see
# `R/radf-methods.R`) rather than a bespoke print method.

svadf_caveat <- "Experimental. Sarkar & Wells (2026) is a non-peer-reviewed preprint; see ?datestamp, Caveats section."

svadf_threshold <- function(t, type = c("origination", "collapse")) {
  type <- match.arg(type)
  if (type == "origination") log(t) / 10 else log(t) / 2
}
