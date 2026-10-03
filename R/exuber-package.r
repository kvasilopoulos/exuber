#' @section Package options:
#' \code{exuber.show_progress}
#' - Should lengthy operations such as \code{radf_mc_cv()} show a progress bar? Default: TRUE
#'
#' \code{exuber.parallel}
#' - Should lengthy operations use parallel computation? Default: TRUE in an
#'   interactive session and FALSE otherwise (scripts, knitr and R CMD check),
#'   because starting workers costs a few seconds. Set it to TRUE in a script to opt
#'   in. The worker cluster is started once per session and reused. The
#'   \code{radf_*_cv()} and \code{radf_*_distr()} simulation engines honor the
#'   option (\code{radf_mc_cv()}, \code{radf_wb_cv()}, \code{radf_sb_cv()},
#'   \code{radf_recovery_cv()} and \code{radf_common_cv()}). The standalone tests
#'   and monitors run serially regardless.
#'
#' \code{exuber.ncores}
#' - How many cores to use for parallel computation. Default: the number of system
#'   cores minus 1 (2 in a non-interactive session), capped by the \code{MC_CORES}
#'   environment variable when it is set.
#'
#' \code{exuber.global_seed}
#' - When set, the seed feeds automatically into all functions that generate random
#'   numbers. Default: NA
#'
#' @name exuber
#' @useDynLib exuber, .registration = TRUE
#' @importFrom Rcpp evalCpp
#' @importFrom lifecycle badge
#' @importFrom stats setNames approx qcauchy lm.fit
#' @docType package
#' @keywords internal
"_PACKAGE"
