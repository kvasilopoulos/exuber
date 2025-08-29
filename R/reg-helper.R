# Helpers -----------------------------------------------------------------

#' Helper functions in accordance to PSY(2015)
#'
#' \code{psy_minw} and \code{psy_ds} use the rules-of- thumb proposed by
#' Phillips et al. (2015) to compute the minimum window size and the
#' minimum duration of an episode of exuberance, respectively.
#'
#' @inheritParams radf_mc_cv
#' @export
#' @importFrom rlang is_scalar_atomic
#' @examples
#' psy_minw(100)
#' psy_ds(100)
psy_minw <- function(n) {
  if (!is_n(n)) {
    n <- NROW(n)
  }
  floor((0.01 + 1.8 / sqrt(n)) * n)
}

#' @rdname psy_minw
#' @param rule Rule to compute the minimum duration of an episode (default: rule = 1,
#' where T denotes the sample size). Rule 1 corresponds to log(T), while rule 2 to log(T)/T.
#' @param delta Frequency-dependent parameter (default; delta = 1).
#'   See details.
#'
#' @details For the minimum duration period, \code{psy_ds} allows the user to choose from two rules:
#'
#' \deqn{rule_1 = \delta \log(n) \quad\& \quad rule_2 = \delta \log(n)/n}{rule_1 = d*log(n) & rule 2 = d*log(n)/n}
#'
#' \code{delta } depends on the frequency of the data and the minimal duration condition.
#'
#' @references Phillips, P. C. B., Shi, S., & Yu, J. (2015). Testing for
#' Multiple Bubbles: Historical Episodes of Exuberance and Collapse in the
#' S&P 500. International Economic Review, 56(4), 1043-1078.
#'
#' @export
psy_ds <- function(n, rule = 1, delta = 1) {
  if (!is_n(n)) {
    n <- NROW(n)
  }
  stopifnot(rule %in% c(1, 2))
  stopifnot(delta > 0)

  if (rule == 1) {
    round(delta * log(n))
  } else if (rule == 2) {
    round(delta * log(n) / n)
  }
}

#' Helper function to find `tb` from the Phillips and Shi (2020)
#'
#' This function helps to find the number of observations in the window
#' over which size is to be controlled.
#'
#' @inheritParams radf_mc_cv
#' @param freq The type of date-interval.
#' @param size The size to be controlled.
#'
#' @references Phillips, P. C., & Shi, S. (2020). Real time monitoring of
#' asset markets: Bubbles and crises. In Handbook of Statistics (Vol. 42, pp. 61-80). Elsevier.
#'
#' @references Shi, S., Hurn, S., Phillips, P.C.B., 2018. Causal change detection in possibly
#' integrated systems: Revisiting the money-income relationship.
#'
#' @export
ps_tb <- function(n, freq = c("monthly", "quarterly", "annual", "weekly"), size = 2) {
  if (!is_n(n)) n <- NROW(n)
  minw <- psy_minw(n)
  freq <- match.arg(freq)
  assert_positive_int(size)
  multi <- switch(freq,
    weekly = 52,
    monthly = 12,
    quarterly = 4,
    annual = 1
  )
  tb <- size * multi
  minw + tb - 1 # is it +1 or minus 1
}
