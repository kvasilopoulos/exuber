# Helpers -----------------------------------------------------------------

#' Helper functions in accordance to PSY(2015)
#'
#' \code{psy_minw} and \code{psy_ds} use the rules-of- thumb proposed by
#' Phillips et al. (reference) to compute the minimum window size and the
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

  floor( (0.01 + 1.8 / sqrt(n)) * n)
}

#' @rdname psy_minw
#' @param rule Rule 1 corresponds to log(T), while rule 2 log(T)/T
#' @param delta Frequency-dependent parameter. See details.
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

lag_selection <- function(data, criterion = c("aic", "bic"), max_lag = 8) {
  criterion <- match.arg(criterion)
  tbl <- lag_selection_table(data, max_lag)
  criterion_vec <- unname(tbl[,criterion])
  min_idx <- which.min(criterion_vec)
  as.integer(criterion_vec[min_idx])
}

lag_selection_table <- function(data, max_lag = 8) {
  x <- parse_data(data)
  nr <- nrow(x)
  nc <- ncol(x)
  snames <- series_names(x)
  dof <- nr - max_lag - 2
  min_criterion <- matrix(NA, nrow = nc, ncol = 2)
  aic <- bic <- vector("numeric", max_lag + 1)
  for (i in 1:nc) {
    for (l in 0:max_lag) {
      reg <- unroot(x, lag = l)
      matx <- reg[,-1]
      maty <- reg[, 1]
      beta <- qr.solve(crossprod(matx)) %*% crossprod(matx, maty)
      res <- maty - matx %*% beta
      npdf <- sum(-1/2 * log(2 * pi) - 1/2 * (res^2))
      aic[l + 1] <- -2 * npdf/nr + 2 * nrow(beta)/nr
      bic[l + 1] <- -2 * npdf/nr + nrow(beta) * log(nr)/nr
    }
    min_criterion[i, 1] <- which.min(aic) - 1
    min_criterion[i, 2] <- which.min(aic) - 1
  }
  dimnames(min_criterion) <- list(snames, c("aic", "bic"))
  min_criterion
}

