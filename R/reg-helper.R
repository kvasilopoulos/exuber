# Helpers -----------------------------------------------------------------

#' Helper functions in accordance to PSY(2015)
#'
#' \code{psy_minw} proposes a minimum window and \code{psy_ds} proposes a rule of
#' thumb to exclude periods of exuberance.
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
#' @param delta Frequency-dependent parameter
#'
#' @details \code{delta } depends on the frequency of the data and the minimal
#' duration condition. For example, for a 30-year period, we set arbitrarily duration
#' to exceed periods such as one year. Then, delta should is 0.7 for yearly data
#' and 5 for monthly data.
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

# TODO export lag_selection

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

