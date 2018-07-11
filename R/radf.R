#' Recursive Augmented Dickey-Fuller test
#'
#' \code{radf} returns the t-statistics from a recursive Augmented Dickey-Fuller
#' test.
#'
#' @param x A univariate or multivariate numeric ts object, data.frame or matrix.
#' The estimation process cannot handle NA values.
#' @param minw A positive integer. The minimum window size, which defaults to
#' \eqn{(0.01 + 1.8/\sqrt(T))*T}{(0.01 + 1.8 / \sqrtT)*T}.
#' @param lag A non-negative integer. The lag of the Augmented Dickey-Fuller regression.
#'
#' @return A list that contains the t-statistic (sequence) for:
#'   \item{ADF}{Augmented Dickey-Fuller.}
#'   \item{BADF}{Backward Augmented Dickey-Fuller.}
#'   \item{SADF}{Supremum Augmented Dickey-Fuller.}
#'   \item{BSADF}{Backward Supremum Augmented Dickey-Fuller.}
#'   \item{GSADF}{Generalized Supremum Augmented Dickey Fuller.}
#'
#' @references Phillips, P. C. B., Wu, Y., & Yu, J. (2011). Explosive Behavior
#' in The 1990s Nasdaq: When Did Exuberance Escalate Asset Values? International
#' Economic Review, 52(1), 201-226.
#'
#' @references Phillips, P. C. B., Shi, S., & Yu, J. (2015). Testing for
#' Multiple Bubbles: Historical Episodes of Exuberance and Collapse in the
#' S&P 500. International Economic Review, 56(4), 1043-1078.
#'
#' @importFrom stats embed
#' @importFrom lubridate date_decimal round_date
#' @export
#'
#' @examples
#' \donttest{
#' # Simulate bubble processes
#' dta <- cbind(sim_dgp1(n = 100), sim_dgp2(n = 100))
#'
#' rfd <- radf(x = dta)
#'
#' # For lag = 1 and minimum window = 20
#' rfd <- radf(x = dta, minw = 20, lag = 1)
#' }
radf <- function(x, minw, lag = 0) {

  if (any(class(x) %in% c("mts", "ts"))) {
    if (all(time(x) == seq(1, NROW(x), 1))) {
      dating <- time(x)
    }else{
      dating <- time(x) %>%
        as.numeric() %>%
        date_decimal() %>%
        round_date("month") %>%
        as.Date()
    }
  } else if (is.data.frame(x)) {
    if (class(x[, 1]) == "Date") {
      dating <- x[, 1]
      x <- x[, -1, drop = FALSE]
    } else if (all(findDates(rownames(x)))) {
      dating <- rownames(x) %>%
        as.Date()
    } else {
      dating <- seq(1, NROW(x))
    }
  } else if (class(x) %in% c("numeric", "matrix")) {
    dating <- index(x)
  } else {
    stop("Unsupported class", call. = FALSE)
  }

  if (anyNA(x)) {
    stop("Recursive least square estimation cannot handle NA", call. = FALSE)
  }

  x <- as.matrix(x)
  nc <- NCOL(x)
  nr <- NROW(x)

  if (is.null(colnames(x))) {
    colnames(x) <- paste("Series", seq(1, nc, 1))
  }

  if (missing(minw)) {
    r0 <- 0.01 + 1.8 / sqrt(nr)
    minw <- floor(r0 * nr)
  } else if (minw > 0 & minw < 3) {
    stop("Argument 'minw' is too small", call. = FALSE)
  }
  is.positive.int(minw)
  is.nonnegeative.int(lag)

  adf <- drop(matrix(0, 1, nc, dimnames = list(NULL, colnames(x))))
  badf <- matrix(0, nr - 1 - lag, nc, dimnames = list(NULL, colnames(x)))
  sadf <- drop(matrix(0, 1, nc, dimnames = list(NULL, colnames(x))))
  gsadf <- drop(matrix(0, 1, nc, dimnames = list(NULL, colnames(x))))
  bsadf <- matrix(0, nr - 1 - lag, nc, dimnames = list(NULL, colnames(x)))

  for (i in 1:nc) {

    yxmat <- unroot(x[, i], lag = lag)

    results <- rls_gsadf_cpp(yxmat, minw)

    adf[i] <- results$adf
    badf[, i] <- results$badf
    sadf[i] <- results$sadf
    gsadf[i] <- results$gsadf
    bsadf[, i] <- results$bsadf
  }

  bsadf_panel <- apply(bsadf, 1, mean)[-c(1:minw)]
  gsadf_panel <- max(bsadf_panel)

  value <- structure(list(adf = adf,
                          badf = badf[-c(1:(minw)), , drop = F],
                          sadf = sadf,
                          bsadf = bsadf[-c(1:(minw)), , drop = F],
                          gsadf = gsadf,
                          bsadf_panel = bsadf_panel,
                          gsadf_panel = gsadf_panel),
                     index = dating,
                     lag = lag,
                     minw = minw,
                     lag = lag,
                     col_names = colnames(x),
                     class = "radf")


  return(value)
}
