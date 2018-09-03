#' Recursive Augmented Dickey-Fuller test
#'
#' \code{radf} returns the t-statistics from a recursive Augmented Dickey-Fuller
#' test.
#'
#' @param data A univariate or multivariate numeric ts object, data.frame or matrix.
#' The estimation process cannot handle NA values.
#' @param minw A positive integer. The minimum window size, which defaults to
#' \eqn{(0.01 + 1.8/\sqrt(T))*T}{(0.01 + 1.8 / \sqrtT)*T}.
#' @param lag A non-negative integer. The lag of the Augmented Dickey-Fuller regression.
#'
#' @return A list that contains the t-statistic (sequence) for:
#'   \item{adf}{Augmented Dickey-Fuller}
#'   \item{badf}{Backward Augmented Dickey-Fuller}
#'   \item{sadf}{Supremum Augmented Dickey-Fuller}
#'   \item{bsadf}{Backward Supremum Augmented Dickey-Fuller}
#'   \item{gsadf}{Generalized Supremum Augmented Dickey Fuller}
#'
#' @references Phillips, P. C. B., Wu, Y., & Yu, J. (2011). Explosive Behavior
#' in The 1990s Nasdaq: When Did Exuberance Escalate Asset Values? International
#' Economic Review, 52(1), 201-226.
#'
#' @references Phillips, P. C. B., Shi, S., & Yu, J. (2015). Testing for
#' Multiple Bubbles: Historical Episodes of Exuberance and Collapse in the
#' S&P 500. International Economic Review, 56(4), 1043-1078.
#'
<<<<<<< HEAD
#' @importFrom stats embed frequency
#' @importFrom lubridate date_decimal round_date
=======
#' @importFrom stats embed frequency time
#' @importFrom lubridate date_decimal round_date
#' @importFrom purrr detect_index
>>>>>>> pipe friendly version: confrom better with ggplot and S3 methods
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
radf <- function(data, minw, lag = 0) {

<<<<<<< HEAD
  if (any(class(x) %in% c("mts", "ts"))) {
    if (all(time(x) == seq(1, NROW(x), 1))) {
      dating <- time(x)
    }else{
      dating <- time(x) %>%
          as.numeric() %>%
          date_decimal()
      if (frequency(x) %in% c(1, 4, 12)) {
        dating <- dating %>%
          round_date("month") %>%
          as.Date()
      }else if (frequency(x) == 52) {
=======
  # class checks
  sim_index <- seq(1, NROW(data), 1)
  if (any(class(data) %in% c("mts", "ts"))) {

    if (all(time(data) == sim_index)) {
      dating <- sim_index
    }else{
      dating <- time(data) %>%
        as.numeric() %>%
        date_decimal()
      if (frequency(data) %in% c(1, 4, 12)) {
        dating <- dating %>%
          round_date("month") %>%
          as.Date()
      }else if (frequency(data) == 52) {
>>>>>>> pipe friendly version: confrom better with ggplot and S3 methods
        dating <- dating %>%
          as.Date()
      }else{
        dating <- dating %>%
          round_date("day") %>%
          as.Date()
      }
    }
<<<<<<< HEAD
  } else if (is.data.frame(x)) { #need to identify date somehow and date format
    if (class(x[, 1]) == "Date") {
      dating <- x[, 1]
      x <- x[, -1, drop = FALSE]
    } else if (all(findDates(rownames(x)))) {
      dating <- rownames(x) %>%
        as.Date()
=======
  } else if (is.data.frame(data)) {
    date_index <- purrr::detect_index(data, lubridate::is.Date)
    if (as.logical(date_index)) {
      dating <- data[, date_index]
      data <- data[, -date_index, drop = FALSE]
>>>>>>> pipe friendly version: confrom better with ggplot and S3 methods
    } else {
      dating <- sim_index
    }
  } else if (class(data) %in% c("numeric", "matrix")) {
    dating <- sim_index
  } else {
    stop("Unsupported class", call. = FALSE)
  }

<<<<<<< HEAD
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
  assert_positive_int(minw)
  assert_nonnegeative_int(lag)

  adf <- drop(matrix(0, 1, nc, dimnames = list(NULL, colnames(x))))
  badf <- matrix(0, nr - 1 - lag, nc, dimnames = list(NULL, colnames(x)))
  sadf <- drop(matrix(0, 1, nc, dimnames = list(NULL, colnames(x))))
  gsadf <- drop(matrix(0, 1, nc, dimnames = list(NULL, colnames(x))))
  bsadf <- matrix(0, nr - 1 - lag, nc, dimnames = list(NULL, colnames(x)))

  for (i in 1:nc) {

    yxmat <- unroot(x[, i], lag = lag)

    results <- rls_gsadf_cpp(yxmat, minw)
=======

  x <- as.matrix(data)
  nc <- NCOL(data)
  nr <- NROW(data)
  # args
  if (is.null(colnames(x))) colnames(x) <- paste("Series", seq(1, nc, 1))
  if (missing(minw)) minw <-  floor((r0 <- 0.01 + 1.8 / sqrt(nr)) * nr)
  # checks
  assert_na(data)
  assert_positive_int(minw, greater_than = 2)
  assert_positive_int(lag, strictly = FALSE)

  point <- nr - minw - lag

  adf <- sadf <- gsadf <- drop(matrix(0, 1, nc,
                                     dimnames = list(NULL, colnames(x))))
  badf <- bsadf <- matrix(0, point, nc, dimnames = list(NULL, colnames(x)))

  for (i in 1:nc) {
>>>>>>> pipe friendly version: confrom better with ggplot and S3 methods

    yxmat <- unroot(x[, i], lag = lag)
    results <- rls_gsadf(yxmat, min_win = minw, lag = lag)

    badf[, i] <- results[1:point]
    adf[i]  <- results[point + 1]
    sadf[i] <- results[point + 2]
    gsadf[i] <- results[point + 3]
    bsadf[, i] <- results[-c(1:(point + 3))]
  }

<<<<<<< HEAD
  bsadf_panel <- apply(bsadf, 1, mean)[-c(1:minw)]
  gsadf_panel <- max(bsadf_panel)

  value <- structure(list(adf = adf,
                          badf = badf[-c(1:(minw)), , drop = F],
                          sadf = sadf,
                          bsadf = bsadf[-c(1:(minw)), , drop = F],
=======
  bsadf_panel <- apply(bsadf, 1, mean)
  gsadf_panel <- max(bsadf_panel)

  value <- structure(list(adf = adf,
                          badf = badf,
                          sadf = sadf,
                          bsadf = bsadf,
>>>>>>> pipe friendly version: confrom better with ggplot and S3 methods
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


unroot <- function(x, lag = 0) {
  if (lag == 0) {
    x_embed <- embed(x, 2)
    yxmat <- cbind(x_embed[, 1], x_embed[, 2])
  } else {
    x_embed <- embed(x, lag + 2)
    dx_embed <- embed(diff(x), lag + 1)[, -1]
    x_lev <- x_embed[, 1]
    x_lag <- x_embed[, 2]
    yxmat <- cbind(x_lev, 1, x_lag, dx_embed)
  }
  return(yxmat)
}
