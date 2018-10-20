#' Simulated Monte Carlo critical values
#'
#' A dataset containing the simulated critical values for 2000 observations
#' with default minimum window.
#'
#' @format A list with lower level lists that contain
#' \describe{
#'   \item{adf:}{Augmented Dickey-Fuller}
#'   \item{badf:}{Backward Augmented Dickey-Fuller}
#'   \item{sadf:}{Supremum Augmented Dickey-Fuller}
#'   \item{bsadf:}{Backward Supremum Augmented Dickey-Fuller}
#'   \item{gsadf:}{Generalized Supremum Augmented Dickey Fuller}
#'   ...
#' }
#' @source simulated from exuber package function \code{\link[=mc_cv]{mc_cv()}}
"crit"

#'@export
#'@importFrom tibble enframe
print.crit <- function(x, ...) {
  # we dont want to overwhelm the console
  print(tibble::enframe(x))
}

