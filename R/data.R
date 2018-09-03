#' Simulated Monte Carlo critical values
#'
#' A dataset containing the simulated critical values from 6:500 observations
#' wirh default minimum window.
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
"crit"


#'@export
#'@importFrom tibble enframe
print.crit <- function(x, ...) {
  # cat("Monte Carlo critical values - # of observations {6-500}")
  # cat("\n", paste0("$ _nan", seq(1,5), ": NULL\n"))
  # cat("", paste0("$ n", seq(6,10), ":List of 5\n"))
  # cat("# ... with 490 more rows")
  print(tibble::enframe(x))
}

