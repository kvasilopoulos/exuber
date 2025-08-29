unroot_adf_null <- function(x, lag) {
  dx_embed <- embed(diff(x), lag + 1)
  dy <- dx_embed[, 1]
  if (lag == 0) {
    yxmat <- cbind(dy, ct = 1)
  } else {
    dx_lags <- dx_embed[, -1]
    yxmat <- cbind(dy, ct = 1, dx_lags)
    colnames(yxmat) <- c("dy", "ct", paste0("dy_lags", 1:lag))
  }
  yxmat
}

unroot_adf <- function(x, lag) {
  x_embed <- embed(x, lag + 2) # conform length
  x_lag <- x_embed[, 2]
  dx_embed <- embed(diff(x), lag + 1)
  dy <- dx_embed[, 1]
  if (lag == 0) {
    yxmat <- cbind(dy, 1, x_lag)
    colnames(yxmat) <- c("dy", "ct", "y")
  } else {
    dx_lags <- dx_embed[, -1]
    yxmat <- cbind(dy, 1, x_lag, dx_lags)
    colnames(yxmat) <- c("dy", "ct", "y", paste0("dy_lags", 1:lag))
  }
  yxmat
}


#' @importFrom stats embed
unroot <- function(x, lag = 0) {
  if (lag == 0) {
    x_embed <- embed(x, 2)
    yxmat <- cbind(x_embed[, 1], x_embed[, 2])
    colnames(yxmat) <- c("x_lev", "x_lag")
  } else {
    x_embed <- embed(x, lag + 2)
    dx_embed <- embed(diff(x), lag + 1)[, -1]
    x_lev <- x_embed[, 1]
    x_lag <- x_embed[, 2]
    yxmat <- cbind(x_lev, ct = 1, x_lag, dx_embed)
    colnames(yxmat) <- c("x_lev", "ct", "x_lag", paste0("dx_embed", 1:lag))
  }
  yxmat
}
