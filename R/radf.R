#' Recursive Augmented Dickey Fuller test
#'
#' \code{radf} returns the t-statistics from a recursive augmented dickey fuller test
#'
#' @param x a data.frame or matrix.
#' @param minw a positive integer.
#' @param lag a non-negative integer.
#' @param format a character string. If not specified, it will try "\%Y-\%m-\%d".
#'
#' @return a list
#'
#' @references Phillips, P. C. B., Wu, Y., & Yu, J. (2011). Explosive Behavior In The 1990S Nasdaq:
#' When Did Exuberance Escalate Asset Values? International Economic Review, 52(1), 201-226.
#'
#' @references Phillips, P. C. B., Shi, S., & Yu, J. (2015). Testing for multiple bubbles:
#' Historical episodes of exuberance and collapse in the S&P 500. International Economic Review, 5
#' 6(4), 1043-1078.
#'
#' @importFrom readr parse_datetime
#' @importFrom stats embed
#' @export
radf <- function(x, minw, lag = 0, format = "%Y-%m-%d"){

  x  <- as.matrix(x)
  nc <- NCOL(x)
  nr <- NROW(x)
  if (missing(minw)) {
    r0 <- 0.01 + 1.8 / sqrt(nr)
    minw = floor(r0 * nr)
  } else if (minw < 3) {
    stop( "Argument 'minw' is too small")
  }
  is.positive.int(minw)
  is.nonnegeative.int(lag)

  if (is.null(colnames(x))) {
    colnames(x) <- paste0("series", seq(1, nc, 1))
  }else{
    colnames(x) <- colnames(x)
  }

  adf   <- drop(matrix(0, 1, nc, dimnames = list(NULL, colnames(x))))
  badf  <- matrix(0, nr - 1 - lag, nc, dimnames = list(NULL, colnames(x)))
  sadf  <- drop(matrix(0, 1, nc, dimnames = list(NULL, colnames(x))))
  gsadf <- drop(matrix(0, 1, nc, dimnames = list(NULL, colnames(x))))
  bsadf <- matrix(0, nr - 1 - lag, nc, dimnames = list(NULL, colnames(x)))

  for (i in 1:nc) {
    if (lag == 0) {
      x_embed  <- embed(x[, i], 2)
      yxmat    <- cbind(x_embed[,1], 1, x_embed[, 2])
    }else{
      x_embed  <- embed(x[, i], lag + 2)
      dx_embed <- embed(diff(x[, i]), lag + 1)[, -1];
      x_lev    <- x_embed[, 1]
      x_lag    <- x_embed[, 2]
      yxmat    <- cbind(x_lev, 1, x_lag, dx_embed)
    }
    results <- rls_gsadf(yxmat, minw);

    adf[i]     <- results$adf
    badf[, i]  <- results$badf
    sadf[i]    <- results$sadf
    gsadf[i]   <- results$gsadf
    bsadf[, i] <- results$bsadf
  }


  value <- list(adf   = adf ,
                badf  = badf[-c(1:(minw)), , drop = F] ,
                sadf  = sadf,
                bsadf = bsadf[-c(1:(minw)), , drop = F] ,
                gsadf = gsadf)
                #info = list(lag = lag, minw = minw, names = colnames(x)))


  # if (is.character(rownames(x))) {
  #   value$info$date <- as.Date(parse_datetime(rownames(x), format = format))
  # } else if (is.numeric(rownames(x))) {
  #   value$info$date <- rownames(x)
  # }else{
  #   value$info$date <- seq(1, nr, 1)
  # }

  if (is.character(rownames(x))) {
    attr(value, "date") <- as.Date(parse_datetime(rownames(x), format = format))
  } else if (is.numeric(rownames(x))) {
    attr(value, "date") <- rownames(x)
  }else{
    attr(value, "date") <- seq(1, nr, 1)
  }
  attr(value, "class") <- "radf"#append(class(value), "radf")
  attr(value, "lag") <- lag
  attr(value, "minw") <- minw
  attr(value, "col_names") <- colnames(x)

  return(value)
}

