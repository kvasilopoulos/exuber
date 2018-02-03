#' Recursive augmented dickey fuller
#'
#' @param x a data.frame or matrix
#' @param minw a positive integer
#' @param lag a non-negative integer
#' @param format default
#'
#' @return a list
#'
#' @importFrom readr parse_datetime
#' @export
radf <- function(x,
                 minw,
                 lag = 0,
                 format = "%m-%Y"){

  x  <- as.matrix(x)
  nc <- NCOL(x)
  nr <- NROW(x)

  if (!lag == round(lag) | lag < 0) {
    stop("'lag' should be a non-negative integer")
  }

  if (missing(minw)) {
    r0 <- 0.01 + 1.8 / sqrt(nr)
    minw = floor(r0 * nr)
  } else if (!minw == round(minw) | minw <= 0) {
    stop("'minw' should be a positive integer")
  }

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


  value <- list(adf   = adf,
                badf  = badf[-c(1:(minw)), , drop = F],
                sadf  = sadf,
                bsadf = bsadf[-c(1:(minw)), , drop = F],
                gsadf = gsadf,
                info  =  list(lag = lag, minw = minw, names = colnames(x)))


  if (is.character(rownames(x))) {
    value$info$date <- as.Date(parse_datetime(rownames(x)), format = format)
  } else if (is.numeric(rownames(x))) {
    value$info$date <- rownames(x)
  }else{
    value$info$date <- seq(1, nr, 1)
  }

  attr(value, "class") <- append(class(value), "radf")
  return(value)
}

