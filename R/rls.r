
# Multiple recursive linear regression  -----------------------------------


rls_gsadf <- function(yxmat, wmin) {
  end <- NROW(yxmat)
  start <- wmin
  total <- end - start + 1
  x <- as.matrix(yxmat[, -1])
  y <- as.matrix(yxmat[, 1])
  nc <- NCOL(x)
  tstat <- matrix(data = -999, nrow = end, ncol = total)

  for (j in 1:total) {
    for (i in j:total) {
      sx <- x[j:(start + i - 1), ]
      sy <- y[j:(start + i - 1), ]
      if (i == j) {
        g <- chol2inv(chol(crossprod(sx)))
        b <- as.matrix(g %*% crossprod(sx, sy))
        res <- sy - sx %*% b
        sqres <- sum(res^2)
        vares <- sqres / (start + i - j - nc)
        sb <- sqrt(vares * diag(g))
        tstat[wmin + i - 1, j] <- (b[2] - 1) / sb[2]
      } else {
        tsxn <- x[start + i - 1, ]
        syn <- y[start + i - 1, ]
        kaka <- 1 / (1 + crossprod(tsxn, g %*% tsxn))
        g <- g - kaka[1] * g %*% tcrossprod(tsxn) %*% g
        b <- b - g %*% tsxn %*% (crossprod(tsxn, b) - syn)
        res <- sy - sx %*% b
        sqres <- sum(res^2)
        vares <- sqres / (start + i - j - nc)
        sb <- sqrt(vares * diag(g))
        tstat[wmin + i - 1, j] <- (b[2] - 1) / sb[2]
      }
    }
  }
  adf <- tstat[end, 1] # or badf[end]
  badf <- tstat[, 1]
  sadf <- max(tstat[, 1])
  bsadf <- apply(tstat, 1, max) # or max(badf)
  gsadf <- max(bsadf)

  return(list(
    adf = adf, badf = badf,
    sadf = sadf, bsadf = bsadf, gsadf = gsadf
  ))
}
