

diagnostics2 <- function(x,y, option = c("gsadf", "sadf")) {

  radf_check(x)
  cv_check(y)
  minw_check(x, y)
  option <- match.arg(option)


    if (option == "gsadf") {
      tstat <- x$gsadf
      if (method(y) == "Monte Carlo") {
        cv1 <- y$gsadf_cv[1]
        cv2 <- y$gsadf_cv[2]
        cv3 <- y$gsadf_cv[3]
      }else{
        cv1 <- y$gsadf_cv[, 1]
        cv2 <- y$gsadf_cv[, 2]
        cv3 <- y$gsadf_cv[, 3]
      }
    }else{
      tstat <- rf$sadf
      if (method(y) == "Monte Carlo") {
        cv1 <- y$sadf_cv[1]
        cv2 <- y$sadf_cv[2]
        cv3 <- y$sadf_cv[3]
      }else{
        cv1 <- y$sadf_cv[, 1]
        cv2 <- y$sadf_cv[, 2]
        cv3 <- y$sadf_cv[, 3]
      }
    }

    sig <- case_when(
      tstat < cv1 ~ "Reject",
      tstat >= cv1 & tstat < cv2 ~ "90%",
      tstat >= cv2 & tstat < cv3 ~ "90%",
      tstat >= cv3 ~ "99%"
    )



  if (all(sig == "Reject")) {
    stop("Cannot reject H0, do not proceed for date stamping or plotting", call. = FALSE)
  }

  cond <- sig == "95%" | sig == "99%"
  proceed <- col_names(x)[cond]

  attr(proceed, "significance") <- sig
  class(proceed) <- "diagnostics"
  attr(proceed, "col_names") <- col_names(x)

  proceed
}
