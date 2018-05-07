#' Date stamping the bubble period(s)
#'
#' \code{datestamp} computes the origination, termination and duration of the bubble episode(s) detected by
#' \code{radf}. Setting \code{min.duration} allows temporary spikes above the critical value sequence to be removed.
#'
#' @inheritParams report
#' @param option whether to plot badf-tstat with the adf-cv or bsadf-tsat with the badf-cv.
#' The default value is "badf".
#' @param min_duration the minimum duration of an explosive period for it to be reported. Default is 0.
#'
#' @return Returns a list of values for each explosive sub-period, giving the origin and termination dates as well as the number of periods explosive behaviour lasts.
#' @references Add reference to Phillips paper here.
#'
#' @importFrom tidyr %>% drop_na
#' @importFrom dplyr filter
#' @importFrom rlang sym
#' @export
#'
datestamp2 <- function(x, y, option = c("gsadf","sadf"), min_duration = 0) {

  radf_check(x)
  cv_check(y)
  minw_check(x, y)
  is.nonnegeative.int(min_duration)

  option <- match.arg(option)

  if (method(y) == "Wild Bootstrap" & option == "sadf") {
    stop("  Explosive periods with Wild Bootstraped critical valuesapply only for the option 'gsadf' ",
         call. = FALSE)
  }

  iter <- diagnostics2(x, y, option) %>% match(col_names(x))
  dating <- index(x)[-c(1:(minw(x) + 1 + lagr(x)))]

  ds <- vector("list", length(iter))
  j <- 1
  for (i in iter) {
    if (method(y) == "Monte Carlo") {
      if (option == "gsadf") {
        ds[[j]] <- which(x$bsadf[,i] > y$badf_cv[,2]) + minw(x) + 1
      }else if (option == "sadf") {
        ds[[j]] <- which(x$badf[, i] > y$adf_cv[2]) + minw(x) + 1
      }
    }else if (method(y) == "Wild Bootstrap") {
      if (option == "gsadf") {
        ds[[j]] <- which(x$bsadf[,i] > y$badf_cv[, 2, i]) + minw(x) + 1
      }else if (option == "sadf") {
        ds[[j]] <- which(x$badf[, i] > rep(y$adf_cv[i, 2], NROW(x$badf))) + minw(x) + 1
      }
    }
    j <- j + 1
  }
  ds_stamp <- lapply(ds, function(z) z %>% stamp() %>%
                       filter(Duration >= min_duration) %>% as.matrix())

  index_corrected <- lapply(ds_stamp, function(t) data.frame(
    "Start" = index(x)[t[, 1]],
    "End" = index(x)[t[, 2]],
    "Duration" = t[, 3], row.names = NULL
    ))

  min_reject <- lapply(ds_stamp, is.data.frame0) %>% unlist()
  res <- index_corrected[!min_reject]

  if (length(res) == 0) {
    stop("Argument 'min_duration' excludes all the explosive periods", call. = FALSE)
  }

  names(res) <- col_names(x)[iter][!min_reject]
  res
}

is.data.frame0 <- function(df) {
  is.data.frame(df) && nrow(df) == 0
}

stamp <- function(ds) {
  start <- ds[c(TRUE, diff(ds) != 1)]
  end   <- ds[c(diff(ds) != 1, TRUE)]
  end[end - start == 0] <- end[end - start == 0] + 1
  duration <- end - start + 1
  foo <- data.frame("Start" = start, "End" = end, "Duration" = duration)
  foo
}

repn <- function(x) {
  ln <- lapply(x, function(x) NROW(x)) %>% unlist()
  nm <- names(x)
  rep(nm, ln)
}
