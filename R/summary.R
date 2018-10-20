#' Summary statistics
#'
#' @param object An object of class \code{\link[=radf]{radf()}}.
#' @param cv An object of class "cv". The output of \code{\link[=mc_cv]{mc_cv()}},
#' \code{\link[=wb_cv]{wb_cv()}} or \code{\link[=sb_cv]{sb_cv()}}
#' @param ... further arguements passed to methods, not used.
#'
#' @return Returns a list of summary statistics,
#' the t-statistic and the critical values of the ADF, SADF and GSADF.
#'
#' @name summary
#'
#' @examples
#' \donttest{
#' # Simulate bubble processes, compute the t-stat and critical values
#' set.seed(4441)
#' dta <- cbind(sim_dgp1(n = 100), sim_dgp2(n = 100))
#' rfd <- radf(dta)
#' mc <- mc_cv(n = 100)
#'
#' # Summary, diagnostics and datestamp (default)
#' summary(x = rfd, y = mc)
#' diagnostics(x = rfd, y = mc)
#' datestamp(x = rfd, y = mc)
#'
#' # Diagnostics for 'sadf'
#' diagnostics(x = rfd, y = mc, option = "sadf")
#'
#' # Use rule of thumb to omit periods of explosiveness which are short-lived
#' rot = round(log(NROW(rfd)))
#' datestamp(x = rfd, y = mc, min_duration = rot)
#' }
#' @export
summary.radf <- function(object, cv, ...) {

  cv <- if (missing(cv)) get_crit(object) else cv
  assert_class(cv, "cv")

  x <- object
  y <- cv
  assert_equal_arg(x, y)

  ret <- list()
  if (method(y) == "Wild Bootstrap") {

    for (i in seq_along(col_names(x))) {
      df1 <- c(x$adf[i], y$adf_cv[i, ])
      df2 <- c(x$sadf[i], y$sadf_cv[i, ])
      df3 <- c(x$gsadf[i], y$gsadf_cv[i, ])
      df <- data.frame(rbind(df1, df2, df3),
                       row.names = c("ADF", "SADF", "GSADF"))
      colnames(df) <- c("t-stat", "90%", "95%", "99%")
      ret[[i]] <- df
    }
    names(ret) <- col_names(x)
  } else if (method(y) == "Monte Carlo") {

    for (i in seq_along(col_names(x))) {
      df1 <- c(x$adf[i], y$adf_cv)
      df2 <- c(x$sadf[i], y$sadf_cv)
      df3 <- c(x$gsadf[i], y$gsadf_cv)
      df <- data.frame(rbind(df1, df2, df3),
                       row.names = c("ADF", "SADF", "GSADF"))
      colnames(df) <- c("tstat", "90%", "95%", "99%")
      ret[[i]] <- df
    }
    names(ret) <- col_names(x)
  } else if (method(y) == "Sieve Bootstrap") {
    ret <- cbind(x$gsadf_panel, t(y$gsadf_panel_cv))
    colnames(ret) <- c("t-stat", "90%", "95%", "99%")
  }

  structure(ret,
            minw = minw(x),
            lag = lagr(x),
            method = method(y),
            iter = iter(y),
            class = "summary.radf")
}


#' @export
print.summary.radf <- function(x, digits = max(3L, getOption("digits") - 3L),
                               ...) {
  cat(
    "\n", "Recursive Unit Root\n",
    "----------------------------------\n",
    "H0:", "Unit root\n",
    "H1:", "Explosive root\n",
    "----------------------------------\n",
    "Critical values:", method(x), "\n",
    "Minimum window:", minw(x), "\n",
    if (method(x) == "Monte Carlo") "Iterations:" else "Bootstraps:", iter(x),
    "\n Lag:", lagr(x), "\n",
    "----------------------------------"
  )
  if (method(x) == "Sieve Bootstrap") {
    cat("\n Panel\n")
    pp <- x[1, , drop = FALSE]
    rownames(pp) <- "GSADF"
    print(format(pp, digits = digits), print.gap = 2L, quote = FALSE)
  }else{
    for (i in seq_along(x)) {
      cat("\n", names(x)[i], "\n")
      print(format(x[[i]], digits = digits), print.gap = 2L)
    }
  }
}


#' @inheritParams summary
#' @param option Whether to apply the "gsadf" or "sadf" methodology. Default is
#' "gsadf".
#' @describeIn datestamp Finds the series that reject the null for 95\%
#' significance level.
#'
#' @return \code{\link[=diagnostics]{diagnostics()}}: Returns a list with the
#' series that reject and the series that do not reject the Null Hypothesis
#'
#' @details In \code{\link[=diagnostics]{diagnostics()}} and
#' \code{\link[=datestamp]{datestamp()}} have also stored a vector in {0,1}
#' that corresponds to {reject, accept} respectively.
#'
#' @importFrom dplyr case_when
#' @export
diagnostics <- function(object, cv, option = c("gsadf", "sadf")) {

  assert_class(object, "radf")
  cv <- if (missing(cv)) get_crit(object) else cv
  assert_class(cv, "cv")
  option <- match.arg(option)

  x <- object
  y <- cv
  assert_equal_arg(x, y)
  panel <- if (method(y) == "Sieve Bootstrap") TRUE else FALSE

  if (option == "gsadf") {

    tstat <- if (panel) x$gsadf_panel else x$gsadf

    if (method(y) == "Monte Carlo") {
      cv1 <- y$gsadf_cv[1]
      cv2 <- y$gsadf_cv[2]
      cv3 <- y$gsadf_cv[3]
    } else if (method(y) == "Wild Bootstrap") {
      cv1 <- y$gsadf_cv[, 1]
      cv2 <- y$gsadf_cv[, 2]
      cv3 <- y$gsadf_cv[, 3]
    } else if (method(y) == "Sieve Bootstrap") {
      cv1 <- y$gsadf_panel_cv[1]
      cv2 <- y$gsadf_panel_cv[2]
      cv3 <- y$gsadf_panel_cv[3]
    }

  } else if (option == "sadf") {

    tstat <- x$sadf

    if (method(y) == "Monte Carlo") {
      cv1 <- y$sadf_cv[1]
      cv2 <- y$sadf_cv[2]
      cv3 <- y$sadf_cv[3]
    } else {
      stop("'sadf' applies onyl to MC critical values", call. = FALSE)
    }
  }

  # in case of simulation exercises
  bool <- case_when(
    tstat < cv2 ~ 0,
    tstat >= cv2  ~ 1)

  sig <- case_when(
    tstat < cv1 ~ "Reject",
    tstat >= cv1 & tstat < cv2 ~ "90%",
    tstat >= cv2 & tstat < cv3 ~ "95%",
    tstat >= cv3 ~ "99%")

  if (all(sig == "Reject")) {
    stop("Cannot reject H0", call. = FALSE)
  } else if (all(bool == 0)) { # bool takes zero if below 95
    stop("Cannot reject H0 for significance level 95%", call. = FALSE)
  } else {
    if (panel) {
      accepted <- ifelse(length(bool),"Panel", NA)
      rejected <- ifelse(length(bool), NA, "Panel")
    }else {
      accepted <- col_names(x)[as.logical(bool)]
      rejected <- col_names(x)[!as.logical(bool)]
    }
  }

  structure(list(accepted = accepted,
                 rejected = rejected,
                 sig = sig,
                 bool = bool),
            panel = panel,
            col_names = if (!panel) col_names(x),
            class = "diagnostics")

}


#' @export
print.diagnostics <- function(x, ...) {

  if (attr(x, "panel")) {
    cat(
      "\n",
      "Diagnostics: Panel\n ",
      "-----------------------------------"
    )
    if (x$sig == "Reject") {
      cat("\n", "Cannot reject H0!")
    } else {
      cat("\n", "Rejects H0 for significance level", x$sig, "\n",
          "-----------------------------------",
          "\n Procced for date stampting and plotting")
    }
  }else{
    cat(
      "\n",
      "Diagnostics: Individual\n",
      "-----------------------------------"
    )
    for (i in seq_along(attr(x, "col_names"))) {
      cat("\n", attr(x, "col_names")[i], ":", sep = "")
      if (x$sig[i] == "Reject") {
        cat("\n", "Cannot reject H0!")
      } else {
        cat("\n", "Rejects H0 for significance level", x$sig[i])
      }
    }
    cat("\n",
        "-----------------------------------",
        "\n Procced for date stampting and plotting for", length(x$accepted),
        "variable(s)\n",
        deparse(as.vector(x$accepted))
    )
  }
}


#' Diagnostics and date stamping periods of mildly explosive behaviour
#'
#' Computes the origination, termination and duration of episodes during which
#' the time series display explosive dynamics.
#'
#' @inheritParams diagnostics
#' @param min_duration The minimum duration of an explosive period for it to be
#' reported. Default is 0.
#'
#' @return \code{\link[=datestamp]{datestamp()}}: Returns a list of values for
#' each explosive sub-period, giving the origin and termination dates as well
#' as the number of periods explosive behavior lasts.
#'
#' @details
#' Setting \code{min_duration} allows temporary spikes above the critical value
#' sequence to be removed. Phillips et al. (2015) propose a simple way to remove
#' small periods of explosiveness by a rule of thumb such as "log(T)" or
#' "log(T)/T", where T is the number of observations.
#'
#' @references Phillips, P. C. B., Shi, S., & Yu, J. (2015). Testing for
#' Multiple Bubbles: Historical Episodes of Exuberance and Collapse in the
#' S&P 500. International Economic Review, 56(4), 1043-1078.
#'
#' @importFrom rlang sym !!
#' @importFrom dplyr filter
#' @export
#'
datestamp <- function(object, cv, option = c("gsadf", "sadf"),
                      min_duration = NULL) {

  assert_class(object, "radf")
  cv <- if (missing(cv)) get_crit(object) else cv
  assert_class(cv, "cv")
  option <- match.arg(option)
  if (is.null(min_duration)) min_duration <- 0

  x <- object
  y <- cv
  panel <- if (method(y) == "Sieve Bootstrap") TRUE else FALSE
  assert_equal_arg(x, y)
  assert_positive_int(min_duration, strictly = FALSE)

  choice <- diagnostics(x, y, option) %>%
    pluck("accepted")
  reps <- if (panel) 1 else match(choice, col_names(x))
  dating <- index(x)[-c(1:(minw(x) + lagr(x)))]

  ds <- vector("list", length(choice))
  if (panel) {
    cv <- if (lagr(x) == 0) {
      y$bsadf_panel_cv[, 2]
    }else {
      y$bsadf_panel_cv[-c(1:lagr(x)), 2]
    }
    ds <- list(which(x$bsadf_panel > cv) + minw(x) + lagr(x))
  }

  for (i in seq_along(choice)) {
    if (method(y) == "Monte Carlo") {
      if (option == "gsadf") {

        cv <- if (lagr(x) == 0) {
          y$bsadf_cv[, 2]
        }else{
          y$bsadf_cv[-c(1:lagr(x)), 2]
        }
        ds[[i]] <- which(x$bsadf[, reps[i]] > cv) + minw(x) + lagr(x)

      } else if (option == "sadf") {

        cv <- if (lagr(x) == 0) {
          y$badf_cv[, 2]
        }else{
          y$badf_cv[-c(1:lagr(x)), 2]
        }
        ds[[i]] <- which(x$badf[, i] > cv) + minw(x) + lagr(x)
      }
    } else if (method(y) == "Wild Bootstrap") {

        cv <- if (lagr(x) == 0) {
          y$bsadf_cv[, 2, i]
        }else{
          y$bsadf_cv[-c(1:lagr(x)), 2, i]
        }
        ds[[i]] <- which(x$bsadf[, reps[i]] > cv) + minw(x) + lagr(x)
    }
  }

  # identification of periods
  stamp <- function(ds) {
    start <- ds[c(TRUE, diff(ds) != 1)]
    end <- ds[c(diff(ds) != 1, TRUE)]
    end[end - start == 0] <- end[end - start == 0] + 1
    duration <- end - start + 1
    foo <- data.frame("Start" = start, "End" = end, "Duration" = duration)
    foo
  }

  ds_stamp <- lapply(ds, function(z) z %>%
      stamp() %>%
      filter(!!sym("Duration") >= min_duration) %>%
      as.matrix())

  add_index <- lapply(ds_stamp, function(t) data.frame(
      "Start" = index(x)[t[, 1]],
      "End" = index(x)[t[, 2]],
      "Duration" = t[, 3], row.names = NULL
    ))

  #min_duration may cause to exclude periods or the whole sample
  min_reject <- lapply(ds_stamp, function(t) length(t) == 0) %>% unlist()
  res <- add_index[!min_reject]
  names(res) <- choice[!min_reject]

  if (length(res) == 0) {
    stop("Argument 'min_duration' excludes all explosive periods",
      call. = FALSE)
  }

  dummy <- matrix(0, nrow = length(index(x)), ncol = length(choice),
                    dimnames = list(seq_along(index(x)), if (panel) "Panel"
                                    else col_names(x)[reps]))
  for (z in seq_along(choice)) {
    dummy[ds[[z]], z] <- 1
  }
  res[["bool"]] <- dummy

  structure(res,
            panel = panel,
            min_duration = min_duration,
            option = option,
            class = c("list", "datestamp"))
}

#' @export
print.datestamp <- function(x, ...) {

    if (attr(x, "panel")) {
      cat(
        "\nDatestamp: Panel\n",
        "-----------------------------------\n")
      print(x[[1]]) # drops list
    }else if (attr(x, "panel") == FALSE) {
      cat(
        "\nDatestamp: Individual\n",
        "-----------------------------------\n"
      )
      print.listof(x[-length(x)])
    }else{
    # in case of stacked fortify
    cat(
      "\nDatestamp: \n",
      "-----------------------------------\n"
    )
    print.listof(x)
  }
}

