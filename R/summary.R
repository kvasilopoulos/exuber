#' Summary statistics, diagnostics and date stamping periods of mildly explosive behaviour.
#'
#'
#' @param object An object of class \code{\link[=radf]{radf()}}.
#' @param cv An object, of class "cv". The output of \code{\link[=mc_cv]{mc_cv()}},
#' \code{\link[=sb_cv]{sb_cv()}} or \code{\link[=sb_cv]{sbc_cv()}}
#' @param panel Logical. If false it will report the panel statistic.
#' @param ... furhter arguements passe to method, not used.
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
summary.radf <- function(object, cv, panel = FALSE, ...) {

  # args
  x <- object
  y <- provide_crit(cv, x)
  # checks
  assert_class(y, cv)
  stopifnot(is.logical(panel))
  assert_panel(x, y, panel = panel)
  assert_equal_minw(x, y)

  ret <- list()
  if (panel) {
    ret <- cbind(x$gsadf_panel, t(y$gsadf_cv))
    colnames(ret) <- c("t-stat", "90%", "95%", "99%")
  }else{
    if (method(y) == "Wild Bootstrap") {
      stopifnot(method(y) != "Sieve Bootstrap")
      for (i in seq_along(col_names(x))) {
        df1 <- c(x$adf[i], y$adf_cv[i, ])
        df2 <- c(x$sadf[i], y$sadf_cv[i, ])
        df3 <- c(x$gsadf[i], y$gsadf_cv[i, ])
        df <- data.frame(rbind(df1, df2, df3),
                         row.names = c("ADF", "SADF", "GSADF")
        )
        colnames(df) <- c("t-stat", "90%", "95%", "99%")
        ret[[i]] <- df
      }
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
    }
    names(ret) <- col_names(x)
  }

  structure(ret,
            minw = minw(y),
            lag = lagr(y),
            method = method(y),
            panel = panel,
            iter = iter(y),
            class = "summary.radf")
}


#' @export
print.summary.radf <- function(x, digits = max(2L, getOption("digits") - 4L),
                               ...) {
  cat(
    "\n", "Recursive Unit Root\n",
    "------------------------------\n",
    "H0:", "Unit root\n",
    "H1:", "Explosive root\n",
    "------------------------------\n",
    "Critical values:", method(x), "\n",
    "Minimum window:", minw(x), "\n",
    if (method(x) == "Monte Carlo") "Iterations:" else "Bootstraps:", iter(x),
    "\n Lag:", lagr(x), "\n",
    "------------------------------"
  )
  if (attr(x, "panel")) {
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
diagnostics <- function(object, cv,
                        option = c("gsadf", "sadf"),
                        panel = FALSE) {

  # args
  x <- object
  y <- provide_crit(cv, x)
  option <- match.arg(option)
  # checks
  assert_class(x, radf)
  assert_class(y, cv)
  assert_panel(x, y, panel = panel)
  assert_equal_minw(x, y)
  stopifnot(is.logical(panel))

  if (option == "gsadf") {

    tstat <- if (panel) x$gsadf_panel else x$gsadf

    if (method(y) %in% c("Monte Carlo", "Sieve Bootstrap")) {
      cv1 <- y$gsadf_cv[1]
      cv2 <- y$gsadf_cv[2]
      cv3 <- y$gsadf_cv[3]
    } else if (method(y) == "Wild Bootstrap") {
      cv1 <- y$gsadf_cv[, 1]
      cv2 <- y$gsadf_cv[, 2]
      cv3 <- y$gsadf_cv[, 3]
    }
  } else if (option == "sadf") {
    tstat <- x$sadf
    if (method(y) == "Monte Carlo") {
      cv1 <- y$sadf_cv[1]
      cv2 <- y$sadf_cv[2]
      cv3 <- y$sadf_cv[3]
    } else if (method(y) == "Wild Bootstrap") {
      stop("Bootstraped critical values apply only 'gsadf' option",
        call. = FALSE)
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
    stop("Cannot reject H0, do not proceed", call. = FALSE)
  } else if (all(bool == 0)) { # bool takes zero if below 95
    stop("You cannot reject H0 for significance level 95%", call. = FALSE)
  } else {
    if (panel) {
      structure(list(accepted = ifelse(length(bool),"Panel", NA),
                     rejected = ifelse(length(bool), NA, "Panel"),
                     sig = sig),
                bool = bool,
                class = "diagnostics_panel")

    }else {
      structure(list(accepted = col_names(x)[as.logical(bool)],
                     rejected = col_names(x)[!as.logical(bool)],
                     sig = sig),
                col_names = col_names(x),
                bool = bool,
                class = "diagnostics")
    }
  }
}

#' @export
print.diagnostics_panel <- function(x, ...) {
  cat(
    "\n",
    "Diagnostics: Panel\n ",
    "------------------------------"
  )
  if (x$sig == "Reject") {
    cat("\n", "Cannot reject H0!")
  } else {
    cat("\n", "Rejects H0 for significance level", x$sig, "\n",
        "-------------------------------",
        "\n Procced for date stampting and plotting")
  }
}


#' @export
print.diagnostics <- function(x, ...) {
  cat(
    "\n",
    "Diagnostics: Individual\n",
    "------------------------------"
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
    "------------------------------",
    "\n Procced for date stampting and plotting for", length(x$accepted),
    "variable(s)\n",
    deparse(as.vector(x$accepted))
  )
}


#' Diagnostics and date stamping periods of mildly explosive behaviour
#'
#' Computes the origination, termination and duration of episodes during which
#' the time series display explosive dynamics.
#'
#' @param min_duration The minimum duration of an explosive period for it to be
#' reported. Default is 0.
#' @inheritParams diagnostics
#'
#' @return \code{\link[=timestamp]{timestamp()}}: Returns a list of values for
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
#' @importFrom rlang sym
#' @importFrom dplyr filter
#' @export
#'
datestamp <- function(object, cv, option = c("gsadf", "sadf"), panel = FALSE,
                      min_duration) {
  # args
  x <- object
  y <- provide_crit(cv, x)
  option <- match.arg(option)
  if (missing(min_duration)) min_duration <- 0
  # checks
  assert_class(x, radf)
  assert_class(y, cv)
  assert_panel(x, y, panel = panel)
  assert_equal_minw(x, y)
  stopifnot(is.logical(panel))
  assert_positive_int(min_duration, strictly = FALSE)


  choice <- diagnostics(x, y, option, panel) %>%
    with(get("accepted"))
  reps <- if (panel) 1 else match(choice, col_names(x))
  dating <- index(x)[-c(1:(minw(x) + lagr(x)))]

  ds <- vector("list", length(choice))

  # error (sadf && panel) ~~~~~~ assert_panel(option)

  # panel instances
  if (panel && option == "gsadf" ) {
    if (method(y) %in% c("Monte Carlo","Sieve Bootstrap")) {
      cv <- ifelse(lagr(x) == 0,
                   y$bsadf_cv[, 2],
                   y$bsadf_cv[-c(1:lagr(x)), 2])
      ds <- list(which(x$bsadf_panel > cv) + minw(x) + lagr(x))
    }
  }

  # individual
  for (i in seq_along(choice)) {
    if (method(y) == "Monte Carlo") {
      if (option == "gsadf") {
        if (panel) {
        }else{
          cv <- ifelse(lagr(x) == 0, y$badf_cv[, 2],
                                     y$badf_cv[-c(1:lagr(x)), 2])
          ds[[i]] <- which(x$bsadf[, reps[i]] > cv) + minw(x) + lagr(x)
         }
      } else if (option == "sadf") {
        ds[[i]] <- which(x$badf[, i] > y$adf_cv[2]) + minw(x)
      }
    } else if (method(y) == "Wild Bootstrap") {
      if (option == "gsadf") {
        cv <- ifelse(lagr(x) == 0, y$badf_cv[, 2, i],
                                   y$badf_cv[-c(1:lagr(x)), 2, i])
        ds[[i]] <- which(x$bsadf[, reps[i]] > cv) + minw(x) + lagr(x)
      }
    }
  }

  # this does the identification of periods
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

  index_add <- lapply(ds_stamp, function(t) data.frame(
      "Start" = index(x)[t[, 1]],
      "End" = index(x)[t[, 2]],
      "Duration" = t[, 3], row.names = NULL
    ))

  #min_duration may cause to exclude periods or the whole sample
  min_reject <- lapply(ds_stamp, function(t) length(t) == 0) %>% unlist()
  res <- index_add[!min_reject]
  names(res) <- choice[!min_reject]

  if (length(res) == 0) {
    stop("Argument 'min_duration' excludes all the explosive periods",
      call. = FALSE)
  }

  dummy <- matrix(0, length(index(x)), length(choice),
                    dimnames = list(index(x), if (panel) "Panel" else col_names(x)[reps]))
  for (z in seq_along(choice)) {
    dvec <- ds_stamp[[z]]
    dummy[dvec, z] <- 1
  }


  structure(res,
            bool = dummy,
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
      "------------------------------\n")
      print(x[[1]])

  }else{

    cat(
      "\nDatestamp: Individual\n",
      "------------------------------\n"
    )
    print.listof(x)
  }

}

