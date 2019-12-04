#' Diagnostics
#'
#' Finds the series that reject the null for at the 5\% significance level.
#'
#' @inheritParams summary
#' @param ... further arguments passed to methods.
#' "gsadf".
#' @return Returns a list with the series that reject and the series that do not reject the
#' Null Hypothesis
#' @details
#' Diagnostics also stores a vector in {0,1} that corresponds to {reject, accept} respectively.
#'
#' @export
diagnostics <- function(object, cv = NULL, ...) {
  UseMethod("diagnostics")
}

#' @importFrom dplyr case_when
#' @inheritParams diagnostics
#' @param option Whether to apply the "gsadf" or "sadf" methodology. Default is
#' @export
diagnostics.radf <- function(object, cv = NULL,
                             option = c("gsadf", "sadf"), ...) {

  assert_class(object, "radf")
  cv <- cv %||% retrieve_crit(object)
  assert_class(cv, "cv")
  assert_match(object, cv)
  option <- match.arg(option)

  x <- object
  y <- cv

  if (option == "gsadf") {
    tstat <- if (is_sb(y)) x$gsadf_panel else x$gsadf

    if (is_mc(y)) {
      cv1 <- y$gsadf_cv[1]
      cv2 <- y$gsadf_cv[2]
      cv3 <- y$gsadf_cv[3]
    } else if (is_wb(y)) {
      cv1 <- y$gsadf_cv[, 1]
      cv2 <- y$gsadf_cv[, 2]
      cv3 <- y$gsadf_cv[, 3]
    } else if (is_sb(y)) {
      cv1 <- y$gsadf_panel_cv[1]
      cv2 <- y$gsadf_panel_cv[2]
      cv3 <- y$gsadf_panel_cv[3]
    }
  } else if (option == "sadf") {

    if (is_sb(y)) {
      stop_glue("'sadf' does not apply for sieve bootstrapped critical values")
    }
    tstat <- x$sadf

    if (is_mc(y)) {
      cv1 <- y$sadf_cv[1]
      cv2 <- y$sadf_cv[2]
      cv3 <- y$sadf_cv[3]
    } else if (is_wb(y)) {
      cv1 <- y$sadf_cv[, 1]
      cv2 <- y$sadf_cv[, 2]
      cv3 <- y$sadf_cv[, 3]
    }
  }

  # in case of simulation exercises
  dummy <- case_when(
    tstat < cv2 ~ 0,
    tstat >= cv2 ~ 1
  )

  sig <- case_when(
    tstat < cv1 ~ "Reject",
    tstat >= cv1 & tstat < cv2 ~ "90%",
    tstat >= cv2 & tstat < cv3 ~ "95%",
    tstat >= cv3 ~ "99%"
  )

  if (is_sb(y)) {
    accepted <- ifelse(length(dummy), "Panel", NA)
    rejected <- ifelse(length(dummy), NA, "Panel")
  } else {
    accepted <- series_names(x)[as.logical(dummy)]
    rejected <- series_names(x)[!as.logical(dummy)]
  }

  structure(
    list(
      accepted = accepted,
      rejected = rejected,
      sig = sig,
      dummy = dummy
    ),
    panel = is_sb(y),
    series_names = if (!is_sb(y)) series_names(x),
    method = get_method(y),
    option = option,
    class = "diagnostics"
  )
}

#' @importFrom cli cat_line cat_rule
#' @importFrom glue glue
#' @importFrom rlang is_bare_character
#' @export
print.diagnostics <- function(x, ...) {

  if (all(x$dummy == 0)) {
    return(message_glue("Cannot reject H0 for significance level 95%"))
  }
  if (purrr::is_bare_character(x$accepted, n = 0)) {
    return(message_glue("Cannot reject H0"))
  }

  cli::cat_line()
  cli::cat_rule(left = glue('Diagnostics (option = {attr(x, "option")})'),
                right = get_method(x))
  cli::cat_line()

  if (attr(x, "panel")) {

    if (x$sig == "Reject")
      cat(" Cannot rejeact H0! \n")
    else
      cat(" Rejects H0 for significance level", x$sig, "\n")

  } else {

    width <- nchar(series_names(x))
    ngaps <- max(8, width) - width
    for (i in seq_along(series_names(x))) {
      cat(series_names(x)[i], ":" , rep(" ", ngaps[i]), sep = "")
      if (x$sig[i] == "Reject")
        cat(" Cannot reject H0! \n")
      else
        cat(" Rejects H0 for significance level", x$sig[i], "\n")
    }
  }
  cli::cat_line()
}

