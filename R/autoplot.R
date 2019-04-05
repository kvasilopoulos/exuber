#' Plotting and tidying radf objects
#'
#'
#' \code{autoplot.radf} takes an \code{radf} object and returns a (list of ) ggplot2 objects.
#' \code{fortify.radf} takes an \code{radf} object and converts it into a data.frame.
#' \code{ggarrange} is a wrapper of \code{\link[=gridExtra]{arrangeGrob()}}, which can be
#' used directly after autoplot to place grobs on a page.
#'
#' @name autoplot.radf
#'
#' @inheritParams datestamp
#'
#' @param include If not FALSE, plot all variables regardless of rejecting the NULL at the 5\% significance level.
#' @param select If not NULL, only plot with names or column number matching this regular expression will be executed.
#' @param ... further arguments passed to method, ignored.
#'
#' @importFrom dplyr filter
#' @importFrom purrr map pluck
#'
#' @export
#' @examples
#' \donttest{
#' dta <- cbind(sim_dgp1(n = 100), sim_dgp2(n = 100))
#'
#' dta %>%
#'   radf() %>%
#'   autoplot() %>%
#'   ggarrange(ncol = 2)
#'
#' # For custom plotting with ggplot2
#' dta %>%
#'   radf() %>%
#'   fortify()
#' }
autoplot.radf <- function(object, cv, include = FALSE, select = NULL,
                          option = c("gsadf", "sadf"),
                          min_duration = 0, ...) {

  cv <- if (missing(cv)) get_crit(object) else cv
  assert_class(cv, "cv")
  assert_positive_int(min_duration, strictly = FALSE)
  option <- match.arg(option)
  assert_equal_arg(object, cv)

  x <- object
  y <- cv

  if (include) {
    if (missing(select)) {
      cname2 <- fortify.radf(x, cv = y, include = include, option = option)
    }else{
      cname2 <- fortify.radf(x, cv = y, option = option,
                            select = select,  include = include)
    }
  }else{
    if (missing(select)) {
      cname2 <- fortify.radf(x, cv = y, option = option)
      }else{
      cname2 <- fortify.radf(x, cv = y, option = option, select = select)
    }
  }

  cname <- if (is_panel(y)) "Panel" else cname2 %>% attr("select")

  g <- vector("list", length = length(cname))

  for (i in seq_along(cname))
    local({
      i <- i
      suppressWarnings(
      dat <- fortify.radf(x, cv = y, include = include, option = option,
                          select = if (is_panel(y)) cname else cname[i]))

      h <- ggplot(dat) +
        geom_line(aes_string(x = "index",
                             y = as.name(colnames(dat)[2])),
                  size = 0.7,
                  colour = "blue") +
        geom_line(aes_string(x = "index",
                             y = as.name(colnames(dat)[3])),
                  size = 0.8,
                  colour = "red",
                  linetype = "dashed") +
        ggtitle(cname[i]) +
        theme_bw() + xlab("") + ylab("")

      shade <-
        if (include) {
          tryCatch(
            datestamp(x, y, option = option,
                      min_duration = min_duration) %>%
              pluck(cname[i]), error =  function(e){})
        }else{
          datestamp(x, y, option = option,
                    min_duration = min_duration) %>%
            pluck(cname[i])
        }



      if (!is.null(shade)) {

       h <- h + geom_rect(data = shade[, -3],
                          fill = "grey",
                          alpha = 0.55, #0.25
                  aes_string(xmin = "Start",
                             xmax = "End",
                             ymin = -Inf,
                             ymax = +Inf))
      }

      g[[i]] <<- h
    })
  names(g) <- cname
  if (length(g) == 1) return(g[[1]]) else return(g)
}


#' @rdname autoplot.radf
#' @inheritParams datestamp
#' @param model An object of class \code{\link[=radf]{radf()}}.
#' @param data original dataset, not used (required by generic
#' \code{\link[=fortify]{fortify()}} method).
#'
#' @importFrom purrr map pluck
#' @importFrom tibble as_tibble
#'
#' @export
fortify.radf <- function(model, data, cv, include = FALSE, select = NULL,
                         option = c("gsadf", "sadf"), ...) {

  cv <- if (missing(cv)) get_crit(model) else cv
  assert_class(cv, "cv")
  option <- match.arg(option)

  x <- model
  y <- cv
  assert_equal_arg(x, y)
  dating <- index(x, trunc = TRUE)

  if (is_panel(y)) {
    nm <- diagnostics(object = x, cv = y, option = option) %>%
      pluck("accepted")
    cname <- "Panel"
    if (!missing(select))
      warning("argument 'select' is redundant", call. = FALSE)
    if (!missing(include) && !is.null(nm))
      warning("argument 'include' is redundant", call. = FALSE)

  }else{

    if (include) {
      nm <- col_names(x)
      if (is.null(select)) {
        cname <- select <- nm
      }else{
        cname <- if (is.character(select)) select else nm[select]
      }
    }else{
      nm <- diagnostics(object = x, cv = y, option = option) %>%
        pluck("accepted")
      if (is.null(select)) {
        cname <- select <- nm
      }else{
        if (is.character(select)) {
          if (select %ni% nm) stop("subscript out of bounds", call. = FALSE)
          cname <- select
        }else{
          cname <- nm[select]
        }
      }
    }
  }

  if (option == "gsadf") {

    tstat_dat <- if (is_panel(y)) x$bsadf_panel else x$bsadf[, cname]

    if (method(y) == "Wild Bootstrap") {
      cv_dat <- if (lagr(x) == 0) {
        y$bsadf_cv[, 2, cname]
      }else{
        y$bsadf_cv[-c(1:lagr(x)), 2, select]
      }
      names_cv <- paste0("cv_", cname)
    }else if (method(y) == "Monte Carlo") {
      cv_dat <- if (lagr(x) == 0) {
        y$bsadf_cv[, 2]
      }else{
        y$bsadf_cv[-c(1:lagr(x)), 2]
      }
      names_cv <- "cv"
    }else if (method(y) == "Sieve Bootstrap") {
      cv_dat <- y$bsadf_panel_cv[, 2]
      if (lagr(cv) > 0) {
        dating <- dating[-c(1:2)]
        tstat_dat <- tstat_dat[-c(1:2)]
      }
      names_cv <- "cv_panel"
    }
  } else if (option == "sadf") {
    tstat_dat <- x$badf[, cname]
    cv_dat <- if (lagr(x) == 0) y$badf_cv[, 2] else y$badf_cv[-c(1:lagr(x)), 2]
    names_cv <- "cv"
  }

  dat <- data.frame(dating, tstat_dat, cv_dat) %>%
    set_names(c("index", cname, names_cv)) %>%
    as_tibble()
  attr(dat, "select") <- cname

  dat

}

#' @rdname autoplot.radf
#' @import ggplot2
#' @importFrom gridExtra arrangeGrob
#' @export
ggarrange <- function(...) {
  p <- do.call(gridExtra::arrangeGrob, c(...))
  class(p) <- c("ggarrange", class(p))
  p
}

#' Print a ggarrange object
#'
#' \code{ggarrange} is a wrapper around gridExtra::arrangeGrob
#'
#' @param x autoplot.radf() object.
#' @param newpage Should a new page (i.e., an empty page) be drawn before the
#' ggExtraPlot is drawn?
#' @param ... ignored
#'
#' @importFrom grid grid.newpage grid.draw
#' @keywords internal
#' @export
print.ggarrange <- function(x, newpage = grDevices::dev.interactive(), ...) {
  if (newpage) grid::grid.newpage()
  grid::grid.draw(x)
}

#' Plotting and tidying datestamp objects
#'
#' Plotting datestamp with \link[=ggplot2]{geom_segment()}
#'
#' @name autoplot.datestamp
#' @param object An object of class \code{\link[=datestamp]{datestamp()}}
#' @import ggplot2
#' @export
#' @examples
#' \donttest{
#'
#' dta <- cbind(sim_dgp1(n = 100), sim_dgp2(n = 100))
#'
#' dta %>%
#'   radf() %>%
#'   datestamp() %>%
#'   autoplot()
#'
#' # Change the colour manually
#' dta %>%
#'   radf() %>%
#'   datestamp() %>%
#'   autoplot() +
#'   ggplot2::scale_colour_manual(values=rep("black", 4 ))
#'
#'
#' }
autoplot.datestamp <- function(object, ...) {

  dating <- index(object)
  scale <- if (lubridate::is.Date(dating)) scale_x_date else scale_x_continuous

  ggplot(object, aes_string(colour = "key")) +
    geom_segment(aes_string(x = "Start",
                            xend = "End",
                            y = "key",
                            yend = "key"),
                 size = 7) +
    theme_bw() + xlab("") + ylab("") + ggtitle("") +
    scale(limits = c(dating[1L], dating[length(dating)])) +
    theme(panel.grid.major.y = element_blank(),
          legend.position = "none",
          plot.margin = margin(0.5, 1, 0, 0, "cm"),
          axis.text.y = element_text(face = "bold", size = 8, hjust = 0))

}

#' @rdname autoplot.datestamp
#' @param model datestamp object
#' @inheritParams autoplot.radf
#'
#' @importFrom purrr map reduce
#' @export
fortify.datestamp <- function(model, data, ...) {
  nr <- map(model, NROW) %>%
    unlist()
  df <- data.frame("key" = rep(names(model), nr),
                   reduce(model, rbind))
  class(df) <- c("data.frame", "datestamp")
  df
}
