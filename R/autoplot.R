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
#' @param arrange If FALSE returns a list of ggplot2 object, otheriwse it grobs the plots on a single page.
#' @param ... further arguments passed to method. Specify common characteristics like `ggplot2::xlab`,
#' that are later paseed to ggplot chain. For multiple changes, the input in the arguement should be in
#' a list.
#'
#' @importFrom dplyr filter
#' @importFrom purrr map pluck
#' @importFrom ggplot2 ggplot aes_string geom_line %+% ggtitle labs geom_rect
#' @importFrom ggplot2 geom_segment theme_bw theme margin element_blank
#' @importFrom ggplot2 element_text scale_x_date scale_x_continuous
#'
#' @details `arrange` offers flexibility to the user by specifying the desired output. If
#' `arrange = FALSE`, the individual plots can be modified after creation the then rearranged with
#' the `ggarrange` function into a single plot.
#'
#' @export
#' @examples
#' \donttest{
#' dta <- data.frame(psy1 = sim_psy1(n = 100), psy2 = sim_psy2(n = 100))
#'
#' dta %>%
#'   radf() %>%
#'   autoplot(ncol = 2)
#'
#'
#' # For custom plotting with ggplot2
#' dta %>%
#'   radf() %>%
#'   fortify()
#' }
autoplot.radf <- function(object, cv, include = FALSE, select = NULL,
                          option = c("gsadf", "sadf"),
                          min_duration = 0, arrange = TRUE, ...) {

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
    } else {
      cname2 <- fortify.radf(x,
        cv = y, option = option,
        select = select, include = include
      )
    }
  } else {
    if (missing(select)) {
      cname2 <- fortify.radf(x, cv = y, option = option)
    } else {
      cname2 <- fortify.radf(x, cv = y, option = option, select = select)
    }
  }

  cname <- if (is_panel_cv(y)) "Panel" else cname2 %>% attr("select")

  g <- vector("list", length = length(cname))

  for (i in seq_along(cname))
    local({
      i <- i
      suppressWarnings(
        dat <- fortify.radf(x,
          cv = y, include = include, option = option,
          select = if (is_panel_cv(y)) cname else cname[i]
        )
      )

      h <- ggplot(dat) +
        geom_line(aes_string(
          x = "index",
          y = as.name(colnames(dat)[2])
        ),
        size = 0.7,
        colour = "blue"
        ) +
        geom_line(aes_string(
          x = "index",
          y = as.name(colnames(dat)[3])
        ),
        size = 0.8,
        colour = "red",
        linetype = "dashed"
        ) +
        theme_bw() + labs(x = "", y = "", title = cname[i])

      shade <-
        if (include) {
          tryCatch(
            datestamp(x, y,
              option = option,
              min_duration = min_duration
            ) %>%
              pluck(cname[i]),
            error = function(e) {}
          )
        } else {
          datestamp(x, y,
            option = option,
            min_duration = min_duration
          ) %>%
            pluck(cname[i])
        }

      if (!is.null(shade)) {
        h <- h + geom_rect(
          data = shade[, -3],
          fill = "grey",
          alpha = 0.55, # 0.25
          aes_string(
            xmin = "Start",
            xmax = "End",
            ymin = -Inf,
            ymax = +Inf
          )
        )
      }

      g[[i]] <<- h
    })
  names(g) <- cname
  out <-
    if (length(g) == 1) {
      g[[1]]
    } else {
      if (arrange)
        ggarrange(g, ...)
      else
        g
    }
  out
}


#' @rdname autoplot.radf
#' @inheritParams datestamp
#' @param model An object of class \code{\link[=radf]{radf()}}.
#' @param data original dataset, not used (required by generic
#' \code{\link[=fortify]{fortify()}} method).
#'
#' @importFrom purrr map pluck
#' @importFrom dplyr as_tibble
#'
#' @export
fortify.radf <- function(model, data, cv = get_crit(model),
                         include = FALSE, select = NULL,
                         option = c("gsadf", "sadf"), ...) {

  assert_class(cv, "cv")
  option <- match.arg(option)

  x <- model
  y <- cv
  assert_equal_arg(x, y)
  dating <- index(x, trunc = TRUE)

  if (is_panel_cv(y)) {
    nm <- diagnostics(object = x, cv = y, option = option) %>%
      pluck("accepted")
    cname <- "Panel"
    if (!missing(select)) {
      warning("argument 'select' is redundant", call. = FALSE)
    }
    if (!missing(include) && !is.null(nm)) {
      warning("argument 'include' is redundant", call. = FALSE)
    }
  } else {
    if (include) {
      nm <- col_names(x)
      if (is.null(select)) {
        cname <- select <- nm
      } else {
        cname <- if (is.character(select)) select else nm[select]
      }
    } else {
      nm <- diagnostics(object = x, cv = y, option = option) %>%
        pluck("accepted")
      if (is.null(select)) {
        cname <- select <- nm
      } else {
        if (is.character(select)) {
          if (select %ni% nm) stop("subscript out of bounds", call. = FALSE)
          cname <- select
        } else {
          cname <- nm[select]
        }
      }
    }
  }

  if (option == "gsadf") {
    tbl_stat <- if (is_panel_cv(y)) x$bsadf_panel else x$bsadf[, cname]

    if (method(y) == "Wild Bootstrap") {
      tbl_cv <- if (lagr(x) == 0) {
        y$bsadf_cv[, 2, cname]
      } else {
        y$bsadf_cv[-c(1:lagr(x)), 2, select]
      }
      names_cv <- paste0("cv_", cname)
    } else if (method(y) == "Monte Carlo") {
      tbl_cv <- if (lagr(x) == 0) {
        y$bsadf_cv[, 2]
      } else {
        y$bsadf_cv[-c(1:lagr(x)), 2]
      }
      names_cv <- "cv"
    } else if (method(y) == "Sieve Bootstrap") {
      tbl_cv <- y$bsadf_panel_cv[, 2]
      if (lagr(cv) > 0) {
        dating <- dating[-c(1:2)]
        tbl_stat <- tbl_stat[-c(1:2)]
      }
      names_cv <- "cv_panel"
    }
  } else if (option == "sadf") {
    tbl_stat <- x$badf[, cname]
    tbl_cv <- if (lagr(x) == 0) y$badf_cv[, 2] else y$badf_cv[-c(1:lagr(x)), 2]
    names_cv <- "cv"
  }

  tbl_rf <- data.frame(dating, tbl_stat, tbl_cv) %>%
    set_names(c("index", cname, names_cv)) %>%
    as_tibble()

  attr(tbl_rf, "select") <- cname
  tbl_rf
}

#' @rdname autoplot.radf
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
#' @export
#' @examples
#' \donttest{
#'
#' dta <- cbind(sim_psy1(n = 100), sim_psy2(n = 100))
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
#'   ggplot2::scale_colour_manual(values = rep("black", 4))
#' }
autoplot.datestamp <- function(object, ...) {
  dating <- index(object)
  scale <- if (lubridate::is.Date(dating)) scale_x_date else scale_x_continuous

  ggplot(object, aes_string(colour = "id")) +
    geom_segment(aes_string(
      x = "Start",
      xend = "End",
      y = "id",
      yend = "id"
    ),
    size = 7
    ) +
    theme_bw() + labs(x = "", y = "", title = "") +
    scale(limits = c(dating[1L], dating[length(dating)])) +
    theme(
      panel.grid.major.y = element_blank(),
      legend.position = "none",
      plot.margin = margin(0.5, 1, 0, 0, "cm"),
      axis.text.y = element_text(face = "bold", size = 8, hjust = 0)
    )
}

#' @rdname autoplot.datestamp
#' @param model datestamp object
#' @inheritParams autoplot.radf
#'
#' @importFrom purrr map reduce
#' @importFrom tibble add_column
#' @export
fortify.datestamp <- function(model, data, ...) {
  nr <- map_dbl(model, nrow)

  tbl_ds <- reduce(model, bind_rows) %>%
    add_column("id" = rep(names(model), nr)) %>%
    mutate(id = as.factor(id)) %>%
    select(id, everything()) %>%
    as_tibble()

  class(tbl_ds) <- append(class(tbl_ds), "datestamp")
  tbl_ds
}
