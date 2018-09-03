#' @importFrom ggplot2 fortify
#' @seealso \code{\link[=fortify.radf]{fortify.radf()}}
#' @export
ggplot2::fortify

#' @importFrom ggplot2 autoplot
#' @seealso \code{\link[=autoplot.radf]{autoplot.radf()}}
#' @export
ggplot2::autoplot

#' Plotting radf with ggplot2
#'
#' Plotting \code{\link[=radf]{radf()}} with ggplot2
#'
#' @inheritParams datestamp
#' @param ... further arguements passed to method, not used
#'
#' @name autoplot.radf
#' @importFrom dplyr filter
#' @importFrom purrr map
#' @export
autoplot.radf <- function(object, cv, select, min_duration,
                          option = c("gsadf", "sadf"),
                          panel = FALSE, ...) {
  # args
  x <- object
  y <- provide_crit(cv, x)
  if (missing(min_duration)) min_duration <- 1
  option <- match.arg(option)

  # checks
  assert_class(y, cv)
  assert_panel(x, y, panel = panel)
  assert_equal_minw(x, y)

  # plot only the series that reject null
  choice <- diagnostics(x, y, option = option, panel = panel) %>%
    with(get("accepted"))
  if (missing(select)) {
    cname <- choice[1]
  }else {
    cname <- if (is.character(select)) select else choice[select]
    ## write unit test if reject first one
    if (all(cname %ni% col_names(x)))
      stop("subscript out of bounds", call. = FALSE)
    if (all((cname %ni% choice))) stop("cannot reject the null", call. = FALSE)
  }

  g <- vector("list", length = length(cname))
  for (i in seq_along(cname))
    local({
      i <- i
      # selected series in fortify (cname) can be only %in% choice
      dat <- fortify.radf(x, y, select = cname[i], option = option)
      shade <- datestamp(x, y, option = option, min_duration = min_duration,
                         panel = panel) %>% with(get(cname[i]))
      h <- ggplot(dat) +
        geom_line(aes_string(x = "date", y = colnames(dat)[2]),
                  size = 0.7, colour = "blue") +
        geom_line(aes_string(x = "date", y = colnames(dat)[3]),
                  colour = "red", size = 0.8, linetype = "dashed") +
        xlab("") + ylab("") + theme_bw() +
        theme(axis.line = element_line(colour = "black"),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              panel.background = element_blank()) +
        ggtitle(cname[i]) +
        geom_rect(data = shade[,-3], fill = "grey", alpha = 0.25,
                  aes_string(xmin = "Start", xmax = "End",
                             ymin = -Inf, ymax = +Inf))
      g[[i]] <<- h
    })
  names(g) <- cname
  if (length(g) == 1) return(g[[1]]) else return(g)
}

#' @inheritParams datestamp
#' @param select choose the asdfa
#' @param model radf object
#' @param data data set, not used
#'
#' @importFrom purrr map
#'
#' @rdname autoplot.radf
#' @export
fortify.radf <- function(model, data , cv, select, option = c("gsadf", "sadf"),
                         panel = FALSE, ...) {
  # args
  x <- model
  y <- provide_crit(cv, x)
  if (missing(select)) select <- 1
  option <- match.arg(option)

  # checks
  assert_class(y, cv)
  assert_panel(x, y, panel = panel)
  assert_equal_minw(x, y)

  dating <- index(x)[-c(1:(minw(x) + lagr(x)))]
  choice <- if (panel) "Panel" else col_names(x)
  cname <-  if (is.character(select)) select else choice[select]
  tstat_names <- paste0("tstat_", choice[select])
  names_cv <- paste0("cv_", choice[select])

  if (option == "gsadf") {
    tstat_dat <- if (panel) x$bsadf_panel else x$bsadf[, select]

    if (method(y) == "Wild Bootstrap") {
      cv_dat <- if (lagr(x) == 0) {
        y$badf_cv[, 2, select]
      }else{
        y$badf_cv[-c(1:lagr(x)), 2, select]
      }
    }else if (method(y) %in% c("Monte Carlo", "Sieve Bootstrap")) {
      cv_dat <- if (lagr(x) == 0) {
        y$badf_cv[, 2]
      }else{
        y$badf_cv[-c(1:lagr(x)), 2]
      }
    }
  } else if (option == "sadf") {
    tstat_dat <- x$badf[, select]
    cv_dat <- rep(y$adf_cv[2], NROW(x$badf))
  }
  dat <- data.frame(dating, tstat_dat, cv_dat)
  colnames(dat) <- c("date", tstat_names, names_cv)
  attr(dat, "select") <- cname
  return(dat)
}

#' @import ggplot2
#' @importFrom gridExtra grid.arrange
#' @rdname autoplot.radf
#' @param ncol number of columns to arrange
#' @export
garrange <- function(..., ncol = 2) {
  do.call(gridExtra::grid.arrange, c(..., ncol = ncol))
}


#' Plotting datestamp objects with ggplot2
#'
#' Plotting \code{\link[=datestamp]{datestamp()}} with ggplot2
#'
#' @param object An object of class datestamp
#' @import ggplot2
#' @export
autoplot.datestamp <- function(object, ...) {
  if (!inherits(object, "datestamp")) object <- fortify.datestamp(object)
  ggplot(object, aes_string(colour = "key")) +
    geom_segment(aes_string(x = "Start", xend = "End",
                            y = "key", yend = "key"), size = 7) +
    ylab("") + xlab("") + theme_bw() +
    theme(panel.grid.major.y = element_blank(),
          legend.position = "none",
          plot.margin = margin(1, 1, 0, 0, "cm"),
          axis.text.y = element_text(face = "bold", size = 8, hjust = 0))
}

#' @param model datestamp object
#' @param data data set, defaults to data used to estimated model
#' @param ... not used by this method
#'
#' @rdname autoplot.datestamp
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

# gbreaks_x <- function(..., breaks) {
#  purrr::map(..., ~.x + ggplot2::scale_x_continuous(breaks = seq(1, 100, breaks)))
# }
#
# gtitle <- function(..., names) {
#   names <- list(names)
#   map2(..., aa, ~.x + ggtitle(.y))
# }

