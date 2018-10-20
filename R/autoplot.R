

#' Plotting radf with ggplot2
#'
#' Plotting \code{\link[=radf]{radf()}} with ggplot2
#'
#' @inheritParams datestamp
#' @param ... further arguements passed to method, not used
#'
#' @name autoplot.radf
#' @importFrom dplyr filter
#' @importFrom purrr map pluck
#' @export
autoplot.radf <- function(object, cv, select,
                          option = c("gsadf", "sadf"),
                          min_duration = NULL, ...) {

  cv <- if (missing(cv)) get_crit(object) else cv
  assert_class(cv, "cv")
  if (is.null(min_duration)) min_duration <- 0
  option <- match.arg(option)


  x <- object
  y <- cv
  assert_equal_arg(x, y)
  panel <- if (method(y) == "Sieve Bootstrap") TRUE else FALSE

  # plot only the series that reject null
  choice <- diagnostics(x, y, option = option) %>%
    pluck("accepted")

  if (missing(select)) {
    cname <- choice
  }else {
    cname <- if (is.character(select)) select else choice[select]

    ## write unit test if reject first one
    if (all(cname %ni% col_names(x))) stop("subscript out of bounds",
                                           call. = FALSE)
    if (all(cname %ni% choice)) stop("cannot reject the null", call. = FALSE)
  }


  g <- vector("list", length = length(cname))

  for (i in seq_along(cname))
    local({
      i <- i
      # selected series in fortify (cname) can be only %in% choice
      dat <- fortify.radf(x, cv = y, select = if (panel) NULL else cname[i],
                          option = option)
      shade <- datestamp(x, y, option = option, min_duration = min_duration) %>%
        pluck(cname[i])

      h <- ggplot(dat) +
        geom_line(aes_string(x = "index", y = as.name(colnames(dat)[2])),
                  size = 0.7, colour = "blue") +
        geom_line(aes_string(x = "index", y = as.name(colnames(dat)[3])),
                  colour = "red", size = 0.8, linetype = "dashed") +
        xlab("") + ylab("") +
        theme_bw() +
        # theme(
        #   axis.line = element_line(colour = "black"),
        #   # panel.grid.major.x = element_blank(),
        #   panel.grid.minor = element_blank(),
        #   panel.background = element_blank()
        # ) +
        ggtitle(cname[i]) +
        geom_rect(data = shade[, -3], fill = "grey", alpha = 0.35, #0.25
                  aes_string(xmin = "Start", xmax = "End",
                             ymin = -Inf, ymax = +Inf))
      g[[i]] <<- h
    })
  names(g) <- cname
  if (length(g) == 1) return(g[[1]]) else return(g)
}

#' @inheritParams datestamp
#' @param select keeps only the variables you mention
#' @param model An object of class \code{\link[=radf]{radf()}}.
#' @param data original dataset, not used.
#'
#' @importFrom purrr map pluck
#'
#' @rdname autoplot.radf
#' @export
fortify.radf <- function(model, data , cv, select,
                         option = c("gsadf", "sadf"), ...) {


  cv <- if (missing(cv)) get_crit(model) else cv
  assert_class(cv, "cv")
  if (missing(select)) select <- diagnostics(model, cv, option) %>%
    pluck("accepted")
  option <- match.arg(option)

  x <- model
  y <- cv
  assert_equal_arg(x, y)
  panel <-  if (method(y) == "Sieve Bootstrap") TRUE else FALSE
  dating <- index(x)[-c(1:(minw(x) + lagr(x)))]

  choice <- if (panel) "Panel" else col_names(x)
  cname <-  if (is.character(select)) select else choice[select]


  names_tstat <- paste0("tstat_", tolower(cname))


  if (option == "gsadf") {

    tstat_dat <- if (panel) x$bsadf_panel else x$bsadf[, cname]

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
      cv_dat <- if (lagr(x) == 0) {
        y$bsadf_panel_cv[, 2]
      }else{
        y$bsadf_panel_cv[-c(1:lagr(x)), 2]
      }
      names_cv <- "cv_panel"
    }
  } else if (option == "sadf") {

    tstat_dat <- x$badf[, cname]
    #rep(y$adf_cv[2], NROW(x$badf))
    cv_dat <- if (lagr(x) == 0) {
      y$badf_cv[, 2]
    }else{
      y$badf_cv[-c(1:lagr(x)), 2]
    }
    names_cv <- "cv"
  }

  dat <- data.frame(dating, tstat_dat, cv_dat)
  colnames(dat) <- c("index", names_tstat, names_cv)
  attr(dat, "select") <- cname
  return(dat)

}

#' @import ggplot2
#' @importFrom gridExtra grid.arrange
#' @rdname autoplot.radf
#' @param ncol number of columns to arrange
#' @export
ggarrange <- function(..., ncol = 2) {
  do.call(gridExtra::grid.arrange, c(..., ncol = ncol))
}


#' Plotting datestamp objects with ggplot2
#'
#' Plotting datestamp with ggplot2
#'
#' @param object An object of class \code{\link[=datestamp]{datestamp()}}
#' @import ggplot2
#' @export
autoplot.datestamp <- function(object, ...) {
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
  model <- model[-length(model)] # get rid of bool
  nr <- map(model, NROW) %>%
    unlist()
  df <- data.frame("key" = rep(names(model), nr),
                   reduce(model, rbind))
  class(df) <- c("data.frame", "datestamp")
  df
}


