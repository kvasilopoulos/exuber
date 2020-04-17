
`%NA%` <- function(x, y) {
  if (is.na(x))
    y
  else x
}

"%ni%" <- Negate("%in%")

# For simulation ----------------------------------------------------------

get_rng <- function(seed) {
  if (!exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE))
    runif(1)
  if (is.null(seed)) {
    RNGstate <- get(".Random.seed", envir = .GlobalEnv)
  }else {
    R.seed <- get(".Random.seed", envir = .GlobalEnv)
    RNGstate <- structure(seed, kind = as.list(RNGkind()))
    on.exit(assign(".Random.seed", R.seed, envir = .GlobalEnv))
  }
  RNGstate
}

get_global_rng <- function() {
  option_seed <- getOption("exuber.global_seed")
  if (!is.na(option_seed) && !is.null(option_seed)) {
    option_seed
  }else{
    NULL
  }
}

set_rng <- function(seed) {
  super <- seed %||% get_global_rng() # local supersedes global
  rng_state <- super %||% get_rng(seed)
  if (!is.null(super)) {
    set.seed(super)
  }
  rng_state
}

get_rng_state <- function(seed) {
  seed %||%
    get_global_rng() %||%
    get_rng(seed)
}

# get crit data --------------------------------------------------------

retrieve_crit <- function(x) {
  nr <- NROW(index(x))
  if (nr > 5 && nr <= length(exuber::radf_crit)) {
    message("Using 'radf_crit' as 'cv'argument..")
    return(exuber::radf_crit[[nr]])
  } else if (nr > length(exuber::radf_crit) && nr <= 2000) {
    message("Using 'radf_crit2' as 'cv'argument..")
    need_exuberdata()
    return(exuberdata::radf_crit2[[nr]])
  }else {
    stop_glue("Cannot provide MC critical values see help(radf_crit).")
  }
}


# options -----------------------------------------------------------------

show_pb <- function() {
  isTRUE(getOption("exuber.show_progress")) &&
    interactive() &&
    !isTRUE(getOption("rstudio.notebook.executing")) &&
    !isTRUE(getOption("knitr.in.progress"))
}

set_pb <- function(iter, width = getOption("width") - 10L) {
  if (show_pb()) {
    txtProgressBar(min = 1, max = iter - 1, style = 3, char = "-", width = width)
  }
}

set_pb_opts <- function(pb) {
  if (show_pb()) {
    list(progress = function(n) setTxtProgressBar(pb, n))
  }else{
    list(progress = NULL)
  }
}

# tidy --------------------------------------------------------------------

array_to_list <- function(x, var) {
  itnames <- pluck(x, var) %>%
    dimnames() %>% pluck(3)
  iter <- length(itnames)

  out <- vector("list", length = iter)
  for (i in 1:iter) {
    out[[i]] <- pluck(x, var)[, , i]
  }
  out
}

#' @importFrom tibble add_column
add_key <- function(x, attr_from) {
  attr_lag <-  get_lag(attr_from) #else 0
  if (is.null(attr_lag)) {
    add_lag <- 0
  } else{
    if (is_sb(attr_from) && attr_lag != 0) {
      add_lag <- attr_lag + 2
    }else{
      add_lag <- attr_lag
    }
  }
  nkey <- get_minw(attr_from) + add_lag
  x %>% add_column(key = (nkey + 1):(nrow(.) + nkey))
}

# predicates --------------------------------------------------------------

#' @importFrom rlang %||%
is_mc <- function(y) {
  get_method(y) %||% FALSE == "Monte Carlo"
}

is_wb <- function(y) {
  get_method(y) %||% FALSE == "Wild Bootstrap"
}

is_sb <- function(y) {
  get_method(y) %||% FALSE == "Sieve Bootstrap"
}


# seq ---------------------------------------------------------------------

extract_cv <- function(y, which = "bsadf_cv", lg = 0) {

  if (is_sb(y)) {
    stop_glue("cannot extract from `sb_cv()`")
  }
  out <- pluck(y, which)
  if (lg != 0) {
    if (is_wb(y)) {
      out <- out[-c(1:lg), , ]
    }else{
      out <- out[-c(1:lg), ]
    }
  }
  if (is_wb(y)) {
    out <- out[, 2, ]
  }else{
    out <- out[, 2]
  }
  out
}





# printing ----------------------------------------------------------------

# cat_line <- function(...) {
#   glue::glue_collapse(..., sep  = "")
# }

# width <- getOption("width")
# line <- paste(rep("-", width), collapse = "")
#
# rule <- function(left = "", right = "") {
#   res <- if (nchar(left) && nchar(right)) {
#     paste0()
#   }else if (nchar(left)) {
#
#   }else{
#     cat(line)
#   }
#   res
# }

