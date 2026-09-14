`%NA%` <- function(x, y) {
  if (is.na(x)) {
    y
  } else {
    x
  }
}


`%NULL%` <- function(cond, x) {
  if (isTRUE(cond)) {
    x
  } else {
    NULL
  }
}

"%ni%" <- Negate("%in%")

# For simulation ----------------------------------------------------------

get_rng <- function(seed) {
  if (!exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
    runif(1)
  }
  if (is.null(seed)) {
    rng_state <- get(".Random.seed", envir = .GlobalEnv)
  } else {
    r_seed <- get(".Random.seed", envir = .GlobalEnv)
    rng_state <- structure(seed, kind = as.list(RNGkind()))
    on.exit(assign(".Random.seed", r_seed, envir = .GlobalEnv))
  }
  rng_state
}

get_global_rng <- function() {
  option_seed <- getOption("exuber.global_seed")
  if (!is.na(option_seed) && !is.null(option_seed)) {
    option_seed
  } else {
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
  lag <- get_lag(x) %||% 0

  if (nr <= 5 || nr > 4000 || lag > 4) {
    stop_glue(
      "Precomputed critical values cover 6 <= n <= 4000 and lag <= 4 (n = {nr}, lag = {lag}); ",
      "pass `cv` explicitly, e.g. `cv = radf_mc_cv({nr}, lag = {lag})`."
    )
  }
  cv <- fetch_crit_bucket(nr, lag = lag)
  if (is.null(cv)) {
    stop_glue(
      "Critical values for n = {nr}, lag = {lag} haven't been simulated ",
      "yet. Pass `cv` explicitly, e.g. `cv = radf_mc_cv({nr}, lag = {lag})`."
    )
  }
  message_glue("Using precomputed critical values for `cv`.")
  cv
}


# options -----------------------------------------------------------------

show_pb <- function() {
  isTRUE(getOption("exuber.show_progress")) &&
    interactive() &&
    !isTRUE(getOption("rstudio.notebook.executing")) &&
    !isTRUE(getOption("knitr.in.progress"))
}

#' @importFrom progressr with_progress handler_txtprogressbar
with_backend <- function(expr) {
  do_par <- getOption("exuber.parallel")
  oplan <- if (do_par) {
    future::plan(future::multisession, workers = getOption("exuber.ncores"))
  } else {
    future::plan(future::sequential)
  }
  on.exit(future::plan(oplan), add = TRUE)
  with_progress(expr, enable = show_pb(), handlers = handler_txtprogressbar())
}

# tidy --------------------------------------------------------------------

array_to_list <- function(x, var) {
  itnames <- pluck(x, var) %>%
    dimnames() %>%
    pluck(3)
  iter <- length(itnames)

  out <- vector("list", length = iter)
  for (i in 1:iter) {
    out[[i]] <- pluck(x, var)[, , i]
  }
  out
}

#' @importFrom tibble add_column tibble
add_key <- function(x, attr_from, trunc = FALSE) {
  nkey <- get_trunc(attr_from)
  if (trunc) {
    key_tbl <- tibble(key = (nkey + 1):(nrow(x) + nkey))
    wkey <- add_column(x, key_tbl)
  } else {
    key_tbl <- tibble(key = 1:nrow(x))
    wkey <- add_column(x, key_tbl)
  }
  wkey
}

na_pad_minw <- function(x, attr_from) {
  trunc <- get_trunc(attr_from)
  cx <- x[1:trunc, ]
  cx[1:trunc, ] <- NA
  bind_rows(cx, x)
}

add_index <- function(x, attr_from, trunc = FALSE) {
  idx <- index(attr_from, trunc = trunc)
  idx_tbl <- tibble(index = idx)
  add_column(x, idx_tbl)
}


# predicates --------------------------------------------------------------

#' @importFrom rlang %||%
is_mc <- function(y) {
  inherits(y, "mc_cv")
}

is_wb <- function(y) {
  get_method(y) %||% FALSE == "Wild Bootstrap"
}

is_sb <- function(y) {
  get_method(y) %||% FALSE == "Sieve Bootstrap"
}
