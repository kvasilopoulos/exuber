#
#' @importFrom rlang is_scalar_atomic
#' @importFrom doFuture `%dofuture%`
#' @importFrom progressr progressor
radf_mc_ <- function(n, minw, nrep, seed = NULL, lag = 0) {

  if(!is.null(dim(n))) {
    message_glue("Did you use `data` instead of `n`? Using `NROW(n)` instead.")
    n <- NROW(n)
  }
  assert_n(n)
  assert_positive_int(n, greater_than = 5)
  assert_positive_int(nrep)
  minw <- minw %||% psy_minw(n)
  assert_positive_int(minw, greater_than = 2)
  assert_positive_int(lag, strictly = FALSE)

  do_par <- getOption("exuber.parallel")

  set_rng(seed)
  results <- with_backend({
    p <- progressor(steps = nrep)
    foreach(
      i = 1:nrep,
      .combine = "cbind",
      .options.future = list(seed = TRUE, globals = structure(TRUE, add = c("rls_gsadf", "unroot"))),
      .inorder = FALSE
    ) %dofuture% {
      p()
      y <- cumsum(rnorm(n))
      yxmat <- unroot(y, lag = lag)
      rls_gsadf(yxmat, min_win = minw, lag = lag)
    }
  })

  n_minw <- n - minw - lag

  adf_crit   <- results[n_minw + 1, ]
  sadf_crit  <- results[n_minw + 2, ]
  gsadf_crit <- results[n_minw + 3, ]

  badf_crit  <- results[1:n_minw, ]
  bsadf_crit <- results[-c(1:(n_minw + 3)), ]

  list(
    adf = adf_crit,
    sadf = sadf_crit,
    gsadf = gsadf_crit,
    badf = badf_crit,
    bsadf = bsadf_crit) %>%
    add_attr(
      index = 1:n,
      method = "Monte Carlo",
      n = n,
      minw = minw,
      iter = nrep,
      lag = lag,
      seed = get_rng_state(seed),
      parallel = do_par
    )

}

#'  Monte Carlo Critical Values
#'
#' \code{radf_mc_cv} computes Monte Carlo critical values for the recursive unit
#' root tests. \code{radf_mc_distr} computes the distribution.
#'
#' @inheritParams radf
#' @param n A positive integer. The sample size.
#' @param nrep A positive integer. The number of Monte Carlo simulations.
#' @param lag A non-negative integer. Number of lags in the auxiliary
#' regression, as in \code{\link{radf}}.
#' @param seed An object specifying if and how the random number generator (rng)
#' should be initialized. Either NULL or an integer will be used in a call to
#' `set.seed` before simulation. If set, the value is saved as "seed" attribute
#' of the returned value. The default, NULL, will not change rng state, and
#' return .Random.seed as the "seed" attribute. Results are reproducible
#' across the parallel and non-parallel option when the same seed is used.
#'
#' @return For \code{radf_mc_cv} a list that contains the critical values for ADF,
#' BADF, BSADF and GSADF test statistics. For \code{radf_mc_distr} a list that
#' contains the ADF, SADF and GSADF distributions.
#'
#' @seealso \code{\link{radf_wb_cv}} for wild bootstrap critical values and
#' \code{\link{radf_sb_cv}} for sieve bootstrap critical values
#'
#' @importFrom foreach foreach
#' @importFrom stats quantile rnorm runif
#' @importFrom lubridate is.Date
#' @importFrom purrr detect_index
#' @export
#'
#' @examples
#' \donttest{
#' # Default minimum window
#' mc <- radf_mc_cv(n = 100)
#'
#' tidy(mc)
#'
#' # Change the minimum window and the number of simulations
#' mc2 <- radf_mc_cv(n = 100, nrep = 600, minw = 20)
#'
#' tidy(mc2)
#'
#' mdist <- radf_mc_distr(n = 100, nrep = 1000)
#'
#' autoplot(mdist)
#' }
radf_mc_cv <- function(n, minw = NULL, nrep = 1000L, seed = NULL, lag = 0) {

  pcnt <- c(0.9, 0.95, 0.99)

  results <- radf_mc_(n, minw = minw, nrep = nrep, seed = seed, lag = lag)

  adf_crit <- quantile(results$adf, probs = pcnt, drop = FALSE)
  sadf_crit <- quantile(results$sadf, probs = pcnt, drop = FALSE)
  gsadf_crit <- quantile(results$gsadf, probs = pcnt, drop = FALSE)

  bsadf_crit <- apply(results$badf, 2, cummax) %>%
    apply(1, quantile, probs = pcnt) %>% t()
  # values taken from PWY
  asy_adf_crit <- rep(
    c(-0.44, -0.08, 0.6),
    each = nrow(bsadf_crit)
  )
  badf_crit <- matrix(
    asy_adf_crit, ncol = 3,
    dimnames = list(NULL, paste0(pcnt*100, "%"))
  )

  list(
    adf_cv = adf_crit,
    sadf_cv = sadf_crit,
    gsadf_cv = gsadf_crit,
    badf_cv = badf_crit,
    bsadf_cv = bsadf_crit
  ) %>%
    inherit_attrs(results) %>%
    add_class("radf_cv", "mc_cv")

}

#' @rdname radf_mc_cv
#' @inheritParams radf_mc_cv
#' @export
radf_mc_distr <- function(n, minw = NULL, nrep = 1000L, seed = NULL, lag = 0) {

  results <- radf_mc_(n, minw = minw, nrep = nrep, seed = seed, lag = lag)

  list(
    adf_distr = results$adf,
    sadf_distr = results$sadf,
    gsadf_distr = results$gsadf
  ) %>%
    inherit_attrs(results) %>%
    add_class("radf_distr", "mc_distr")

}
