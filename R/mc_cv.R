#' @importFrom rlang is_scalar_atomic
mc_ <- function(n, minw, nrep, seed) {

  if (!is_n(n)) { # case of providiing data in 'n'
    stop_glue("Argument 'n' should be a positive integer")
  }
  if (is.null(minw)) {
    minw <- psy_minw(n)
  }

  assert_positive_int(n, greater_than = 5)
  assert_positive_int(nrep)
  assert_positive_int(minw, greater_than = 2)

  rng_state <- set_rng(seed = seed)

  show_pb <- getOption("exuber.show_progress")
  pb <- set_pb(show_pb, nrep)
  opts <- set_pb_opts(show_pb, pb)

  do_par <- getOption("exuber.parallel")
  if (do_par) {
    cl <- parallel::makeCluster(getOption("exuber.ncores"), type = "PSOCK")
    registerDoSNOW(cl)
    on.exit(parallel::stopCluster(cl))
  }

  `%fun%` <- if (do_par) `%dopar%` else `%do%`

  results <- foreach(
    i = 1:nrep,
    .export = c("rls_gsadf", "unroot"),
    .combine = "cbind",
    .options.snow = opts,
    .inorder = FALSE
  ) %fun% {
    if (show_pb && !do_par)
      setTxtProgressBar(pb, i)
    y <- cumsum(rnorm(n))
    yxmat <- unroot(y)
    rls_gsadf(yxmat, min_win = minw)
  }

  point <- n - minw

  adf_crit   <- results[point + 1, ]
  sadf_crit  <- results[point + 2, ]
  gsadf_crit <- results[point + 3, ]

  badf_crit  <- results[1:point, ]
  bsadf_crit <- results[-c(1:(point + 3)), ]

  list(
    adf = adf_crit,
    sadf = sadf_crit,
    gsadf = gsadf_crit,
    badf = badf_crit,
    bsadf = bsadf_crit) %>%
    add_attr(
      seed = rng_state,
      method = "Monte Carlo",
      n = n,
      minw = minw,
      iter = nrep
    )

}

#'  Monte Carlo Critical Values
#'
#' \code{mc_cv} computes Monte Carlo critical values for the recursive unit
#' root tests. \code{mc_dist} computes the distribution.
#'
#' @inheritParams radf
#' @param n A positive integer. The sample size.
#' @param nrep A positive integer. The number of Monte Carlo simulations.
#' @param seed An object specifying if and how the random number generator(rng)
#' should be initialized. Either NULL or an integer will be used in a call to
#' `set.seed` before simulation. If set, the value is save as "seed" attribute
#' of the returned value. The default, NULL will note change the rng state, and
#' return .Random.seed as the "seed" attribute.
#' @param opt_bsadf Options for bsadf critical value calculation. "conventional"
#' corresponds to the max of the quantile of the simulated distribution, while
#' "conservative" corresponds to the quantile of the max which is more conservative
#' in nature, thus the name.
#' @param opt_badf Options for badf critical value calculation. "fixed" corresponds
#' to log(log(n*s))/100 rule, "asymptotic" to asymptotic critical values and
#' simulated to the monte carlo simulations.
#'
#' @return A list that contains the critical values for ADF, BADF, BSADF and GSADF
#' t-statistics.
#'
#' @seealso \code{\link{wb_cv}} for Wild Bootstrapped critical values and
#' \code{\link{sb_cv}} for Sieve Bootstrapped critical values
#'
#' @importFrom doSNOW registerDoSNOW
#' @importFrom parallel detectCores makeCluster stopCluster
#' @importFrom foreach foreach %dopar% %do%
#' @importFrom utils setTxtProgressBar txtProgressBar
#' @importFrom stats quantile rnorm runif
#' @importFrom lubridate is.Date
#' @importFrom purrr detect_index
#' @export
#'
#' @examples
#' \dontrun{
#' # Default minimum window
#' mc <- mc_cv(n = 100)
#'
#' # Change the minimum window and the number of simulations
#' mc <- mc_cv(n = 100, nrep = 2500, minw = 20)
#'
#' mdist <- mc_distr(n = 100)
#' autoplot(mdist)
#' }
mc_cv <- function(n, minw = NULL, nrep = 2000L, seed = NULL,
                  opt_badf = c("fixed", "asymptotic", "simulated"),
                  opt_bsadf = c("conservative", "conventional")) {

  opt_badf <- match.arg(opt_badf)
  opt_bsadf <- match.arg(opt_bsadf)

  pr <- c(0.9, 0.95, 0.99)

  results <- mc_(n, minw = minw, nrep = nrep, seed = seed)

  adf_crit <- quantile(results$adf, probs = pr, drop = FALSE)
  sadf_crit <- quantile(results$sadf, probs = pr, drop = FALSE)
  gsadf_crit <- quantile(results$gsadf, probs = pr, drop = FALSE)

  bsadf_crit <-
    if (opt_bsadf == "conventional") {
      apply(results$bsadf, 1, quantile, probs = pr) %>%
        t() %>% apply(2, cummax)
    } else if (opt_bsadf == "conservative") {
      apply(results$badf, 2, cummax) %>%
        apply(1, quantile, probs = pr) %>% t()
    }

  if (opt_badf == "fixed") {
    temp <- log(log(n * seq(get_minw(results) + 1, n))) / 100
    badf_crit <- matrix(
      rep(temp, 3), ncol = 3, dimnames = list(NULL, paste(pr))
    )
  } else if (opt_badf == "asymptotic") {
    temp <- c(-0.44, -0.08, 0.6) %>% rep(each = 100) # values taken from PWY
    badf_crit <- matrix(temp, ncol = 3, dimnames = list(NULL, paste(pr))
    )
  } else if (opt_badf == "simulated") {
    badf_crit <- apply(results$badf, 1, quantile, probs = pr) %>%
      t() %>% apply(2, cummax)
  }

  list(adf_cv = adf_crit,
       sadf_cv = sadf_crit,
       gsadf_cv = gsadf_crit,
       badf_cv = badf_crit,
       bsadf_cv = bsadf_crit) %>%
    inherit_attrs(results) %>%
    add_attr(opt_badf = opt_badf,
             opt_bsadf = opt_bsadf) %>%
    add_class("mc_cv", "cv")

}

#' @rdname mc_cv
#' @inheritParams mc_cv
#' @export
mc_distr <- function(n, minw = NULL, nrep = 2000L, seed = NULL) {

  results <- mc_(n, minw = minw, nrep = nrep, seed = seed)

  list(adf_cv = results$adf,
       sadf_cv = results$sadf,
       gsadf_cv = results$gsadf) %>%
    inherit_attrs(results) %>%
    add_class("mc_distr", "distr")

}
