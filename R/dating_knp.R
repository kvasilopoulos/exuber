# Kejriwal, Nguyen & Perron (2025, JTSA 46(5), "An Improved Procedure
# for Retrospectively Dating the Emergence and Collapse of Bubbles";
# "KNP"). See docs/dating-and-root-inference.md, "SSR/BIC
# dating vs. PSY recursive dating", for the full evaluation this
# implements.
#
# KNP's own DGP/estimating model (their eq. 1-3, confirmed via rendered
# PDF pages) is structurally identical to HLS (2017)'s Model 2: an
# unfitted unit-root regime, then an intercept+slope-fitted explosive
# regime, then an unfitted unit-root regime resuming from a shifted
# level (an instantaneous, not gradual, collapse). Their Theorem 1
# proves the plain-OLS joint SSR minimiser over this model is
# INCONSISTENT: the origination-date estimate T_hat_1 converges to the
# true COLLAPSE date T2^0, not the true origination date T1^0 -- a
# strictly worse failure than PSY's own well-known "late" bias. Their
# fix (Theorem 2) is small: omit the single squared residual at the
# collapse-date observation (Delta y_{T2+1})^2 from the objective before
# minimising, which restores consistency for both dates (and for the
# explosive-coefficient estimate). A footnote proves this is numerically
# equivalent to a one-time-dummy modification of HLS's own Model 4
# regression, but the omission formulation is the cheap one to compute:
# it needs no new regression at all, only subtracting a single already-
# available squared term from dating_hls.R's existing Model-2-style
# closed-form SSR (hls_segment_ssr()), reused directly here.
#
# KNP's Section 3 generalises this to m breaks (eq. 10-11): regimes
# alternate unit root (unfitted) / explosive (intercept + slope fitted),
# starting with a unit root, and every unit-root regime that follows a
# collapse omits its first residual. Their proposed algorithm is a
# Bai-Perron dynamic programme over that objective -- because the
# unit-root restriction is imposed exactly (not estimated), the
# restricted SSR of every segment is known in closed form and no
# Perron-Qu iteration over initial values is needed, so the DP returns
# exactly the global grid-search minimiser. knp_dp() below; every
# segment SSR is O(1) from hls_prefix_sums(), so the whole search is
# O(m T^2). The number of breaks is taken as given, as in the paper
# (which leaves its selection open).

# KNP's m-break dynamic programme. Pairs are i-indexed 1..n1 (x_i =
# y_i, z_i = Delta y_{i+1}); regime j covers (tau_{j-1}, tau_j]; odd j is
# a unit-root regime (cost sum z^2, minus its first term when it follows
# a collapse and `omit`), even j an explosive one (OLS SSR of z on x with
# intercept). Every regime has at least k_min pairs. Returns the break
# points tau_1..tau_m and the minimised SSR.
knp_dp <- function(y, breaks, trim = 0.05, omit = TRUE) {
  ps <- hls_prefix_sums(y)
  n1 <- ps$n1
  k_min <- max(2L, ceiling(trim * n1))
  nreg <- breaks + 1L
  if (nreg * k_min > n1) {
    return(list(tau = rep(NA_integer_, breaks), ssr = Inf))
  }
  cost <- function(j, lo, hi) {
    if (j %% 2L == 0L) {
      return(hls_segment_ssr(ps, lo, hi, TRUE))
    }
    ssr <- hls_segment_ssr(ps, lo, hi, FALSE)
    if (omit && j > 1L) ssr <- ssr - (ps$cz2[lo + 2L] - ps$cz2[lo + 1L])
    ssr
  }
  # V[j, hi + 1]: best SSR of the first j regimes ending at pair hi;
  # arg[j, hi + 1]: the start (tau_{j-1}) achieving it.
  V <- arg <- matrix(NA_real_, nreg, n1 + 1L)
  hi1 <- k_min:n1
  V[1L, hi1 + 1L] <- cost(1L, 0L, hi1)
  for (j in 2:nreg) {
    his <- if (j == nreg) n1 else (j * k_min):(n1 - (nreg - j) * k_min)
    for (hi in his) {
      lo <- ((j - 1L) * k_min):(hi - k_min)
      v <- V[j - 1L, lo + 1L] + cost(j, lo, hi)
      k <- which.min(v)
      V[j, hi + 1L] <- v[k]
      arg[j, hi + 1L] <- lo[k]
    }
  }
  tau <- integer(breaks)
  hi <- n1
  for (j in nreg:2) {
    hi <- arg[j, hi + 1L]
    tau[j - 1L] <- hi
  }
  list(tau = tau, ssr = V[nreg, n1 + 1L])
}

# Jointly searches (tau1, tau2) minimising KNP's SSR (eq. 3, `omit =
# FALSE`) or their omission-corrected SSR (eq. 4, `omit = TRUE`,
# default) -- both reuse dating_hls.R's hls_prefix_sums()/hls_segment_ssr()
# directly, since KNP's model has the same three-segment (unfitted,
# intercept+slope, unfitted) shape as HLS's Model 2. Unlike
# hls_model23(), KNP's own candidate set T_epsilon(2) imposes no
# directional sign constraint on the fitted "peak".
knp_find_break <- function(y, trim = 0.05, omit = TRUE) {
  n1 <- length(y) - 1L
  ps <- hls_prefix_sums(y)
  k_min <- max(2L, ceiling(trim * n1))
  tau1_max <- n1 - 2L * k_min
  best <- list(ssr = Inf, tau1 = NA_integer_, tau2 = NA_integer_)
  if (tau1_max < k_min) {
    return(best)
  }
  for (tau1 in k_min:tau1_max) {
    tau2 <- (tau1 + k_min):(n1 - k_min)
    ssr <- hls_segment_ssr(ps, 0L, tau1, FALSE) +
      hls_segment_ssr(ps, tau1, tau2, TRUE) +
      hls_segment_ssr(ps, tau2, n1, FALSE)
    if (omit) {
      z2_single <- ps$cz2[tau2 + 2L] - ps$cz2[tau2 + 1L]
      ssr <- ssr - z2_single
    }
    j <- which.min(ssr)
    if (ssr[j] < best$ssr) best <- list(tau1 = tau1, tau2 = tau2[j], ssr = ssr[j])
  }
  best
}

#' Bias-Corrected Bubble Dating (Kejriwal, Nguyen & Perron 2025)
#'
#' \code{dating_knp} dates bubble episodes (origination, collapse) by
#' minimising a residual-omission-corrected sum of squared residuals over
#' a model of alternating regimes: unit root, explosive, unit root
#' resuming from a shifted level after an instantaneous collapse, and so
#' on. Plain OLS over this model is provably inconsistent -- the
#' origination-date estimate converges to the true \emph{collapse} date,
#' not the origination date -- which \code{omit = TRUE} (the default)
#' fixes by dropping the squared residual at each candidate collapse date
#' from the objective before minimising.
#'
#' \code{breaks = 2} (the default) is the single-bubble model. More
#' breaks use Kejriwal, Nguyen & Perron's dynamic-programming algorithm,
#' which returns the exact global minimiser of the objective in
#' \code{O(breaks * n^2)}. The number of breaks is taken as given, as in
#' the paper: e.g. two per episode \code{\link{datestamp}} finds.
#'
#' @note This is a residual-sum-of-squares model-selection dating
#' procedure, not a hypothesis test -- it needs no critical values at all.
#'
#' @inheritParams radf
#' @param trim Minimum fraction of the (differenced) sample required in
#' each regime (default 0.05).
#' @param breaks Number of break dates (the paper's \code{m}): two per
#' bubble; an odd number lets the last bubble run to the end of the sample
#' (its collapse is then \code{NA}).
#' @param omit Use Kejriwal, Nguyen & Perron's consistency-restoring
#' correction (default \code{TRUE}). \code{FALSE} gives the plain,
#' provably inconsistent OLS estimator (their Theorem 1) -- kept mainly
#' to demonstrate the correction's effect, not for practical dating.
#'
#' @return An object of class \code{dating_knp_obj}: a list with
#' \code{origination}, \code{collapse} (dates) and \code{delta} (the
#' fitted explosive AR coefficient) -- named vectors (one value per series)
#' for a single bubble, matrices (one row per bubble, one column per
#' series) for more.
#'
#' @references Kejriwal, M., Nguyen, L., & Perron, P. (2025). An
#' improved procedure for retrospectively dating the emergence and
#' collapse of bubbles. Journal of Time Series Analysis, 46(5), 867-883.
#'
#' @seealso \code{\link{dating_hls}}, \code{\link{dating_pdc}} for related
#' SSR-based dating approaches.
#'
#' @note Returns its own class (not `radf_obj`), so it does not plug into
#' `summary()`/`\link{datestamp}`/`tidy`; it has its own `print()` and
#' `autoplot()` methods instead. Prints its own
#' dating table (model, origination, collapse, recovery) -- see
#' `vignette("naming-and-analysis", package = "exuber")` for the full
#' picture of which functions do and don't fit that pipeline.
#'
#' @section Status:
#' `r lifecycle::badge("experimental")`
#'
#' @examples
#' \donttest{
#' res <- dating_knp(sim_data$psy1, trim = 0.05)
#' print(res)
#' autoplot(res)
#'
#' # Compare the bias-corrected estimate against the plain (inconsistent) OLS
#' # one, layering an extra reference line onto the internal autoplot() output
#' res_plain <- dating_knp(sim_data$psy1, trim = 0.05, omit = FALSE)
#' autoplot(res) +
#'   ggplot2::geom_vline(xintercept = as.numeric(res_plain$origination), linetype = 3)
#'
#' # Two bubbles
#' dating_knp(sim_data$psy2, breaks = 4)
#' }
#'
#' @family dating
#' @export
dating_knp <- function(data, trim = 0.05, omit = TRUE, breaks = 2L) {
  assert_positive_int(breaks)
  x <- parse_data(data)
  n <- nrow(x)
  snames <- colnames(x)
  idx <- attr(x, "index")
  nc <- ncol(x)
  nb <- ceiling(breaks / 2)

  origination <- collapse <- matrix(NA_character_, nb, nc, dimnames = list(NULL, snames))
  delta <- matrix(NA_real_, nb, nc, dimnames = list(NULL, snames))

  for (j in seq_len(nc)) {
    y <- as.numeric(x[, j])
    ps <- hls_prefix_sums(y)
    tau <- if (breaks == 2L) {
      fit <- knp_find_break(y, trim, omit)
      c(fit$tau1, fit$tau2)
    } else {
      knp_dp(y, breaks, trim, omit)$tau
    }
    if (anyNA(tau)) next
    ends <- c(tau, ps$n1)
    for (b in seq_len(nb)) {
      t1 <- ends[2L * b - 1L]
      t2 <- ends[2L * b]
      origination[b, j] <- as.character(idx[t1 + 1L])
      if (2L * b <= breaks) collapse[b, j] <- as.character(idx[t2 + 1L])
      delta[b, j] <- unname(hls_segment_coef(ps, t1, t2)["slope"]) + 1
    }
  }
  if (nb == 1L) {
    origination <- origination[1L, ]
    collapse <- collapse[1L, ]
    delta <- delta[1L, ]
  }

  list(origination = origination, collapse = collapse, delta = delta) %>%
    add_attr(index = idx, series_names = snames, n = n, trim = trim, omit = omit, breaks = breaks, mat = x) %>%
    add_class("dating_knp_obj")
}

#' Plot method for dating_knp() output
#'
#' Plots each series with vertical markers at the estimated origination and collapse dates.
#'
#' @param object An object of class \code{dating_knp_obj}, the output of \code{\link{dating_knp}}.
#' @param ... Further arguments passed to methods. Not used.
#'
#' @return A \link[ggplot2]{ggplot}
#' @seealso \code{\link{dating_knp}}
#' @export
autoplot.dating_knp_obj <- function(object, ...) {
  idx <- index(object)
  flat <- function(m) if (is.matrix(m)) setNames(c(m), rep(colnames(m), each = nrow(m))) else m
  breaks <- breaks_tbl(idx, origination = flat(object$origination), collapse = flat(object$collapse))
  autoplot_series_breaks(mat(object), idx, breaks)
}

#' @export
print.dating_knp_obj <- function(x, digits = max(3L, getOption("digits") - 3L), ...) {
  cat_line()
  cat_rule(left = glue(
    "dating_knp (n = {attr(x, 'n')}, trim = {attr(x, 'trim')}, omit = {attr(x, 'omit')}, ",
    "breaks = {attr(x, 'breaks') %||% 2}"
  ))
  cat_line()
  m <- function(v) if (is.matrix(v)) v else t(v)
  o <- m(x$origination)
  print(
    data.frame(
      series = rep(colnames(o) %||% names(x$origination), each = nrow(o)),
      bubble = rep(seq_len(nrow(o)), ncol(o)),
      origination = c(o), collapse = c(m(x$collapse)), delta = c(m(x$delta)),
      row.names = NULL
    ),
    digits = digits, print.gap = 2L, row.names = FALSE
  )
  cat_line()
}
