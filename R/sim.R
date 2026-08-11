#' Simulation of a single-bubble process
#'
#' The following function generates a time series which switches from a martingale to a mildly explosive
#' process and then back to a martingale.
#'
#' @param n A positive integer specifying the length of the simulated output series.
#' @param te A scalar in (0, tf) specifying the observation in which the bubble originates.
#' @param tf A scalar in (te, n) specifying the observation in which the bubble collapses.
#' @param c A positive scalar determining the autoregressive coefficient in the explosive regime.
#' @param alpha A positive scalar in (0, 1) determining the value of the expansion rate in the autoregressive coefficient.
#' @param sigma A positive scalar indicating the standard deviation of the innovations.
#' @inheritParams radf_mc_cv
#'
#' @details
#' The data generating process is described by the following equation:
#' \deqn{X_t = X_{t-1}1\{t < \tau_e\}+ \delta_T X_{t-1}1\{\tau_e \leq t\leq \tau_f\} +
#' \left(\sum_{k=\tau_f+1}^t \epsilon_k + X_{\tau_f}\right) 1\{t > \tau_f\} + \epsilon_t 1\{t \leq \tau_f\}
#' }{X[t] = X[t-1] 1{t < te}+ \delta[T] * X[t-1] 1{te \le t \le tf} +
#' (\sum [k=tf+1]^t \epsilon[k] + X[tf]) 1{t > tf} + \epsilon[t] 1{t \le tf},}
#'
#' where the autoregressive coefficient \eqn{\delta_T}{\delta[T]} is given by:
#'
#' \deqn{\delta_T = 1 + cT^{-a}}{\delta[T] = 1 + c*T^{-a}}
#'
#' with \eqn{c>0}, \eqn{\alpha \in (0,1)}{\alpha in (0,1)},
#' \eqn{\epsilon \sim iid(0, \sigma^2)}{\epsilon - iid(0, \sigma^2)} and
#' \eqn{X_{\tau_f} = X_{\tau_e} + X'}{X[tf] = X[te] + X'} with \eqn{X' = O_p(1)}{X'= 0p(1)},
#' \eqn{\tau_e = [T r_e]}{te = [T re]} dates the origination of the bubble,
#'  and \eqn{\tau_f = [T r_f]}{tf = [T rf]} dates the collapse of the bubble.
#' During the pre- and post- bubble periods, \eqn{[1, \tau_e)}{[1, te)},
#' \eqn{X_t}{Xt} is a pure random walk process. During the bubble expansion period
#'  \eqn{\tau_e, \tau_f]}{[te,tf]} becomes a mildly explosive process with expansion rate
#'  given by the autoregressive coefficient \eqn{\delta_T}{\delta[T]}; and, finally
#'  during the post-bubble period, \eqn{(\tau_f, \tau]}{(tf, t]}  \eqn{X_t}{Xt} reverts to a martingale.
#'
#'
#' For further details see Phillips et al. (2015) p. 1054.
#'
#' @param e An optional numeric vector of length \code{n - 1} of innovations
#' to use in place of \code{rnorm(n - 1, sd = sigma)}. Lets the plain PSY
#' equation above be driven by a non-Gaussian/heteroskedastic/dependent shock
#' sequence instead of i.i.d. Gaussian noise -- see \code{\link{sim_innov}}
#' (heavy-tailed/skewed), \code{\link{sim_vol_garch}} (GARCH/TGARCH),
#' \code{\link{sim_vol_cir}}/\code{\link{sim_vol_sv}} (stochastic volatility)
#' and \code{\link{sim_fi}} (long-memory) for ready-made generators. Default
#' \code{NULL} reproduces the plain i.i.d. Gaussian DGP exactly.
#' @param shifts An optional data frame/list with integer element/column
#' \code{date} (in \code{2:n}) and numeric element/column \code{size}, adding
#' a one-period deterministic level shift of magnitude \code{size} at each
#' \code{date} -- Harvey, Leybourne, Tatlow & Zu (2025)'s level-shift DGP.
#' Default \code{NULL} adds no shifts.
#' @param coef_noise An optional numeric vector of length \code{n - 1},
#' mean-zero/unit-variance, perturbing the explosive-regime coefficient as
#' \code{delta + coef_a * coef_noise[t] / sqrt(n)} instead of the fixed
#' \code{delta} -- Kurozumi & Nishi (2025)'s stochastically varying explosive
#' coefficient. Default \code{NULL} keeps \code{delta} fixed.
#' @param coef_a A positive scalar scaling \code{coef_noise}. Ignored if
#' \code{coef_noise} is \code{NULL}.
#'
#' @return A numeric vector of length n.
#' @export
#'
#' @references Phillips, P. C. B., Shi, S., & Yu, J. (2015). Testing for Multiple Bubbles:
#' Historical Episodes of Exuberance and Collapse in the S&P 500. International Economic Review, 5
#' 6(4), 1043-1078.
#'
#' @seealso \code{\link{sim_psy2}}, \code{\link{sim_blan}}, \code{\link{sim_evans}}
#'
#' @examples
#' # 100 periods with bubble origination date 40 and termination date 55
#' sim_psy1(n = 100, seed = 123) %>%
#'   autoplot()
#'
#' # 200 periods with bubble origination date 80 and termination date 110
#' sim_psy1(n = 200, seed = 123) %>%
#'   autoplot()
#'
#' # 200 periods with bubble origination date 100 and termination date 150
#' sim_psy1(n = 200, te = 100, tf = 150, seed = 123) %>%
#'   autoplot()
#'
#' # Same DGP, driven by GARCH(1,1) innovations instead of i.i.d. Gaussian
#' sim_psy1(n = 200, seed = 123, e = sim_vol_garch(199, seed = 123)) %>%
#'   autoplot()
#'
#' # Same DGP, with two deterministic level shifts
#' sim_psy1(n = 200, seed = 123, shifts = list(date = c(50, 150), size = c(20, -20))) %>%
#'   autoplot()
sim_psy1 <- function(n, te = 0.4 * n, tf = 0.15 * n + te, c = 1,
                     alpha = 0.6, sigma = 6.79, seed = NULL,
                     e = NULL, shifts = NULL, coef_noise = NULL, coef_a = 1) {
  assert_positive_int(n)
  assert_between(te, 0, n)
  assert_between(tf, te, n)
  assert_positive_int(c)
  assert_between(alpha, 0, 1)
  stopifnot(sigma >= 0)
  if (!is.null(e) && length(e) != n - 1) {
    stop_glue("Argument 'e' should have length n - 1")
  }
  if (!is.null(coef_noise) && length(coef_noise) != n - 1) {
    stop_glue("Argument 'coef_noise' should have length n - 1")
  }

  set_rng(seed)

  delta <- 1 + c * n ^ (-alpha)
  eps <- e %||% rnorm(n - 1, sd = sigma)

  shift_at <- numeric(n)
  if (!is.null(shifts)) {
    shift_at[shifts$date] <- shift_at[shifts$date] + shifts$size
  }

  y <- 100

  for (t in 2:n) {
    delta_t <- if (!is.null(coef_noise) && t >= te && t <= tf) {
      delta + coef_a * coef_noise[t - 1] / sqrt(n)
    } else {
      delta
    }
    if (t < te) {
      y[t] <- y[t - 1] + eps[t - 1] + shift_at[t]
    } else if (t >= te & t <= tf) {
      y[t] <- delta_t * y[t - 1] + eps[t - 1] + shift_at[t]
    } else if (t == tf + 1) {
      y[t] <- y[te] + eps[t - 1] + shift_at[t]
    } else {
      y[t] <- y[t - 1] + eps[t - 1] + shift_at[t]
    }
  }

  y %>%
    add_attr(seed = get_rng_state(seed)) %>%
    add_class(class = "sim")
}


#' Simulate innovations with heavy-tailed/skewed marginal distributions
#'
#' Generates a shock sequence with the same PSY-style mean equation in mind
#' (\code{\link{sim_psy1}}, \code{\link{sim_psy2}}) but a non-Gaussian
#' marginal, standardized to mean 0 and variance \code{sigma^2} so it drops
#' straight into \code{sim_psy1(..., e = sim_innov(...))}.
#'
#' \code{dist = "t"} rescales a Student-t(\code{df}) draw to variance 1
#' before scaling by \code{sigma} (exact, closed form: \code{Var(t_df) =
#' df / (df - 2)}). \code{dist = "skew_t"} combines two independent
#' standardized Student-t draws Azzalini-style,
#' \code{delta * abs(T0) + sqrt(1 - delta^2) * T1} with
#' \code{delta = xi / sqrt(1 + xi^2)}, then standardizes using the closed-form
#' mean/variance of that combination (via \code{E|T0|}, itself closed-form
#' through the Beta function). \code{xi > 0} skews right, \code{xi < 0} skews
#' left, \code{xi = 0} reduces to the symmetric \code{t} case.
#'
#' @inheritParams sim_psy1
#' @param n Number of innovations to generate.
#' @param dist One of \code{"normal"}, \code{"t"}, \code{"skew_t"}.
#' @param df Degrees of freedom for \code{"t"}/\code{"skew_t"} (\code{> 2}).
#' @param xi Skewness parameter for \code{"skew_t"} (any real; 0 = symmetric).
#'
#' @return A numeric vector of length \code{n}.
#' @export
#'
#' @references Wu, R., Shi, S. & Wu, J. (2025). "Quantile analysis for
#' financial bubble detection and surveillance." JTSA, 46(5), 908-931 (uses
#' N(0,1)/t(3)/skewed-t(3, -0.75)/skewed-t(3, 0.75) innovations in their
#' Monte Carlo design, eq. 6).
#'
#' @seealso \code{\link{sim_psy1}}
#'
#' @examples
#' sim_innov(199, dist = "skew_t", df = 3, xi = -0.75, seed = 1)
sim_innov <- function(n, dist = c("normal", "t", "skew_t"), sigma = 6.79,
                      df = 5, xi = 0, seed = NULL) {
  dist <- match.arg(dist)
  assert_positive_int(n)
  stopifnot(sigma >= 0, df > 2)

  set_rng(seed)

  z <- switch(dist,
    normal = rnorm(n),
    t = rt(n, df) / sqrt(df / (df - 2)),
    skew_t = {
      delta <- xi / sqrt(1 + xi ^ 2)
      t0 <- rt(n, df) / sqrt(df / (df - 2))
      t1 <- rt(n, df) / sqrt(df / (df - 2))
      raw <- delta * abs(t0) + sqrt(1 - delta ^ 2) * t1
      e_abs_t0 <- (2 * sqrt(df) / ((df - 1) * beta(df / 2, 0.5))) / sqrt(df / (df - 2))
      mean_raw <- delta * e_abs_t0
      sd_raw <- sqrt(max(1 - delta ^ 2 * e_abs_t0 ^ 2, .Machine$double.eps))
      (raw - mean_raw) / sd_raw
    }
  )

  (z * sigma) %>%
    add_attr(seed = get_rng_state(seed)) %>%
    add_class(class = "sim")
}

#' Simulate GARCH(1,1)/TGARCH(1,1) innovations
#'
#' Generates shocks \code{z_t = sqrt(h_t) * eps_t} under a GARCH(1,1)
#' recursion, with an optional threshold (leverage) term, for use as
#' \code{sim_psy1(..., e = sim_vol_garch(...))}.
#'
#' \deqn{z_t = \sqrt{h_t}\,\epsilon_t,\quad
#' h_t = \omega + \alpha z_{t-1}^2 + \beta h_{t-1} +
#' \gamma z_{t-1}^2 1\{z_{t-1}<0\}}{
#' z[t] = sqrt(h[t]) * eps[t], h[t] = omega + alpha*z[t-1]^2 + beta*h[t-1] +
#' gamma*z[t-1]^2 * 1(z[t-1] < 0)}
#'
#' with \eqn{\epsilon_t \sim NIID(0,1)}{eps[t] ~ NIID(0,1)} and
#' \eqn{h_0 = z_0 = 0}. \code{gamma = 0} (the default) is plain GARCH(1,1);
#' \code{gamma > 0} adds the TGARCH leverage effect (larger response to
#' negative shocks).
#'
#' @param n Number of innovations to generate.
#' @param omega,alpha,beta Positive GARCH(1,1) parameters. Default
#' (\code{omega = 0.1, alpha = 0.1, beta = 0.8}) matches Whitehouse, Harvey &
#' Leybourne (2025) and Harvey, Leybourne, Taylor & Zu (2024).
#' @param gamma Non-negative TGARCH leverage parameter. Monschang & Wilfling
#' (2021)'s NASDAQ calibration is \code{omega = 0.4387, alpha = 0, beta =
#' 0.9319, gamma = 0.1306}.
#' @inheritParams sim_innov
#'
#' @return A numeric vector of length \code{n}.
#' @export
#'
#' @references Whitehouse, E.J., Harvey, D.I. & Leybourne, S.J. (2025).
#' "Real-time monitoring of explosive financial bubbles." Monschang, V. &
#' Wilfling, B. (2021). "Sup-ADF-style bubble-detection methods under test."
#' Empirical Economics, 61, 145-172.
#'
#' @seealso \code{\link{sim_psy1}}
#'
#' @examples
#' sim_vol_garch(199, seed = 1)
#' # NASDAQ-calibrated TGARCH (Monschang & Wilfling 2021)
#' sim_vol_garch(199, omega = 0.4387, alpha = 0, beta = 0.9319, gamma = 0.1306, seed = 1)
sim_vol_garch <- function(n, omega = 0.1, alpha = 0.1, beta = 0.8, gamma = 0,
                          seed = NULL) {
  assert_positive_int(n)
  stopifnot(omega > 0, alpha >= 0, beta >= 0, gamma >= 0)

  set_rng(seed)

  eps <- rnorm(n)
  h <- numeric(n)
  z <- numeric(n)
  h_prev <- 0
  z_prev <- 0
  for (t in seq_len(n)) {
    h[t] <- omega + alpha * z_prev ^ 2 + beta * h_prev +
      gamma * z_prev ^ 2 * (z_prev < 0)
    z[t] <- sqrt(h[t]) * eps[t]
    h_prev <- h[t]
    z_prev <- z[t]
  }

  z %>%
    add_attr(seed = get_rng_state(seed)) %>%
    add_class(class = "sim")
}

#' Simulate CIR-type stochastic-volatility innovations
#'
#' Generates shocks driven by a Cox-Ingersoll-Ross (square-root) stochastic
#' variance process, Euler-Maruyama discretized, for use as
#' \code{sim_psy1(..., e = sim_vol_cir(...))}.
#'
#' \deqn{d\sigma^2(r) = \kappa(\theta - \sigma^2(r))dr + \xi\sigma(r)dB(r)}{
#' d sigma^2(r) = kappa*(theta - sigma^2(r))dr + xi*sigma(r)*dB(r)}
#'
#' discretized over \code{n} steps of \code{r} in \eqn{[0, 1]}, with variance
#' reflected at zero if a step would take it negative. Default parameters
#' (\eqn{\kappa=0.03}{kappa=0.03}, \eqn{\theta=0.25}{theta=0.25},
#' \eqn{\xi=0.1}{xi=0.1}) match Harvey, Leybourne & Zu (2019)'s robustness
#' design, "representative of Bollerslev and Zhou (2002)".
#'
#' @param n Number of innovations to generate.
#' @param kappa,theta,xi Positive CIR parameters (mean-reversion speed,
#' long-run variance, vol-of-vol).
#' @param sigma0_sq Non-negative starting variance. Defaults to \code{theta}.
#' @inheritParams sim_innov
#'
#' @return A numeric vector of length \code{n}.
#' @export
#'
#' @references Harvey, D.I., Leybourne, S.J. & Zu, Y. (2019). "Testing
#' explosive bubbles with time-varying volatility." Econometric Reviews,
#' 38(10), 1131-1151.
#'
#' @seealso \code{\link{sim_psy1}}, \code{\link{sim_vol_sv}}
#'
#' @examples
#' sim_vol_cir(199, seed = 1)
sim_vol_cir <- function(n, kappa = 0.03, theta = 0.25, xi = 0.1,
                        sigma0_sq = theta, seed = NULL) {
  assert_positive_int(n)
  stopifnot(kappa > 0, theta > 0, xi > 0, sigma0_sq >= 0)

  set_rng(seed)

  dt <- 1 / n
  sig2 <- numeric(n)
  sig2[1] <- sigma0_sq
  if (n > 1) {
    db <- rnorm(n - 1, sd = sqrt(dt))
    for (i in 2:n) {
      prev <- max(sig2[i - 1], 0)
      sig2[i] <- max(prev + kappa * (theta - prev) * dt + xi * sqrt(prev) * db[i - 1], 0)
    }
  }

  (sqrt(sig2) * rnorm(n)) %>%
    add_attr(seed = get_rng_state(seed)) %>%
    add_class(class = "sim")
}

#' Simulate AR(1) lognormal stochastic-volatility innovations
#'
#' Generates shocks \code{z_t = sigma_t * eps_t} with a persistent AR(1)
#' log-variance, for use as \code{sim_psy1(..., e = sim_vol_sv(...))}.
#'
#' \deqn{\log\sigma_t^2 = \phi\log\sigma_{t-1}^2 + \eta_t,\quad
#' \eta_t \sim iid\, N(0, \tau^2)}{log(sigma[t]^2) = phi*log(sigma[t-1]^2) +
#' eta[t], eta[t] ~ iid N(0, tau^2)}
#'
#' with \code{phi} close to (but below) 1 for the "double local-to-unity"
#' near-integrated-variance case studied in the source.
#'
#' @param n Number of innovations to generate.
#' @param phi AR(1) log-variance persistence, in (0, 1).
#' @param tau Positive standard deviation of the log-variance innovations.
#' @param log_sigma0_sq Starting value of \code{log(sigma^2)}. Defaults to 0.
#' @inheritParams sim_innov
#'
#' @return A numeric vector of length \code{n}.
#' @export
#'
#' @references Sarkar, A. & Wells, M.T. (2025). "Double Local-to-Unity."
#' arXiv:2512.06823.
#'
#' @seealso \code{\link{sim_psy1}}, \code{\link{sim_vol_cir}}
#'
#' @examples
#' sim_vol_sv(199, seed = 1)
sim_vol_sv <- function(n, phi = 0.98, tau = 0.1, log_sigma0_sq = 0,
                       seed = NULL) {
  assert_positive_int(n)
  assert_between(phi, 0, 1)
  stopifnot(tau > 0)

  set_rng(seed)

  log_sig2 <- numeric(n)
  log_sig2[1] <- log_sigma0_sq
  if (n > 1) {
    eta <- rnorm(n - 1, sd = tau)
    for (i in 2:n) log_sig2[i] <- phi * log_sig2[i - 1] + eta[i - 1]
  }

  (exp(log_sig2 / 2) * rnorm(n)) %>%
    add_attr(seed = get_rng_state(seed)) %>%
    add_class(class = "sim")
}

#' Simulate fractionally-integrated (long-memory) innovations
#'
#' Generates \eqn{u_t = \Delta^{-d}\epsilon_t}{u[t] = Delta^(-d) eps[t]},
#' \eqn{\epsilon_t}{eps[t]} i.i.d. \eqn{(0, \sigma^2)}{(0, sigma^2)}, via a
#' truncated \eqn{MA(\infty)}{MA(Inf)} expansion of the fractional-differencing
#' operator, for use as \code{sim_psy1(..., e = sim_fi(...))}.
#'
#' \deqn{\Delta^{-d} = \sum_{j=0}^\infty \psi_j L^j,\quad \psi_0 = 1,\quad
#' \psi_j = \psi_{j-1}\frac{j-1+d}{j}}{Delta^(-d) = sum_j psi[j] L^j, psi[0]
#' = 1, psi[j] = psi[j-1] * (j-1+d)/j}
#'
#' truncated at \code{max(200, n)} lags with a matching burn-in (dropped
#' before returning) to limit truncation bias in the early observations.
#'
#' @param n Number of innovations to generate.
#' @param d Long-memory (fractional differencing) parameter, in (0, 0.5) for
#' \eqn{u_t}{u[t]} itself to be stationary.
#' @inheritParams sim_innov
#'
#' @return A numeric vector of length \code{n}.
#' @export
#'
#' @references Lui, Y.L., Phillips, P.C.B. & Yu, J. (2024). "Robust testing
#' for explosive behavior with strongly dependent errors."
#'
#' @seealso \code{\link{sim_psy1}}
#'
#' @examples
#' sim_fi(199, d = 0.2, seed = 1)
sim_fi <- function(n, d = 0.2, sigma = 1, seed = NULL) {
  assert_positive_int(n)
  stopifnot(d > 0, d < 0.5, sigma > 0)

  set_rng(seed)

  # Truncation lag for the MA(Inf) expansion of Delta^(-d) -- needs a full
  # (m + 1)-tap window of *past* eps realized before each kept observation,
  # so the innovation series must run m longer than the output, not just
  # `n` long (a length-n window would leave only the last observation
  # defined). Convolved by hand rather than stats::filter()/stats::convolve()
  # -- both segfault on some Windows R builds even on trivial inputs.
  m <- max(500, 5 * n)
  eps <- rnorm(n + m, sd = sigma)
  psi <- numeric(m + 1)
  psi[1] <- 1
  for (j in 2:(m + 1)) psi[j] <- psi[j - 1] * (j - 2 + d) / (j - 1)
  u <- vapply(seq_len(n), function(k) sum(psi * rev(eps[k:(k + m)])), numeric(1))

  u %>%
    add_attr(seed = get_rng_state(seed)) %>%
    add_class(class = "sim")
}

#' Simulation of a two-bubble process
#'
#' The following data generating process is similar to  \code{\link{sim_psy1}}, with the difference that
#' there are two episodes of mildly explosive dynamics.
#'
#' @inheritParams sim_psy1
#' @param te1 A scalar in (0, n) specifying the observation in which the first bubble originates.
#' @param tf1 A scalar in  (te1, n) specifying the observation in which the first bubble collapses.
#' @param te2 A scalar in (tf1, n) specifying the observation in which the second bubble originates.
#' @param tf2 A scalar in (te2, n) specifying the observation in which the second bubble collapses.
#'
#' @details
#' The two-bubble data generating process is given by (see also \code{sim_psy1}):
#'
#' \deqn{X_t = X_{t-1}1\{t \in N_0\}+ \delta_T X_{t-1}1\{t \in B_1 \cup B_2\} +
#' \left(\sum_{k=\tau_{1f}+1}^t \epsilon_k + X_{\tau_{1f}}\right) 1\{t \in N_1\} }{
#' X[t]=X[t-1] 1{t in N[0]}+ \delta[T] * X[t-1] 1{t in B[1] union B[2]} +
#' (\sum[k=t1f+1]^t \epsilon[k] + X'[t1f]) 1{t in N[1]} +
#' }
#'
#' \deqn{ + \left(\sum_{l=\tau_{2f}+1}^t \epsilon_l + X_{\tau_{2f}}\right) 1\{t \in N_2\} +
#' \epsilon_t 1\{t \in N_0 \cup B_1 \cup B_2\}}{(\sum[l=t2f+1]^t \epsilon[l] + X'[t2f]) 1{t in N[2]} +
#' \epsilon[t] 1{t in N[0] union B[1] union B[2]},}
#'
#' where the autoregressive coefficient \eqn{\delta_T}{\delta[T]} is:
#'
#' \deqn{\delta_T = 1 + cT^{-a}}{\delta[T] = 1 + c*T^{-a},}
#'
#' with \eqn{c>0}, \eqn{\alpha \in (0,1)}{\alpha in (0,1)},
#' \eqn{\epsilon \sim iid(0, \sigma^2)}{\epsilon - iid(0, \sigma^2)},
#' \eqn{N_0 = [1, \tau_{1e})}{N0 = [1, t1e)},
#' \eqn{B_1 = [\tau_{1e}, \tau_{1f}]}{B1 = [te1, t1f]},
#' \eqn{N_1 = (\tau_{1f}, \tau_{2e})}{N0 = (t1f, t2e)},
#' \eqn{B_2 = [\tau_{2e}, \tau_{2f}]}{N0 = [t2e, t2f]},
#' \eqn{N_2 = (\tau_{2f}, \tau]}{N0 = [t2f, t]},
#' where \eqn{\tau}{t} is the last observation of the sample.
#' The observations \eqn{\tau_{1e} = [T r_{1e}]}{te1 = [T re1]}
#' and \eqn{\tau_{1f} = [T r_{1f}]}{tf = [T r1f]}
#' are the origination and termination dates of the first bubble;
#' \eqn{\tau_{2e} = [T r_{2e}]}{te2 = [T re2]} and \eqn{\tau_{2f} = [T r_{2f}]}{tf = [T r2f]}
#' are the origination and termination dates of the second bubble.
#' After the collapse of the first bubble, \eqn{X_t}{X[t]} resumes a martingale path until time
#' \eqn{\tau_{2e}-1}{t2e - 1}, and a second episode of exuberance begins at \eqn{\tau_{2e}}{t2e}.
#' Exuberance lasts lasts until \eqn{\tau_{2f}}{t2f} at which point the process collapses to a value of
#' \eqn{X_{\tau_{2f}}}{X[t2f]}. The process then continues on a martingale path until the end of the
#' sample period \eqn{\tau}{t}. The duration of the first bubble is assumed to be longer than
#' that of the second bubble, i.e. \eqn{\tau_{1f}-\tau_{1e}>\tau_{2f}-\tau_{2e}}{t1f - t1e > t2f - t2e}.
#'
#' For further details you can refer to Phillips et al., (2015) p. 1055.
#'
#' @return A numeric vector of length \code{n}.
#' @export
#'
#' @references Phillips, P. C. B., Shi, S., & Yu, J. (2015). Testing for Multiple Bubbles:
#' Historical Episodes of Exuberance and Collapse in the S&P 500. International Economic Review, 5
#' 6(4), 1043-1078.
#'
#' @seealso \code{\link{sim_psy1}}, \code{\link{sim_blan}}, \code{\link{sim_evans}}
#'
#' @examples
#' # 100 periods with bubble origination dates 20/60 and termination dates 40/70
#' sim_psy2(n = 100, seed = 123) %>%
#'  autoplot()
#'
#' # 200 periods with bubble origination dates 40/120 and termination dates 80/140
#' sim_psy2(n = 200, seed = 123) %>%
#'   autoplot()
sim_psy2 <- function(n, te1 = 0.2 * n, tf1 = 0.2 * n + te1,
                     te2 = 0.6 * n, tf2 = 0.1 * n + te2,
                     c = 1, alpha = 0.6, sigma = 6.79, seed = NULL) {
  assert_positive_int(n)
  assert_between(te1, 0, n)
  assert_between(tf1, te1, n)
  assert_between(te2, tf1, n)
  assert_between(tf2, te2, n)
  assert_between(alpha, 0, 1)
  stopifnot(sigma >= 0)

  set_rng(seed)

  delta <- 1 + c * n ^ (-alpha)
  y <- 100

  for (i in 2:n) {
    if (i < te1) {
      y[i] <- y[i - 1] + rnorm(1, sd = sigma)
    } else if (i >= te1 & i <= tf1) {
      y[i] <- delta * y[i - 1] + rnorm(1, sd = sigma)
    } else if (i == tf1 + 1) {
      y[i] <- y[te1] + rnorm(1, sd = sigma)
    } else if (i > tf1 + 1 & i < te2) {
      y[i] <- y[i - 1] + rnorm(1, sd = sigma)
    } else if (i >= te2 & i <= tf2) {
      y[i] <- delta * y[i - 1] + rnorm(1, sd = sigma)
    } else if (i == tf2 + 1) {
      y[i] <- y[te2] + rnorm(1, sd = sigma)
    } else {
      y[i] <- y[i - 1] + rnorm(1, sd = sigma)
    }
  }

  y %>%
    add_attr(seed = get_rng_state(seed)) %>%
    add_class("sim")
}

#' Simulation of a single-bubble process with multiple forms of collapse regime
#'
#' @description
#'
#' The new generating process considered here differs from the `sim_psy1` model in
#' three respects - Phillips and Shi (2018):
#'
#' \emph{First, it includes an asymptotically negligible drift in the martingale
#' path during normal periods. Second, the collapse process is modeled directly as
#' a transient mildly integrated process that covers an explicit period of market collapse.
#' Third, a market recovery date is introduced to capture the return to normal market behavior.
#' }
#' * `sudden:` with `beta = 0.1` and `tr =  tf + 0.01*n`
#' * `disturbing:` with `beta = 0.5` and `tr =  tf + 0.1*n`
#' * `smooth:` with `beta = 0.9` and `tr =  tf + 0.2*n`
#'
#' In order to provide the duration of the collapse period `tr` as `tr = tf + 0.2n`,
#' you have to provide `tf` as well.
#'
#'
#' @inheritParams sim_psy1
#' @param tr A scalar in  (tf, n) specifying the observation in which market recovers
#' @param c A positive scalar determining the drift in the normal market periods.
#' @param c1 A positive scalar determining the autoregressive coefficient in the explosive regime.
#' @param c2 A positive scalar determining the autoregressive coefficient in the collapse regime.
#' @param eta A positive scalar (>0.5) determining the drift in the normal market periods.
#' @param alpha A positive scalar in (0, 1) determining the autoregressive coefficient in the bubble period.
#' @param beta A positive scalar in (0, 1) determining the autoregressive coefficient in the collapse period.
#'
#'
#' @return A numeric vector of length \code{n}.
#'
#' @references Phillips, Peter CB, and Shu-Ping Shi. "Financial bubble implosion
#' and reverse regression." Econometric Theory 34.4 (2018): 705-753.
#'
#' @seealso \code{\link{sim_psy1}}
#' @export
#' @examples
#' # Disturbing collapse (default)
#' disturbing <- sim_ps1(100)
#' autoplot(disturbing)
#'
#' # Sudden collapse
#' sudden <- sim_ps1(100, te = 40, tf= 60, tr = 61, beta = 0.1)
#' autoplot(sudden)
#'
sim_ps1 <- function(n, te = 0.4 * n, tf = te + 0.2 * n , tr = tf + 0.1*n,
                    c = 1, c1 = 1, c2 = 1, eta = 0.6, alpha = 0.6, beta = 0.5,
                    sigma = 6.79, seed = NULL) {

  assert_positive_int(n)
  assert_between(te, 0, n)
  assert_between(tf, te, n)
  assert_between(tr, tf, n)
  assert_positive_int(c)
  assert_positive_int(c1)
  assert_positive_int(c2)
  assert_between(alpha, 0, 1)
  assert_between(beta, 0, 1)
  stopifnot(eta > 0.5, sigma >= 0)

  set_rng(seed)
  drift <- c*n^(-eta)
  delta <- 1 + c1 * n^(-alpha)
  gamma <- 1 - c2 * n^(-beta)
  y <- 100

  for (t in 2:n) {
    if (t < te) {
      y[t] <- drift + y[t - 1] + rnorm(1, sd = sigma)
    } else if (t >= te & t <= tf) {
      y[t] <- delta * y[t - 1] + rnorm(1, sd = sigma)
    } else if (t > tf & t <= tr ) {
      y[t] <- gamma * y[t - 1] + rnorm(1, sd = sigma)
    } else {
      y[t] <- drift + y[t - 1] + rnorm(1, sd = sigma)
    }
  }
  y %>%
    add_attr(seed = get_rng_state(seed)) %>%
    add_class("sim")
}


sim_ps2 <- function(n,
                    te1 = 0.2 * n, tf1 = te1 + 0.2 * n , tr1 = tf1 + 0.1*n,
                    te2 = 0.6 * n, tf2 = te2 + 0.15 * n , tr2 = tf2 + 0.1*n,
                    c = 1, c1 = 1, c2 = 1, eta = 0.6, alpha = 0.6, beta = 0.5,
                    sigma = 6.79, seed = NULL) {

  assert_positive_int(n)
  assert_between(te1, 0, n)
  assert_between(tf1, te1, n)
  assert_between(tr1, tf1, n)
  assert_between(te2, tf1, n)
  assert_between(tf2, te2, n)
  assert_between(tr2, tf2, n)
  assert_between(alpha, 0, 1)
  assert_positive_int(c)
  assert_positive_int(c1)
  assert_positive_int(c2)
  assert_between(alpha, 0, 1)
  assert_between(beta, 0, 1)
  stopifnot(eta > 0.5, sigma >= 0)

  set_rng(seed)
  drift <- c*n^(-eta)
  delta <- 1 + c1 * n^(-alpha)
  gamma <- 1 - c2 * n^(-beta)
  y <- 100

  for (t in 2:n) {
    if (t < te1) {
      y[t] <- drift + y[t - 1] + rnorm(1, sd = sigma) # normal
    } else if (t >= te1 & t <= tf1) {
      y[t] <- delta * y[t - 1] + rnorm(1, sd = sigma) # bubble1
    } else if (t > tf1 & t <= tr1 ) {
      y[t] <- gamma * y[t - 1] + rnorm(1, sd = sigma) # collapse 1
    }  else if (t > tr1 + 1 & t < te2) {
      y[t] <- drift + y[t - 1] + rnorm(1, sd = sigma) # normal 2
    }  else if (t >= te2 + 1 & t <= tf2) {
      y[t] <- delta * y[t - 1] + rnorm(1, sd = sigma) # bubble 2
    }  else if (t > tf2 + 1 & t <= tr2) {
      y[t] <- gamma * y[t - 1] + rnorm(1, sd = sigma) # collapse 2
    } else {
      y[t] <- drift + y[t - 1] + rnorm(1, sd = sigma) # normal 3
    }
  }
  y %>%
    add_attr(seed = get_rng_state(seed)) %>%
    add_class("sim")
}




#' Simulation of a Blanchard (1979) / Rotermann-Wilfling (2018) bubble process
#'
#' Simulation of a Blanchard (1979) rational bubble process, or (with
#' \code{type = "rotermann_wilfling"}) Rotermann & Wilfling (2018)'s
#' lognormal-mixture extension of it.
#'
#' @inheritParams sim_psy1
#' @param pi A positive value in (0, 1) which governs the probability of the bubble continuing to grow.
#' @param r A positive scalar that determines the growth rate of the bubble process.
#' @param b0 The initial value of the bubble.
#' @param type \code{"blanchard"} (default) or \code{"rotermann_wilfling"}. \code{r} is only used by \code{"blanchard"}; \code{delta}/\code{rw_sigma} only by \code{"rotermann_wilfling"} (see Details).
#' @param delta A scalar in (0, 1), the Rotermann-Wilfling deflation
#' parameter. Only used for \code{type = "rotermann_wilfling"}.
#' @param rw_sigma A positive scalar, the standard deviation (on the log
#' scale) of the Rotermann-Wilfling multiplicative lognormal shock. Only
#' used for \code{type = "rotermann_wilfling"}.
#'
#' @export
#' @return A numeric vector of length \code{n}.
#'
#' @importFrom stats rbinom rlnorm rt rcauchy pnorm
#' @details
#' Blanchard's bubble process (\code{type = "blanchard"}) has two regimes,
#' which occur with probability \eqn{\pi} and \eqn{1-\pi}.
#' In the first regime, the bubble grows exponentially, whereas in the second regime, the bubble
#' collapses to a white noise.
#'
#' With probability \eqn{\pi}:
#' \deqn{B_{t+1} = \frac{1+r}{\pi}B_t+\epsilon_{t+1}}{B[t+1]=(1+r)/\pi*B[t]+\epsilon[t+1],}
#' With probability \eqn{1 - \pi}:
#' \deqn{B_{t+1} = \epsilon_{t+1}}{B[t+1] = \epsilon[t+1],}
#'
#' where \code{r} is a positive constant and \eqn{\epsilon \sim iid(0, \sigma^2)}{\epsilon - iid(0, \sigma^2)}.
#'
#' Rotermann & Wilfling (2018)'s bubble (\code{type = "rotermann_wilfling"})
#' replaces the "collapse to white noise" regime with a *partial,
#' stochastically evolving* deflation, giving periodically recurring,
#' gradually-deflating trajectories instead of an abrupt one-period collapse:
#' \deqn{B_t = \frac{B_{t-1}u_t}{\delta}}{B[t] = B[t-1]*u[t]/delta} with probability \eqn{\pi}, or
#' \deqn{B_t = \frac{1-\pi\delta}{1-\pi}B_{t-1}u_t}{B[t] = (1-\pi*\delta)/(1-\pi) * B[t-1] * u[t]}
#' with probability \eqn{1-\pi}, where
#' \eqn{u_t \sim iid\,LN(-rw\_sigma^2/2,\ rw\_sigma^2)}{u[t] ~ iid LN(-rw_sigma^2/2, rw_sigma^2)}
#' (so \eqn{E[u_t] = 1}). \eqn{\delta \in (0, 1)} ensures the bubble never
#' collapses to exactly zero and can re-inflate.
#'
#' @references Blanchard, O. J. (1979). Speculative bubbles, crashes and rational expectations.
#' Economics letters, 3(4), 387-389.
#'
#' Rotermann, B. & Wilfling, B. (2018). "A new stochastic bubble process:
#' Theoretical properties and empirical tests." Applied Economics Letters,
#' 25(15), 1091-1096. As used for Monte Carlo power analysis in Monschang,
#' V. & Wilfling, B. (2021). "Sup-ADF-style bubble-detection methods under
#' test." Empirical Economics, 61, 145-172.
#'
#' @seealso \code{\link{sim_psy1}}, \code{\link{sim_psy2}}, \code{\link{sim_evans}}
#'
#' @examples
#' sim_blan(n = 100, seed = 123) %>%
#'   autoplot()
#'
#' sim_blan(n = 250, type = "rotermann_wilfling", delta = 0.984, seed = 123) %>%
#'   autoplot()
sim_blan <- function(n, pi = 0.7, sigma = 0.03, r = 0.05, b0 = 0.1,
                     type = c("blanchard", "rotermann_wilfling"),
                     delta = 0.984, rw_sigma = 0.05, seed = NULL) {
  type <- match.arg(type)
  assert_positive_int(n)
  assert_between(pi, 0, 1)
  stopifnot(sigma >= 0)
  stopifnot(r >= 0)

  set_rng(seed)

  if (type == "blanchard") {
    b <- b0
    theta <- rbinom(n, 1, pi)
    i <- 1
    while (i < n) {
      if (b[i] > 0) {
        if (theta[i] == 1) {
          b[i + 1] <- (1 + r) / pi * b[i] + rnorm(1, 0, sigma)
        } else {
          b[i + 1] <- rnorm(1, 0, sigma)
        }
        i <- i + 1
      } else {
        i <- i - 1
      }
    }
  } else {
    assert_between(delta, 0, 1)
    stopifnot(rw_sigma > 0)
    theta <- rbinom(n - 1, 1, pi)
    u <- rlnorm(n - 1, meanlog = -rw_sigma ^ 2 / 2, sdlog = rw_sigma)
    b <- b0
    for (i in seq_len(n - 1)) {
      b[i + 1] <- if (theta[i] == 1) {
        b[i] * u[i] / delta
      } else {
        ((1 - pi * delta) / (1 - pi)) * b[i] * u[i]
      }
    }
  }

  b %>%
    add_attr(seed = get_rng_state(seed)) %>%
    add_class("sim")
}

#' Simulation of an Evans (1991) bubble process
#'
#' Simulation of an Evans (1991) rational periodically collapsing bubble process.
#'
#' @inheritParams sim_blan
#' @param delta A positive scalar, with restrictions (see details).
#' @param tau The standard deviation of the innovations.
#' @param alpha A positive scalar, with restrictions (see details).
#' @param b1 A positive scalar, the initial value of the series. Defaults to \code{delta}.
#'
#' @return A numeric vector of length \code{n}.
#'
#' @importFrom stats rbinom
#'
#' @details
#'
#' \code{delta} and \code{alpha} are positive parameters which satisfy \eqn{0 < \delta < (1+r)\alpha}.
#' \code{delta} represents the size of the bubble after collapse.
#' The default value of \code{r} is 0.05.
#' The function checks whether \code{alpha} and \code{delta} satisfy this condition and will return an error if not.
#'
#' The Evans bubble has two regimes. If \eqn{B_t \leq \alpha}{B[t] \le \alpha} the bubble grows at an average rate of \eqn{1 + r}:
#'
#' \deqn{B_{t+1} = (1+r) B_t u_{t+1},}{B[t+1]= (1+r)*B[t]*u[t+1].}
#'
#' When \eqn{B_t > \alpha}{B[t] > \alpha} the bubble expands at the increased rate of \eqn{(1+r)\pi^{-1}}:
#'
#' \deqn{B_{t+1} =  [\delta + (1+r)\pi^{-1} \theta_{t+1}(B_t -  (1+r)^{-1}\delta B_t )]u_{t+1},}{B[t+1] = \delta*(1+r)/\pi* (B[t]-\delta/(1+r))) *u[t+1],}
#'
#' where \eqn{\theta} theta is a binary variable that takes the value 0 with probability \eqn{1-\pi} and 1 with probability \eqn{\pi}.
#' In the second phase, there is a (\eqn{1-\pi})  probability of the bubble process collapsing to \code{delta}.
#' By modifying the values of \code{delta}, \code{alpha} and \code{pi} the user can change the frequency at which bubbles appear, the mean duration of a bubble before collapse and the scale of the bubble.
#'
#' @export
#'
#' @seealso \code{\link{sim_psy1}}, \code{\link{sim_psy2}}, \code{\link{sim_blan}}
#'
#' @references Evans, G. W. (1991). Pitfalls in testing for explosive
#' bubbles in asset prices. The American Economic Review, 81(4), 922-930.
#'
#' @examples
#' sim_evans(100, seed = 123) %>%
#'   autoplot()
sim_evans <- function(n, alpha = 1, delta = 0.5, tau = 0.05, pi = 0.7,
                      r = 0.05, b1 = delta, seed = NULL) {

  # checks here
  assert_positive_int(n)
  stopifnot(alpha > 0)
  if (delta < 0 | delta > (1 + r) * alpha) {
    stop_glue("alpha and delta should satisfy: 0 < delta < (1+r)*alpha")
  }
  assert_between(pi, 0, 1)
  stopifnot(r >= 0)

  set_rng(seed)

  y <- rnorm(n, 0, tau)
  u <- exp(y - tau ^ 2 / 2)
  theta <- rbinom(n, 1, pi)
  b <- b1

  for (i in 1:(n - 1)) {
    if (b[i] <= alpha) {
      b[i + 1] <- (1 + r) * b[i] * u[i + 1]
    } else {
      b[i + 1] <- (delta + pi ^ (-1) * (1 + r) * theta[i + 1] * (b[i] -
        (1 + r) ^ (-1) * delta)) * u[i + 1]
    }
  }

  b %>%
    add_attr(seed = get_rng_state(seed)) %>%
    add_class("sim")
}

#' Simulation of dividends
#'
#' Simulate (log) dividends from a random walk with drift.
#'
#' @inheritParams sim_psy1
#' @param mu A scalar indicating the drift.
#' @param r A positive value indicating the discount factor.
#' @param log Logical. If true dividends follow a lognormal distribution.
#' @param output A character string giving the fundamental price("pf") or
#' dividend series("d"). Default is `pf'.
#'
#' @return A numeric vector of length n.
#' @export
#'
#' @details
#'
#' If log is set to FALSE (default value) dividends follow:
#'
#' \deqn{d_t = \mu + d_{t-1} + \epsilon_t}{d[t] = \mu + d[t-1] + \epsilon[t],}
#'
#' where \eqn{\epsilon \sim \mathcal{N}(0, \sigma^2)}{\epsilon - N(0, \sigma^2)}. The default parameters
#' are \eqn{\mu = 0.0373}, \eqn{\sigma^2 = 0.1574} and \eqn{d[0] = 1.3} (the initial value of the dividend sequence).
#' The above equation can be solved to yield the fundamental price:
#'
#' \deqn{F_t = \mu(1+r)r^{-2} + r^{-1}d_t}{F[t] = \mu * (1 + r)/r^2 + d[t]/r.}
#'
#' If log is set to TRUE then dividends follow a lognormal distribution or log(dividends) follow:
#'
#' \deqn{\ln(d_t) = \mu + \ln(d_{t-1}) + \epsilon_t}{ln(d[t]) = \mu + ln(d[t-1]) + \epsilon[t],}
#'
#' where \eqn{\epsilon \sim \mathcal{N}(0, \sigma^2)}{\epsilon - N(0, \sigma^2)}. Default parameters are
#' \eqn{\mu = 0.013}, \eqn{\sigma^2 = 0.16}. The fundamental price in this case is:
#'
#' \deqn{F_t = \frac{1+g}{r-g}d_t}{F[t] = (1 + g)/(r -g) * d[t],}
#'
#' where \eqn{1+g=\exp(\mu+\sigma^2/2)}{1 + g = exp(\mu + \sigma^2/2)}.
#' All default parameter values are those suggested by West (1988).
#'
#' @references West, K. D. (1988). Dividend innovations and stock price volatility.
#' Econometrica: Journal of the Econometric Society, p. 37-61.
#'
#' @examples
#' # Price is the sum of the bubble and fundamental components
#' # 20 is the scaling factor
#' pf <- sim_div(100, r = 0.05, output = "pf", seed = 123)
#' pb <- sim_evans(100, r = 0.05, seed = 123)
#' p <- pf + 20 * pb
#'
#' autoplot(p)
sim_div <- function(n, mu, sigma, r = 0.05,
                    log = FALSE, output = c("pf", "d"), seed = NULL) {
  initval <- 1.3
  # Values obtained from West(1988, p53)
  if (missing(mu)) if (log) mu <- 0.013 else mu <- 0.0373
  if (missing(sigma)) if (log) sigma <- sqrt(0.16) else sigma <- sqrt(0.1574)

  assert_positive_int(n)
  stopifnot(sigma >= 0)
  stopifnot(r >= 0)
  stopifnot(is.logical(log))
  return <- match.arg(output)

  set_rng(seed)
  d <- stats::filter(mu + c(initval, rnorm(n - 1, 0, sigma)),
    c(1),
    init = 1.3, method = "recursive"
  ) %>%
    as.numeric() # filter coerces to time-series

  if (log) {
    g <- exp(mu + sigma ^ 2 / 2) - 1
    pf <- (1 + g) * d / (r - g)
  } else {
    pf <- mu * (1 + r) * r ^ (-2) + d / r
  }

  out <- if (return == "pf") pf else d

  out %>%
    add_attr(seed = get_rng_state(seed)) %>%
    add_class("sim")

}

#' Simulation of a stochastic branching-tree bubble
#'
#' Simulation of Gourieroux & Jasiak (2025)'s stochastic-tree bubble process:
#' a positive stationary submartingale generated by a binomial tree with
#' *stochastic* branching intensity (a random-coefficient autoregression, as
#' opposed to Cox-Ross-Rubinstein's deterministic branches). Blanchard &
#' Watson (1982)'s bubble (\code{\link{sim_blan}}) is the special case of
#' *constant* intensity.
#'
#' The stochastic intensity is \eqn{p_t = \Phi(X_t)}{p[t] = Phi(X[t])}, with
#' \eqn{X_t}{X[t]} a latent stationary Gaussian AR(1):
#' \deqn{X_t = \mu + \rho(X_{t-1}-\mu) + \sigma\sqrt{1-\rho^2}u_t,\quad u_t \sim iid\,N(0,1)}{
#' X[t] = mu + rho*(X[t-1]-mu) + sigma*sqrt(1-rho^2)*u[t], u[t] ~ iid N(0,1)}
#' Given \eqn{p_t}{p[t]}, draw \eqn{Z_t \sim Bernoulli(p_t)}{Z[t] ~
#' Bernoulli(p[t])} and set (the paper's eq. 5-6):
#' \deqn{Y_t = \xi_{1t}Y_{t-1}+\epsilon_t,\quad
#' \xi_{1t}=\frac{1}{a}\frac{Z_t}{p_t},\quad
#' \epsilon_t=\frac{\eta}{1-a}\left(1-\xi_{1t}\right)+\frac{\eta}{a}\frac{1-Z_t}{1-p_t}}{
#' Y[t] = xi1[t]*Y[t-1] + eps[t], xi1[t] = (1/a)*(Z[t]/p[t]),
#' eps[t] = eta/(1-a)*(1-xi1[t]) + eta/a*(1-Z[t])/(1-p[t])}
#' \eqn{a>1} controls the growth rate in a branch's active phase, \eqn{\eta>0}
#' sets the price floor \eqn{\eta/(1-a)}{eta/(1-a)} (Corollary 1 in the
#' source: \eqn{Y_t \ge \eta/(1-a)}{Y[t] >= eta/(1-a)}), \eqn{\rho} controls
#' persistence of the bubble-growth phase, and \eqn{\sigma} controls the
#' frequency of bubbles. The process has no finite mean (the source's
#' Proposition 3) -- occasional very large values are a feature of the model,
#' not a bug.
#'
#' Default parameters (\eqn{\mu=-1,\eta=1,a=0.95,\sigma=4,\rho=0.7}{mu=-1,
#' eta=1, a=0.95, sigma=4, rho=0.7}) reproduce the source's own illustrative
#' example (Section 2.3, Figure 2).
#'
#' @param n A positive integer specifying the length of the simulated output series.
#' @param a A scalar in (0, 1) (note: \eqn{1/a > 1} is the growth rate).
#' @param eta A positive scalar setting the price floor \code{eta / (1 - a)}.
#' @param mu,rho,sigma Parameters of the latent Gaussian AR(1) intensity process (\code{rho} in (-1, 1), \code{sigma > 0}).
#' @param y0 Starting value. Defaults to the price floor \code{eta / (1 - a)}.
#' @inheritParams sim_psy1
#'
#' @return A numeric vector of length \code{n}.
#' @export
#'
#' @references Gourieroux, C. & Jasiak, J. (2025). "A Stochastic Tree for
#' Bubble Asset Modelling and Pricing." JTSA, 46(5), 932-944.
#'
#' @seealso \code{\link{sim_blan}}
#'
#' @examples
#' sim_tree(100, seed = 123) %>%
#'   autoplot()
sim_tree <- function(n, a = 0.95, eta = 1, mu = -1, rho = 0.7, sigma = 4,
                     y0 = eta / (1 - a), seed = NULL) {
  assert_positive_int(n)
  assert_between(a, 0, 1)
  stopifnot(eta > 0, sigma > 0)
  assert_between(rho, -1, 1)

  set_rng(seed)

  x <- numeric(n)
  x[1] <- mu
  if (n > 1) {
    u <- rnorm(n - 1)
    for (t in 2:n) x[t] <- mu + rho * (x[t - 1] - mu) + sigma * sqrt(1 - rho ^ 2) * u[t - 1]
  }
  # clip away from the exact 0/1 floating-point floor/ceiling -- an
  # unclipped p can round to exactly 0 or 1 in a long series' tail, making
  # xi1/eps below 0/0 or x/0, and one NaN then propagates through every
  # subsequent y[t]
  p <- pmin(pmax(pnorm(x), 1e-10), 1 - 1e-10)
  z <- rbinom(n, 1, p)
  xi1 <- z / (a * p)
  eps <- (eta / (1 - a)) * (1 - xi1) + (eta / a) * (1 - z) / (1 - p)

  y <- numeric(n)
  y[1] <- y0
  if (n > 1) for (t in 2:n) y[t] <- xi1[t] * y[t - 1] + eps[t]

  y %>%
    add_attr(seed = get_rng_state(seed)) %>%
    add_class("sim")
}

#' Simulation of a mixed causal-noncausal AR(1,1) bubble
#'
#' Simulation of Blasques, Koopman, Mingoli & Telg (2025)'s mixed
#' causal-noncausal autoregressive (MAR) bubble process, in which
#' transient, self-terminating local bubbles arise autonomously from the
#' *noncausal* (forward-looking) component -- no scripted origination/
#' collapse dates, unlike \code{\link{sim_psy1}}.
#'
#' \deqn{(1-\phi_1 L)(1-\psi_1 L^{-1})y_t = \epsilon_t}{(1 - phi1*L)(1 -
#' psi1*L^-1) y[t] = eps[t]}
#'
#' Simulated by the standard two-sided-filtering method for MAR processes
#' (Lanne & Saikkonen 2011; Gourieroux & Zakoian 2017): the noncausal
#' component is generated by running \eqn{u_t=\psi_1 u_{t+1}+\epsilon_t}{u[t]
#' = psi1*u[t+1] + eps[t]} *backward* from a zero boundary \code{burn}
#' observations past the end of the sample, then the causal component by
#' running \eqn{y_t=\phi_1 y_{t-1}+u_t}{y[t] = phi1*y[t-1] + u[t]} *forward*
#' from a zero boundary \code{burn} observations before the start; both
#' burn-in windows are then dropped.
#'
#' @param n A positive integer specifying the length of the simulated output series.
#' @param phi1 Causal AR coefficient, in (0, 1).
#' @param psi1 Noncausal AR coefficient, in (0, 1).
#' @param dist Innovation distribution: \code{"cauchy"} or \code{"t"} (with \code{df} degrees of freedom).
#' @param df Degrees of freedom if \code{dist = "t"}.
#' @param burn Non-negative burn-in length applied at *both* ends (see Details).
#' @inheritParams sim_psy1
#'
#' @return A numeric vector of length \code{n}.
#' @export
#'
#' @references Blasques, F., Koopman, S.J., Mingoli, G. & Telg, S. (2025).
#' "A Novel Test for the Presence of Local Explosive Dynamics." JTSA, 46(5), 966-980.
#'
#' @seealso \code{\link{sim_psy1}}
#'
#' @examples
#' sim_mar(200, seed = 123) %>%
#'   autoplot()
sim_mar <- function(n, phi1 = 0.7, psi1 = 0.7, dist = c("cauchy", "t"),
                    df = 2, burn = 100, seed = NULL) {
  dist <- match.arg(dist)
  assert_positive_int(n)
  assert_between(phi1, 0, 1)
  assert_between(psi1, 0, 1)
  stopifnot(burn >= 0)

  set_rng(seed)

  m <- n + 2 * burn
  eps <- if (dist == "cauchy") rcauchy(m) else rt(m, df)

  u <- numeric(m + 1)
  for (t in m:1) u[t] <- psi1 * u[t + 1] + eps[t]
  u <- u[1:m]

  y <- numeric(m)
  y[1] <- u[1]
  for (t in 2:m) y[t] <- phi1 * y[t - 1] + u[t]

  y[(burn + 1):(burn + n)] %>%
    add_attr(seed = get_rng_state(seed)) %>%
    add_class("sim")
}

#' Simulation of a latent common-factor bubble across multiple series
#'
#' Simulation of Chen, Phillips & Shi (2023)'s common-bubble DGP: \code{n_series}
#' observed series driven by one latent PSY-style bubble factor plus
#' idiosyncratic noise, \eqn{X_t = \Lambda f_t + e_t}{X[t] = Lambda*f[t] +
#' e[t]}, loadings \eqn{\Lambda \sim U[0,2]}{Lambda ~ U[0,2]}. Unlike every
#' other \code{sim_*} function, this returns a multi-column
#' \code{data.frame} (one column per series, exuber's standard panel input
#' shape) rather than a single numeric vector.
#'
#' @param n_series A positive integer, the number of observed series.
#' @inheritParams sim_psy1
#' @param sigma_e A non-negative scalar, the idiosyncratic-noise standard deviation.
#'
#' @return A \code{data.frame} with \code{n_series} numeric columns of length
#' \code{n}, plus a \code{"factor"} attribute holding the latent bubble
#' factor itself.
#' @export
#'
#' @references Chen, Y., Phillips, P.C.B. & Shi, S. (2023). "Common Bubble
#' Detection in Large Dimensional Financial Systems." Cowles Foundation
#' Discussion Paper.
#'
#' @seealso \code{\link{sim_psy1}}
#'
#' @examples
#' sim_common(n_series = 5, n = 100, seed = 123)
sim_common <- function(n_series, n, te = 0.4 * n, tf = 0.15 * n + te, c = 1,
                       alpha = 0.6, sigma = 6.79, sigma_e = 0.1, seed = NULL) {
  assert_positive_int(n_series)
  stopifnot(sigma_e >= 0)

  set_rng(seed)

  f <- sim_psy1(n, te = te, tf = tf, c = c, alpha = alpha, sigma = sigma)
  loadings <- runif(n_series, 0, 2)
  e <- matrix(rnorm(n * n_series, sd = sigma_e), nrow = n, ncol = n_series)
  x <- outer(as.numeric(f), loadings) + e
  colnames(x) <- paste0("series_", seq_len(n_series))

  as.data.frame(x) %>%
    add_attr(seed = get_rng_state(seed), factor = as.numeric(f))
}

#' Simulation of a bivariate co-explosive pair
#'
#' Simulation of Evripidou, Harvey, Leybourne & Sollis (2022)'s co-explosive
#' DGP: an explosive series \code{x} (from \code{\link{sim_psy1}}) and a
#' second series \code{y} linked to a lead/lagged copy of it (and optionally
#' a third, independent explosive series \code{z}),
#' \eqn{y_t = \mu_y + \phi_x x_{t-i} + \phi_z z_t + \epsilon_{y,t}}{y[t] =
#' mu_y + phi_x*x[t-i] + phi_z*z[t] + eps_y[t]}. \code{i > 0} means
#' \code{x}'s explosive episode leads \code{y}'s; \code{i < 0} means it lags.
#'
#' @inheritParams sim_psy1
#' @param lag Integer lead (\code{> 0}) or lag (\code{< 0}) of \code{x} relative to \code{y}; 0 = contemporaneous.
#' @param phi_x,phi_z,mu_y Linkage coefficients and intercept for \code{y}.
#' @param sigma_y A non-negative scalar, the standard deviation of \code{y}'s own noise.
#' @param x_args,z_args Named lists of extra arguments passed to the \code{\link{sim_psy1}} calls generating \code{x} and (if \code{phi_z != 0}) \code{z}.
#'
#' @return A \code{data.frame} with columns \code{x} and \code{y} (length \code{n}).
#' @export
#'
#' @references Evripidou, C., Harvey, D.I., Leybourne, S.J. & Sollis, R.
#' (2022). "Co-explosive behaviour in explosive financial bubbles." OBES.
#'
#' @seealso \code{\link{sim_psy1}}
#'
#' @examples
#' sim_coexplosive(n = 100, lag = 5, seed = 123)
sim_coexplosive <- function(n, lag = 0, phi_x = 1, phi_z = 0, mu_y = 0,
                            sigma_y = 6.79, x_args = list(), z_args = list(),
                            seed = NULL) {
  assert_positive_int(n)
  stopifnot(sigma_y >= 0)

  set_rng(seed)

  x <- as.numeric(do.call(sim_psy1, c(list(n = n), x_args)))
  z <- if (phi_z != 0) as.numeric(do.call(sim_psy1, c(list(n = n), z_args))) else rep(0, n)

  idx <- seq_len(n) - lag
  valid <- idx >= 1 & idx <= n
  x_lag <- rep(NA_real_, n)
  x_lag[valid] <- x[idx[valid]]

  y <- mu_y + phi_x * x_lag + phi_z * z + rnorm(n, sd = sigma_y)

  data.frame(x = x, y = y) %>%
    add_attr(seed = get_rng_state(seed), lag = lag)
}

#' Simulation of a Markov-switching present-value bubble
#'
#' Simulation of Chan & Santi (2021)'s bubble component of a present-value
#' state-space model: an AR(1) whose persistence switches between a
#' "surviving" (explosive) and a "collapsing" (mean-reverting) regime under
#' a first-order Markov chain, rather than at deterministic dates
#' (\code{\link{sim_psy1}}) or a fixed-probability mixture
#' (\code{\link{sim_blan}}).
#'
#' \deqn{b_t = \frac{1}{\lambda_{S_t}}b_{t-1}+\epsilon_t^b,\quad
#' \epsilon_t^b \sim iid\,N(0,\sigma_b^2)}{b[t] = (1/lambda[S[t]]) * b[t-1] +
#' eps_b[t], eps_b[t] ~ iid N(0, sigma_b^2)}
#' with \eqn{S_t \in \{1,2\}}{S[t] in {1,2}} a Markov chain with transition
#' probabilities \code{p11 = P(S[t]=1|S[t-1]=1)}, \code{p22 =
#' P(S[t]=2|S[t-1]=2)}. Regime 1 ("surviving") uses \code{lambda1 < 1}
#' (so \code{1/lambda1 > 1}, explosive); regime 2 ("collapsing") uses
#' \code{lambda2 > 1} (mean-reverting). Note: the source's own eq. 16 indexes
#' the coefficient by \eqn{S_{t+1}}{S[t+1]}; this implementation uses the
#' contemporaneous \eqn{S_t}{S[t]} instead (an indexing-convention
#' simplification, not a change to the qualitative Markov-switching
#' mechanism).
#'
#' @param n A positive integer specifying the length of the simulated output series.
#' @param p11,p22 Regime-1-to-1 and regime-2-to-2 transition probabilities, in (0, 1).
#' @param lambda1,lambda2 Regime persistence parameters (\code{lambda1 < 1} explosive, \code{lambda2 > 1} mean-reverting).
#' @param sigma_b A positive scalar, the bubble-innovation standard deviation.
#' @param b0 Starting value.
#' @param s0 Starting regime, \code{1L} or \code{2L}.
#' @inheritParams sim_psy1
#'
#' @return A numeric vector of length \code{n}, with a \code{"regime"} attribute (the simulated \code{S_t} path).
#' @export
#'
#' @references Chan, J.C.C. & Santi, C. (2021). "Speculative Bubbles in
#' Present-Value Models: A Bayesian Markov-Switching State Space Approach."
#' Journal of Economic Dynamics and Control, 127, 104101.
#'
#' @seealso \code{\link{sim_psy1}}, \code{\link{sim_blan}}
#'
#' @examples
#' sim_msbubble(200, seed = 123) %>%
#'   autoplot()
sim_msbubble <- function(n, p11 = 0.98, p22 = 0.90, lambda1 = 0.98,
                         lambda2 = 1.03, sigma_b = 0.05, b0 = 0, s0 = 1L,
                         seed = NULL) {
  assert_positive_int(n)
  assert_between(p11, 0, 1)
  assert_between(p22, 0, 1)
  stopifnot(sigma_b > 0, s0 %in% c(1L, 2L))

  set_rng(seed)

  s <- integer(n)
  s[1] <- s0
  if (n > 1) {
    unif <- runif(n - 1)
    for (t in 2:n) {
      s[t] <- if (s[t - 1] == 1L) {
        if (unif[t - 1] < p11) 1L else 2L
      } else {
        if (unif[t - 1] < p22) 2L else 1L
      }
    }
  }
  lambda <- c(lambda1, lambda2)[s]

  b <- numeric(n)
  b[1] <- b0
  if (n > 1) {
    eps <- rnorm(n - 1, sd = sigma_b)
    for (t in 2:n) b[t] <- (1 / lambda[t]) * b[t - 1] + eps[t - 1]
  }

  b %>%
    add_attr(seed = get_rng_state(seed), regime = s) %>%
    add_class("sim")
}

#' Simulation of a deterministic technology-adoption "false bubble" null
#'
#' Simulation of Chen, Chen, Huang, Li & Zhang (2026)'s false-bubble DGP: a
#' hump-shaped, *deterministic* technology-adoption shock embedded in
#' dividend growth, engineered so a Campbell-Shiller present-value
#' fundamental alone -- with **no bubble component at all** -- displays a
#' locally explosive-looking price path. Useful as a null (no-bubble) stress
#' test distinct from a plain random walk.
#'
#' Dividends follow a random walk with drift plus the technology hump:
#' \eqn{d_t = d_{t-1}+\mu+\tau_t+\eta_t}{d[t] = d[t-1] + mu + tau[t] +
#' eta[t]}. The hump \eqn{\tau_t}{tau[t]} rises linearly from \code{t1} to
#' \code{t1 + kappa} then falls linearly to \code{t2} (\code{shape =
#' "triangular"}, the source's own worked example, eq. 4), or follows a
#' Gaussian bump centered at \code{t1 + kappa} (\code{shape = "gaussian"}).
#' Because \eqn{\tau_t}{tau[t]} is deterministic (known in advance), its
#' contribution to the price is an exact forward-looking discounted sum,
#' \eqn{T_t=\sum_{s>t}\beta^{s-t}\tau_s}{T[t] = sum_{s>t} beta^(s-t)*tau[s]}
#' with \eqn{\beta=1/(1+r)}{beta = 1/(1+r)}, added to the same fundamental
#' pricing formula \code{\link{sim_div}} uses. This is a simplified,
#' single-shock reproduction of the source's mechanism (deterministic hump
#' -> hump-shaped fundamental price, no bubble), not its full DOLS/
#' multiple-functional-form robustness machinery.
#'
#' @param n A positive integer specifying the length of the simulated output series.
#' @param t1 Adoption (ramp-up start) date, in \code{1:n}.
#' @param t2 Maturation (shock end) date, in \code{t1:n}.
#' @param kappa Peak lag (time from \code{t1} to the hump's peak), in \code{0:(t2 - t1)}.
#' @param shape \code{"triangular"} (default) or \code{"gaussian"}.
#' @param amplitude A positive scalar scaling the hump's peak height.
#' @param mu A scalar, the baseline dividend-growth drift.
#' @param sigma_d A positive scalar, the dividend-growth innovation standard deviation.
#' @param r A positive scalar, the discount rate.
#' @param d0 Starting (log) dividend level.
#' @inheritParams sim_psy1
#'
#' @return A numeric vector of length \code{n} (the price), with
#' \code{"dividend"} and \code{"technology"} attributes.
#' @export
#'
#' @references Chen, H., Chen, L., Huang, D., Li, Y. & Zhang, Z. (2026).
#' "Technology Fundamentals and False Bubble Detection: Evidence from
#' Dot-Com and AI Episodes." arXiv:2604.25826.
#'
#' @seealso \code{\link{sim_div}}, \code{\link{sim_evans}}
#'
#' @examples
#' sim_falsebubble(200, seed = 123) %>%
#'   autoplot()
sim_falsebubble <- function(n, t1 = floor(0.3 * n), t2 = floor(0.7 * n),
                            kappa = floor((t2 - t1) / 2),
                            shape = c("triangular", "gaussian"),
                            amplitude = 1, mu = 0.02, sigma_d = 0.05,
                            r = 0.05, d0 = 0, seed = NULL) {
  shape <- match.arg(shape)
  assert_positive_int(n)
  assert_between(t1, 1, n)
  assert_between(t2, t1, n)
  stopifnot(kappa > 0, kappa < (t2 - t1), amplitude >= 0, r > 0, sigma_d > 0)

  set_rng(seed)

  tt <- seq_len(n)
  tau <- numeric(n)
  if (shape == "triangular") {
    up <- tt >= t1 & tt <= t1 + kappa
    down <- tt > t1 + kappa & tt <= t2
    tau[up] <- (tt[up] - t1) / kappa
    tau[down] <- (t2 - tt[down]) / (t2 - t1 - kappa)
  } else {
    peak <- t1 + kappa
    width <- (t2 - t1) / 4
    tau <- exp(-0.5 * ((tt - peak) / width) ^ 2)
    tau[tt < t1 | tt > t2] <- 0
  }
  tau <- amplitude * tau

  d <- numeric(n)
  d[1] <- d0
  if (n > 1) {
    eta <- rnorm(n - 1, sd = sigma_d)
    for (t in 2:n) d[t] <- d[t - 1] + mu + tau[t] + eta[t - 1]
  }

  beta <- 1 / (1 + r)
  bigt <- numeric(n)
  for (t in seq_len(n)) {
    future <- if (t < n) (t + 1):n else integer(0)
    if (length(future) > 0) bigt[t] <- sum(beta ^ (future - t) * tau[future])
  }

  pf <- mu * (1 + r) * r ^ (-2) + d / r
  p <- pf + bigt / r

  p %>%
    add_attr(seed = get_rng_state(seed), dividend = d, technology = tau) %>%
    add_class("sim")
}


# Methods -----------------------------------------------------------------


#' @export
print.sim <- function(x, ...) {
  attributes(x) <- NULL
  print(x)
}

#' @export
format.sim <- function(x, ...) {
  out <- signif(x, 3)
  out[is.na(x)] <- NA
  out
}

#' @export
#' @keywords internal
autoplot.sim <- function(object, ...) {
  object %>%
    enframe() %>%
    ggplot(aes(name, value)) +
    geom_line() +
    theme_exuber()
}
