#include "exubercore/radf.hpp"

#include <algorithm>
#include <cmath>
#include <stdexcept>
#include <vector>

namespace exubercore {

using namespace arma;

arma::vec radf(const arma::mat& yxmat, int min_win, int lag) {
  if (lag < 0) {
    throw std::invalid_argument("lag must be non-negative");
  }
  if (min_win < 2 || min_win > static_cast<int>(yxmat.n_rows)) {
    throw std::invalid_argument("min_win must be between 2 and nrow(yxmat)");
  }
  if (lag > 0 && static_cast<int>(yxmat.n_cols) < lag + 2) {
    throw std::invalid_argument("yxmat must have at least lag + 2 columns when lag > 0");
  }

  int start = min_win;
  int end = yxmat.n_rows;
  int total = end - start + 1;

  arma::mat tstat = zeros<mat>(total, total);
  tstat.fill(arma::datum::nan);

  if (lag == 0) {

    arma::vec y = yxmat.col(0);
    arma::vec x = yxmat.col(1);

    arma::vec u;
    double sx, sy, sxx, sxy;
    int T;
    double meanx, meany, den, beta, alpha, sbeta, suu;

    for (int j = 0; j < total; ++j) {
      sx = sum(x.rows(j, j + start - 1));
      sy = sum(y.rows(j, j + start - 1));
      sxx = sum(x.rows(j, j + start - 1) % x.rows(j, j + start - 1));
      sxy = sum(x.rows(j, j + start - 1) % y.rows(j, j + start - 1));
      for (int i = j ; i < total; ++i) {
        if (i == j) {
        } else {
          // Sum here to coerce from vec to double -- not needed n = 1
          sx += sum(x.row(start + i - 1));
          sy += sum(y.row(start + i - 1));
          sxx += sum(x.row(start + i - 1) % x.row(start + i - 1));
          sxy += sum(y.row(start + i - 1) % x.row(start + i - 1));
        }
        T = start + i - j;
        meanx = sx/T;
        meany = sy/T;
        den = sxx/T-meanx*meanx;
        beta = (sxy/T-meanx*meany)/den;
        alpha = meany-beta*meanx;
        u = y.rows(j, start + i - 1) - alpha - beta*x.rows(j, start + i - 1);
        suu = as_scalar(trans(u) * u);
        sbeta = sqrt(suu/(T-2)/den/T);
        tstat(i, j) = (beta - 1)/ sbeta;
      }
    }
  }else{
    //removing minus one here ~ the dependent variable
    int nc = yxmat.n_cols - 1;

    arma::mat x = yxmat.cols(1, nc);
    arma::mat y = yxmat.col(0);

    arma::mat sx, sy, tsx, g, b, syn, res, sb;
    double kaka, sqres, vares;
    arma::colvec tsxn;
    arma::rowvec sxn;

    for (int j = 0; j < total; ++j) {
      sx = x.rows(j, start + j - 1);
      sy = y.rows(j, start + j - 1);
      tsx = sx.t();
      g = inv(tsx * sx);
      b = g * tsx * sy;
      for (int i = j ; i < total; ++i) {
        if (i == j) {
        } else {
          sx = x.rows(j, start + i - 1);
          sy = y.rows(j, start + i - 1);
          tsxn = trans(x.row(start + i - 1));
          syn = y.row(start + i - 1);
          sxn = trans(tsxn);
          kaka = 1 / (1 + as_scalar(sxn * g * tsxn));
          g -= kaka * ((g * tsxn) * (sxn * g));
          b -= g * tsxn * as_scalar(sxn * b - syn);
        }
        res = sy - sx * b;
        sqres = as_scalar(trans(res) * res);
        vares = sqres/(start+i-j-nc);
        sb = sqrt(vares * diagvec(g));
        tstat(i, j) = (b(1) - 1)/ sb(1);
      }
    }
  }

  double adf = tstat(total-1, 0);
  arma::vec badf = tstat.col(0);
  double sadf = max(badf);
  arma::colvec bsadf = max(tstat, 1);
  double gsadf = max(bsadf);

  arma::vec results(2*total+3);
  results.rows(0, total-1) = badf;
  results.row(total)=adf;
  results.row(total+1)=sadf;
  results.row(total+2)=gsadf;
  results.rows(total+3,2*total+2)=bsadf;
  return results;
}

arma::vec radf_nested(const arma::mat& yxmat, const arma::ivec& minw, int n_min, int lag) {
  if (lag < 0) throw std::invalid_argument("lag must be non-negative");
  const int K = static_cast<int>(minw.n_elem);
  if (K < 1) throw std::invalid_argument("minw must be non-empty");
  const int R = static_cast<int>(yxmat.n_rows);
  const int N = n_min + K - 1;
  if (R != N - 1 - lag) {
    throw std::invalid_argument("nrow(yxmat) must equal n_min + length(minw) - 2 - lag");
  }
  const int nc = static_cast<int>(yxmat.n_cols) - 1;  // regressors
  if (lag > 0 && nc < lag + 2) {
    throw std::invalid_argument("yxmat must have at least lag + 2 columns when lag > 0");
  }
  const int mmin = minw.min();
  const int need = (lag == 0) ? 3 : nc + 1;  // T - (#params) >= 1 for the variance
  if (mmin < need) throw std::invalid_argument("min(minw) is too small for the regression");
  for (int k = 0; k < K; ++k) {
    const int Rn = n_min + k - 1 - lag;
    if (minw(k) < mmin || Rn - minw(k) < 0) {
      throw std::invalid_argument("minw[k] must leave at least one window inside n = n_min + k");
    }
  }

  // Distinct window sizes, so the per-m prefix maxima are tracked once each.
  arma::ivec ms = arma::unique(minw);
  const int M = static_cast<int>(ms.n_elem);
  std::vector<int> m_index(minw.max() + 1, -1);
  for (int a = 0; a < M; ++a) m_index[ms(a)] = a;

  arma::vec w0(R);  w0.fill(arma::datum::nan);
  // pmax(e) = max over starts j' <= j (so far) of W(j', e).
  arma::vec pmax(R);  pmax.fill(-arma::datum::inf);
  // gm(a, e) = W with window >= ms(a) ending at e, maximised over starts:
  // i.e. the bsadf sequence for window ms(a).
  arma::mat gm(M, R);  gm.fill(-arma::datum::inf);

  if (lag == 0) {
    const arma::vec y = yxmat.col(0);
    const arma::vec x = yxmat.col(1);
    for (int j = 0; j + mmin - 1 < R; ++j) {
      double sx = 0, sy = 0, sxx = 0, sxy = 0, syy = 0;
      for (int e = j; e < R; ++e) {
        sx += x(e); sy += y(e); sxx += x(e) * x(e); sxy += x(e) * y(e); syy += y(e) * y(e);
        const int T = e - j + 1;
        if (T < mmin) continue;
        const double sxx_c = sxx - sx * sx / T;
        const double sxy_c = sxy - sx * sy / T;
        const double syy_c = syy - sy * sy / T;
        const double beta = sxy_c / sxx_c;
        const double ssr = syy_c - beta * sxy_c;
        const double t = (beta - 1) / std::sqrt(ssr / (T - 2) / sxx_c);
        if (j == 0) w0(e) = t;
        if (t > pmax(e)) pmax(e) = t;
      }
      for (int a = 0; a < M; ++a) {
        const int e = j + ms(a) - 1;
        if (e < R) gm(a, e) = pmax(e);
      }
    }
  } else {
    // Plain arrays and hand-written nc x nc updates: the per-window work is
    // a handful of tiny matrix-vector products, and Armadillo temporaries
    // (heap allocations, BLAS calls) per window cost more than the flops.
    const arma::mat x = yxmat.cols(1, nc);
    const arma::vec y = yxmat.col(0);
    std::vector<double> xtx(nc * nc), xty(nc), g(nc * nc), b(nc), gx(nc), xe(nc);
    for (int j = 0; j + mmin - 1 < R; ++j) {
      std::fill(xtx.begin(), xtx.end(), 0.0);
      std::fill(xty.begin(), xty.end(), 0.0);
      double yty = 0;
      for (int e = j; e < R; ++e) {
        for (int a = 0; a < nc; ++a) xe[a] = x(e, a);
        const double ye = y(e);
        for (int a = 0; a < nc; ++a) {
          xty[a] += xe[a] * ye;
          for (int c = 0; c < nc; ++c) xtx[a * nc + c] += xe[a] * xe[c];
        }
        yty += ye * ye;
        const int T = e - j + 1;
        if (T < mmin) continue;
        if (T == mmin) {
          const arma::mat ginv = arma::inv_sympd(arma::mat(xtx.data(), nc, nc));
          for (int a = 0; a < nc; ++a)
            for (int c = 0; c < nc; ++c) g[a * nc + c] = ginv(a, c);
        } else {
          // Sherman-Morrison rank-1 update of (X'X)^-1, as radf() does.
          double denom = 1.0;
          for (int a = 0; a < nc; ++a) {
            double acc = 0;
            for (int c = 0; c < nc; ++c) acc += g[a * nc + c] * xe[c];
            gx[a] = acc;
            denom += xe[a] * acc;
          }
          for (int a = 0; a < nc; ++a)
            for (int c = 0; c < nc; ++c) g[a * nc + c] -= gx[a] * gx[c] / denom;
        }
        double ssr = yty;
        for (int a = 0; a < nc; ++a) {
          double acc = 0;
          for (int c = 0; c < nc; ++c) acc += g[a * nc + c] * xty[c];
          b[a] = acc;
          ssr -= acc * xty[a];
        }
        const double t = (b[1] - 1) / std::sqrt(ssr / (T - nc) * g[1 * nc + 1]);
        if (j == 0) w0(e) = t;
        if (t > pmax(e)) pmax(e) = t;
      }
      for (int a = 0; a < M; ++a) {
        const int e = j + ms(a) - 1;
        if (e < R) gm(a, e) = pmax(e);
      }
    }
  }

  // gsadf for n = sup over ends e <= R_n - 1 of the window-m(n) bsadf.
  for (int a = 0; a < M; ++a) {
    for (int e = 1; e < R; ++e) {
      if (gm(a, e - 1) > gm(a, e)) gm(a, e) = gm(a, e - 1);
    }
  }
  arma::vec out(R + K);
  out.rows(0, R - 1) = w0;
  for (int k = 0; k < K; ++k) {
    const int Rn = n_min + k - 1 - lag;
    out(R + k) = gm(m_index[minw(k)], Rn - 1);
  }
  return out;
}

} // namespace exubercore
