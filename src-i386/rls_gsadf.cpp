//' @export
// [[Rcpp::depends(RcppArmadillo)]]
#include <RcppArmadillo.h>

using namespace Rcpp;
using namespace arma;

// [[Rcpp::export]]
arma::vec rls_gsadf(const arma::mat & yxmat, int min_win, int lag = 0) {

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
