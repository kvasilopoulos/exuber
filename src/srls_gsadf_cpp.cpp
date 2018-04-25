//' @export
#include <Rcpp.h>

// [[Rcpp::plugins("cpp11")]]

using namespace Rcpp;
using namespace std;
// [[Rcpp::export]]
RcppExport SEXP srls_gsadf_cpp(SEXP ayy, SEXP axx, SEXP amin_win) {
  NumericVector yy(ayy);
  NumericVector xx(axx);
  int min_win = as<int>(amin_win);
  int n = yy.size();
  int reps = n - min_win + 1;
  NumericMatrix tstat(n, reps);
  std::fill(tstat.begin(), tstat.end(), -999);
  double sx;
  double sy;
  double sxx;
  double sxy;
  for (int j = 0; j < reps; ++j) {
    sx = sum(xx[seq(j, j + min_win - 1)]);
    sy = sum(yy[seq(j, j + min_win - 1)]);
    sxx = sum(xx[seq(j, j + min_win - 1)] * xx[seq(j, j + min_win - 1)]);
    sxy = sum(xx[seq(j, j + min_win - 1)] * yy[seq(j, j + min_win - 1)]);
    for (int i = j ; i < reps; ++i) {
      if (i == j) {
      } else {
        sx += xx[min_win + i - 1];
        sy += yy[min_win + i - 1];
        sxx += (xx[min_win + i - 1] * xx[min_win + i - 1]);
        sxy += (yy[min_win + i - 1] * xx[min_win + i - 1]);
      }
      int T = min_win + i - j;
      double meanx = sx/T;
      double meany = sy/T;
      double den = sxx/T-meanx*meanx;
      double beta = (sxy/T-meanx*meany)/den;
      double alpha = meany-beta*meanx;
      NumericVector u = yy[seq(j, min_win + i - 1)] - alpha - beta*xx[seq(j, min_win + i - 1)];
      double suu = sum(u*u);
      double sbeta = sqrt(suu/(T-2)/den/T);
      tstat(min_win + i - 1, j) = (beta - 1)/ sbeta;
    }
  }

  NumericVector adf_temp = tstat( n-1, _ );
  double adf = adf_temp(0);
  NumericVector badf = tstat( _, 0 );
  double sadf = max(badf);
  NumericVector bsadf(n);
  for(int i = 0; i < n; i++) {
    bsadf[i] = max(tstat(i, _));
  }
  double gsadf = max(bsadf);

  NumericVector results(2*n+3);
  results[seq(0, n-1)] = bsadf;
  results(n)=sadf;
  results(n+1)=gsadf;
  results(n+2)=adf;
  results[seq(n+3,2*n+2)]=badf;
  return results;
}
