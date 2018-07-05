// Multiple Linear Regression Recursive Least Squares
// Simon Spavound 2018
//' @export
// [[Rcpp::depends(RcppArmadillo)]]
#include <RcppArmadillo.h>

using namespace Rcpp;
//using namespace arma;
//using namespace std;

// [[Rcpp::export]]

RcppExport SEXP rls_gsadf_cpp(arma::mat yxmat, SEXP min_win) {
  int start = as<int>(min_win);
  int end = yxmat.n_rows;
  //probably need to remove the following 1 - 1 has been removed now negative 2
  int total = end - start + 1;
  //int ncols = yxmat.n_cols;

  //be careful with counting from 0 here
  arma::mat x = yxmat.cols(1, yxmat.n_cols - 1);
  arma::mat y = yxmat.col(0);
  //Rcout << "end = " << end << std::endl;

  //removing minus one here
  int nc = yxmat.n_cols - 1;
  //manually changing this
  arma::mat tstat(end + 1, total);
  tstat.fill(arma::datum::nan);

  arma::mat sx;
  arma::mat sy;
  arma::mat tsx;
  arma::mat g;
  arma::mat b;
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
        arma::colvec tsxn = trans(x.row(start + i - 1));
        arma::mat syn = y.row(start + i - 1);
        arma::rowvec sxn = trans(tsxn);
        double kaka = 1 / (1 + as_scalar(sxn * g * tsxn));
        g -= kaka * ((g * tsxn) * (sxn * g));
        b -= g * tsxn * as_scalar(sxn * b - syn);
      }
    arma::mat res = sy - sx * b;
    arma::mat sqres = sum(square(res));
    arma::mat vares = sqres/(start+i-j-nc);
    arma::mat sb = sqrt(as_scalar(vares) * diagvec(g));
    tstat(start + i - 1, j) = (b(1) - 1)/ sb(1);
    }
  }

  arma::mat tstatadj = tstat.rows(0, end - 1);

  double adf = tstat(end - 1, 0);
  arma::vec badf = tstatadj.col(0);
  double sadf = max(badf);
  arma::vec bsadf = max(tstatadj, 1);
  double gsadf = max(bsadf);

  return Rcpp::List:: create(Rcpp::Named("adf")=adf,
                             Rcpp::Named("tstat")=tstatadj,
                             Rcpp::Named("badf")=badf,
                             Rcpp::Named("sadf")=sadf,
                             Rcpp::Named("bsadf")=bsadf,
                             Rcpp::Named("gsadf")=gsadf);
}

