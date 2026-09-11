//' @export
// [[Rcpp::depends(RcppArmadillo)]]
#include <RcppArmadillo.h>
#include "vendor/exubercore/include/exubercore/radf.hpp"

// [[Rcpp::export]]
arma::vec rls_gsadf(const arma::mat & yxmat, int min_win, int lag = 0) {
  try {
    return exubercore::radf(yxmat, min_win, lag);
  } catch (const std::exception& e) {
    Rcpp::stop(e.what());
  }
}

// [[Rcpp::export]]
arma::vec rls_nested(const arma::mat & yxmat, const arma::ivec & minw, int n_min, int lag = 0) {
  try {
    return exubercore::radf_nested(yxmat, minw, n_min, lag);
  } catch (const std::exception& e) {
    Rcpp::stop(e.what());
  }
}
