#pragma once

#include <armadillo>

namespace exubercore {

// Recursive least-squares ADF/SADF/GSADF/BSADF test statistics (Phillips,
// Shi & Yu 2015), computed via the matrix inversion lemma to avoid
// re-inverting the regressor cross-product at every window.
//
// yxmat: column 0 is the dependent variable (levels), remaining columns are
// the regressors (constant, lag, lagged differences) as produced by
// exuber's `unroot()`. min_win: minimum regression window size. lag: ADF
// lag length (0 selects the closed-form single-regressor path).
//
// Returns a vector of length 2*total + 3, where total = nrow(yxmat) -
// min_win + 1:
//   [0, total)            badf
//   [total]                adf
//   [total + 1]            sadf
//   [total + 2]            gsadf
//   [total + 3, 2*total+3) bsadf
//
// Throws std::invalid_argument if min_win is not a valid window size for
// yxmat.
arma::vec radf(const arma::mat& yxmat, int min_win, int lag = 0);

} // namespace exubercore
