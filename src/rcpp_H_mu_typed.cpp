#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]


double H_mu_typed(NumericMatrix mu, NumericMatrix d_X, NumericMatrix d_Y) {
      double value=0;
      value = 0;
      for (int i = 0; i <d_X.nrow(); i++) {
            for (int ii = 0; ii < d_X.nrow(); ii++) {
                  for (int j = 0; j < d_Y.nrow(); j++) {
                        for (int jj = 0; jj < d_Y.nrow(); jj++) {
                              value = value + mu(i, j) * mu(ii, jj) * 
                                    fabs(d_X(i, ii) - d_Y(j, jj));
                        };
                  };
            };
      };
      return(value);
}      

