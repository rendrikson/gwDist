#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]

NumericMatrix mat_G(NumericMatrix d_X, NumericMatrix d_Y) {
      
                  NumericMatrix G(d_X.nrow()*d_Y.nrow(),d_X.nrow()*d_Y.nrow());
                  for (int i = 0; i < d_X.nrow(); i++) {
                  for (int j = 0; j < d_Y.nrow(); j++) {
                  for (int ii = 0; ii < d_X.nrow(); ii++) {
                  for (int jj = 0; jj < d_Y.nrow(); jj++) {
                  G(i*d_Y.nrow()+j,ii*d_Y.nrow()+jj) = fabs(d_X(i, ii) - d_Y(j, jj));
                  };
                  };
                  };
                  };
                  return G;
                  }
      




