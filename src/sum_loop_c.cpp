#include <Rcpp.h>
using namespace Rcpp;

//' Sum loop C
//'
//' This function loops over the calculate mean survival
//'
//' @param x vector of linear predictors
//' @param y vector of survival times
//' @export
// [[Rcpp::export]]
NumericVector sum_loop_c(NumericVector x, NumericVector y) {
  int n = y.size();
  int m = x.size();
  NumericVector z(m);
  for(int j = 0; j < m; ++j){
    for(int i = 0; i < n; ++i){
      z[j] += pow(y[i], exp(x[j]));
    }
  }
  return z;
}
