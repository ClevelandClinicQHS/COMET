#include <Rcpp.h>
using namespace Rcpp;

//' Mean Survival
//'
//' This function calculates mean survival
//'
//' @param lp vector of linear predictors
//' @param surv_rates vector of survival times
//' @export
//'
//' @examples
//' lps <- c(0, 1, -.5)
//' surv_rates <- seq(1,.85, -.0005)
//' mean_survival(lps, surv_rates)
// [[Rcpp::export]]
NumericVector mean_survival(NumericVector lp, NumericVector surv_rates) {
   int n = surv_rates.size();
   int m = lp.size();
   NumericVector z(m);
   for(int j = 0; j < m; ++j){
     for(int i = 0; i < n; ++i){
       z[j] += pow(surv_rates[i], exp(lp[j]));
     }
   }
   return z;
 }
