#include <Rcpp.h>
using namespace Rcpp;

//' Expected Survival
//'
//' Calculates Expected Survival Probability or Restricted Mean Survival Time (RMST)
//'
//' @param lp vector of linear predictors
//' @param surv_rate vector of survival times
//' @export
//'
//' @return if surv_rate is of length 1 returns probability of survival, if surv_rate is a vector resturns RMST
//'
//' @examples
//' lps <- c(0, 1, -.5)
//' ## Expected Survival
//' expected_survival(lps, 0.85)
//' ## Restricted Mean Survival Time
//' expected_survival(lps, seq(1,.85, -.0005))
// [[Rcpp::export]]
NumericVector expected_survival(NumericVector lp, NumericVector surv_rate) {
   int n = surv_rate.size();
   int m = lp.size();
   NumericVector z(m);
   for(int j = 0; j < m; ++j){
     for(int i = 0; i < n; ++i){
       z[j] += pow(surv_rate[i], exp(lp[j]));
     }
   }
   return z;
 }
