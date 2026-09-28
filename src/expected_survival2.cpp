#include <Rcpp.h>
#include <RcppParallel.h>
using namespace Rcpp;
using namespace RcppParallel;

// [[Rcpp::depends(RcppParallel)]]

// Worker struct: one thread computes a chunk of lp values
struct ExpectedSurvivalWorker : public Worker {
  const RVector<double> lp;
  const RVector<double> surv_rate;
  RVector<double> output;

  ExpectedSurvivalWorker(const NumericVector& lp,
                         const NumericVector& surv_rate,
                         NumericVector& output)
    : lp(lp), surv_rate(surv_rate), output(output) {}

  void operator()(std::size_t begin, std::size_t end) {
    int n = surv_rate.length();
    for (std::size_t j = begin; j < end; ++j) {
      double exp_lp = std::exp(lp[j]);
      double acc = 0.0;
      for (int i = 0; i < n; ++i)
        acc += std::pow(surv_rate[i], exp_lp);
      output[j] = acc;
    }
  }
};

// [[Rcpp::export]]
NumericVector expected_survival(NumericVector lp, NumericVector surv_rate) {
  int m = lp.size();
  NumericVector output(m);

  ExpectedSurvivalWorker worker(lp, surv_rate, output);
  parallelFor(0, m, worker);

  return output;
}
