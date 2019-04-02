#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericVector compoundIntWithDeposits(double initial, NumericVector rates, NumericVector deposits) {
  int n_balances = rates.size() + 1;
  NumericVector results(n_balances);
  results[0] = initial;
  for (int i=0; i < n_balances; ++i) {
    results[i + 1] = deposits[i] + results[i] * (1.0 + rates[i]);
  }
  return results;
}

/*** R
library("purrr")
set.seed(3)
# https://www.ifa.com/articles/with_stock_returns_normally_distributed/
volatility_params <- list(
    "IFA-5"   = list(mean = 0.32, sd = 0.53)
  , "IFA-50"  = list(mean = 0.60, sd = 2.24)
  , "IFA-100" = list(mean = 0.90, sd = 4.53)
  , "S&P"     = list(mean = 0.91, sd = 4.10)
)

deposits <- rep(1500, 300)
rates <- map(volatility_params, ~append(., list(n = 300))) %>%
  map(~do.call(rnorm, args = .)) %>%
  map(~ . / 100)

lk <- map(rates, ~compoundIntWithDeposits(initial = 0, rates = ., deposits = deposits))
*/
