#include <Rcpp.h>
using namespace Rcpp;

// This is a simple example of exporting a C++ function to R. You can
// source this function into an R session using the Rcpp::sourceCpp 
// function (or via the Source button on the editor toolbar). Learn
// more about Rcpp at:
//
//   http://www.rcpp.org/
//   http://adv-r.had.co.nz/Rcpp.html
//   http://gallery.rcpp.org/
//

// [[Rcpp::export]]
List getIntervalBalance(
    NumericVector initial_balance
  , NumericMatrix interest
  , NumericVector payments
) {
  // each loan gets a row, each column is a slot for a payment to that loan
  int n_loans    = initial_balance.size();
  int n_balances = payments.size();
  NumericMatrix balances(n_loans, n_balances + 1);
  NumericMatrix paid(n_loans, n_balances);
  
  // the first column is the initial balance
  for (int i=0; i < n_loans; ++i) {
    balances(i, 0) = initial_balance[i];
  }
  
  // iterate over the payments
  for (int j=0; j < n_balances; j++) {
    
    NumericVector c_balances = balances(_, j);
    
    // if we have no payments left for any loan then we'll exit... the
    // NumericMatricies already have 0's in any "untouched" cells
    if (is_true(all(c_balances == 0))) {
      break;
    }
    
    // calculate the payment based on the relative size of the loans just like
    // Great Lakes does... 
    // TODO(mac): build functionality for "minimum payment" handling
    NumericVector 
      pay_share  = c_balances / sum(c_balances),
      pay_amount = pay_share * payments[j];
    
    // if the allocated amount is too much, we need to deduct the overpay
    NumericVector overpay  = abs(pmin(c_balances - pay_amount, 0));
    NumericVector actual_pay = round(pay_amount - overpay, 2);
    
    // create the next balances
    NumericVector n_balances = c_balances - actual_pay;
    
    // if there is overpay, redistribute to the rest of the non-zero loans
    if (sum(overpay) < 0) {
      NumericVector overpay_share  = n_balances / sum(n_balances);
      NumericVector redist_overpay = round(overpay * overpay_share, 2);
      actual_pay = actual_pay + redist_overpay;
      
      n_balances = n_balances - redist_overpay;
    }
    
    // record the actual amount paid
    paid(_, j) = actual_pay;
    
    // calculate the interest (after payment) and record
    balances(_, j + 1) = round(n_balances * (1.0 + interest(_, j)), 2);
  } // for-loop
  
  return List::create(Named("balances") = balances , Named("payments") = paid);
}


/*** R
loans <- c(100, 200, 40, 5)
pymts <- c(50, 50, 100, 150, 200)
interests <- matrix(rep(0.05, length(loans) * length(pymts)), nrow = length(loans))
getIntervalBalance(loans, interests, pymts)
*/
