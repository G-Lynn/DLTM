#include <RcppArmadillo.h>
using namespace Rcpp;

// Below is a simple example of exporting a C++ function to R. You can
// source this function into an R session using the Rcpp::sourceCpp 
// function (or via the Source button on the editor toolbar)

// For more on using Rcpp click the Help button on the editor toolbar

// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::export]]

int Eta_Check(double a2, arma::mat FF_t, arma::mat Alpha_t, arma::mat OMEGA_t, arma::mat Kappa_t, arma::mat Eta_t){
  int D_t = 1;
  int K = 3;
  
  for(int d = 0; d<D_t; d++){
    
    double expC = arma::sum( arma::exp( Eta_t.row(d) ) );

    
    for(int k = 0; k<K; k++){
      double expCMk = expC - exp( Eta_t(d,k) );
      double lambda_dk = 1.0/( OMEGA_t(d,k) + 1.0/a2);
      Rcpp::Rcout << "lambda: " << lambda_dk << std::endl;
      arma::mat F_alpha = FF_t.row(d)*Alpha_t.col(k);
      
      Rcpp::Rcout << "Kappa_dtk: " << Kappa_t(d,k) << std::endl;
      Rcpp::Rcout << "OMEGA*log(expCMk): " << OMEGA_t(d,k)*log(expCMk) << std::endl;
      Rcpp::Rcout << "F_alpha: " << F_alpha << std::endl;
      
      double q_dk = lambda_dk*( Kappa_t(d,k) + OMEGA_t(d,k)*log(expCMk) + (1.0/a2)*F_alpha(0) );
      Rcpp::Rcout << "q_dk: " << q_dk << std::endl;
      Eta_t(d,k) = R::rnorm(q_dk, sqrt(lambda_dk) );
      expC = expCMk + exp(Eta_t(d,k));
      
    }
    Rcpp::Rcout << "Eta_td: " << Eta_t.row(d) << std::endl;
  }
  
  return 0;
}
