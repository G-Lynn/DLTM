#include <RcppArmadillo.h>
using namespace Rcpp;

// Below is a simple example of exporting a C++ function to R. You can
// source this function into an R session using the Rcpp::sourceCpp 
// function (or via the Source button on the editor toolbar)

// For more on using Rcpp click the Help button on the editor toolbar

// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::export]]

arma::mat Eta_dkt(const std::string& fname, arma::vec index, int m, int B, int thin, double a2, arma::mat FF_t, arma::mat Alpha_t, arma::mat OMEGA_t, arma::mat Kappa_t, arma::mat Eta_t){
  int D_t = Eta_t.n_rows;
  int K = Eta_t.n_cols;
  
  for(int d = 0; d<D_t; d++){
    double expC = arma::sum( arma::exp( Eta_t.row(d) ) );
    for(int k = 0; k<K; k++){
      int kk = index(k)-1;
      double expCMk = expC - exp( Eta_t(d,kk) );
      
      double lambda_dk = 1.0/( OMEGA_t(d,kk) + 1.0/a2); 
      arma::mat F_alpha = FF_t.row(d)*Alpha_t.col(kk);
      
      double q_dk = lambda_dk*( Kappa_t(d,kk) + OMEGA_t(d,kk)*log(expCMk) + (1.0/a2)*F_alpha(0) );
      Eta_t(d,kk) = R::rnorm(q_dk, sqrt(lambda_dk) );
      expC = expCMk + exp(Eta_t(d,kk));
    }
  }

  std::ofstream outFile;  // construct an iostream for writing the sampled values of Beta
  if( (m>B) && (m%thin==0) ) outFile.open( fname.c_str(), std::ofstream::app);  // only open the steam if post-burnin 
  
  for(int d = 0; d<D_t; d++){
    if( (m>B) && (m%thin==0) ) outFile << d + 1;  // write the doc number to lead each row
    
    for(int k=0; k<K; k++){
      double eta_constrained = Eta_t(d,k) - Eta_t(d,K-1);

      if( (m>B) && (m%thin==0) ) {  // only write if the file if post burnin
        outFile << "," << eta_constrained;  // write \Eta_{d,k,t} separated by commas
      }
      
    }
    outFile << std::endl;
  }
  
  if( (m>B) && (m%thin==0) ) outFile.close(); // close iostream
  return Eta_t;
}
