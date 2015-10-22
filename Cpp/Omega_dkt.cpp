#include <RcppArmadillo.h>

// Below is a simple example of exporting a C++ function to R. You can
// source this function into an R session using the Rcpp::sourceCpp 
// function (or via the Source button on the editor toolbar)

// For more on using Rcpp click the Help button on the editor toolbar
// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::export]]
arma::mat Omega_dkt(arma::vec N_D_t, arma::mat Eta_t){

  int K = Eta_t.n_cols;
  int D_t = Eta_t.n_rows;
  
  arma::mat Omega_t(D_t,K);
  Omega_t.zeros();
  arma::mat expC = arma::sum( arma::exp( Eta_t ),1);
  for(int k = 0; k<K; k++){
    for(int d = 0; d<D_t; d++){
      double psi_dkt = Eta_t(d,k) - log( expC(d) - exp( Eta_t(d,k) ));
      
      // Now the Polya Gamma approximation
      double E_z = 0;
      double sigma2 = 0;
          
      if( psi_dkt == 0){
        E_z = 1.0 / 4.0;
        sigma2 = 1.0 /24.0;
      }else{
        E_z = 1.0 / ( 2 * psi_dkt ) * tanh( psi_dkt / 2 );
        sigma2 = 1.0 / ( 4 * pow( psi_dkt, 3 ) ) * (sinh( psi_dkt ) - psi_dkt ) * pow( 1 / cosh( psi_dkt / 2 ), 2 );
      }
      Omega_t(d,k) = R::rnorm( N_D_t(d)*E_z, sqrt( N_D_t(d) * sigma2 ) );
    }
  }
  
  return Omega_t;
}
