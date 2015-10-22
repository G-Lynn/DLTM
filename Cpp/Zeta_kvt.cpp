#include <RcppArmadillo.h>

// Below is a simple example of exporting a C++ function to R. You can
// source this function into an R session using the Rcpp::sourceCpp 
// function (or via the Source button on the editor toolbar)

// For more on using Rcpp click the Help button on the editor toolbar
// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::export]]
arma::mat Zeta_kvt(arma::vec N_y_k, arma::mat Beta_k){

  int V = Beta_k.n_rows;
  int t_T = Beta_k.n_cols;
  
  arma::mat Zeta_k(V,t_T);
  Zeta_k.zeros();
  
  arma::mat expC = arma::sum( arma::exp(Beta_k),0);
  for(int t = 0; t<t_T; t++){
    for(int v = 0; v<V; v++){
      double gamma_kvt = Beta_k(v,t) - log( expC(t) - exp( Beta_k(v,t) ));
      //if(t==0){
      //  Rcpp::Rcout << v << << " 1: " << Beta_k(v,t) << "; 2: " << expC(t) << "; 3: " << exp(Beta_k(v,t)) << std::endl;
      //}
      // Now the Polya Gamma approximation
      double E_z = 0;
      double sigma2 = 0;
          
      if( gamma_kvt == 0){
        E_z = 1.0 / 4.0;
        sigma2 = 1.0 /24.0;
      }else{
        E_z = 1.0 / ( 2 * gamma_kvt ) * tanh( gamma_kvt / 2 );
        sigma2 = 1.0 / ( 4 * pow( gamma_kvt, 3 ) ) * (sinh( gamma_kvt ) - gamma_kvt ) * pow( 1 / cosh( gamma_kvt / 2 ), 2 );
      }
      Zeta_k(v,t) = R::rnorm( N_y_k(t)*E_z, sqrt( N_y_k(t) * sigma2 ) );
    }
  }
  return Zeta_k;
}
