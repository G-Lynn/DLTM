#include <Rcpp.h>
using namespace Rcpp;

// Below is a simple example of exporting a C++ function to R. You can
// source this function into an R session using the Rcpp::sourceCpp 
// function (or via the Source button on the editor toolbar)

// For more on using Rcpp click the Help button on the editor toolbar

// [[Rcpp::export]]

Rcpp::NumericVector rpgApproxCpp(int nSamples, Rcpp::NumericVector n, Rcpp::NumericVector z){
  Rcpp::NumericVector PG(nSamples);
  int nr = n.size();
  double sigma2 = 0;
  double E_z = 0;
  
  for(int i = 0; i<nSamples; i++){
    if(nr == 1){
      
      if( z(0) == 0 ){ 
        
        E_z = 1.0/4.0;
        sigma2 = 1.0/24.0;
        
      }else{
        E_z = 1.0 / ( 2 * z(0) ) * tanh( z(0) / 2 );
        sigma2 = 1.0 / ( 4 * pow( z(0), 3 ) ) * (sinh( z(0) ) - z(0) ) * pow( 1 / cosh( z(0) / 2 ), 2 );
      }
      
      PG(i) = R::rnorm( n(0)*E_z, sqrt( n(0) * sigma2 ) );
      
    }else{
      if( z(i) == 0){
        E_z = 1.0 / 4.0;
        sigma2 = 1.0 /24.0;
      }else{
        E_z = 1.0 / ( 2 * z(i) ) * tanh( z(i) / 2 );
        sigma2 = 1.0 / ( 4 * pow( z(i), 3 ) ) * (sinh( z(i) ) - z(i) ) * pow( 1 / cosh( z(i) / 2 ), 2 );
      }
      
      PG(i) = R::rnorm( n(i)*E_z, sqrt( n(i) * sigma2 ) );
    }

  }

  return PG;
}