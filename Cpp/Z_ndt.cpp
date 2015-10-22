#include <Rcpp.h>
using namespace Rcpp;

// Below is a simple example of exporting a C++ function to R. You can
// source this function into an R session using the Rcpp::sourceCpp 
// function (or via the Source button on the editor toolbar)

// For more on using Rcpp click the Help button on the editor toolbar

// [[Rcpp::export]]

Rcpp::List Z_ndt(NumericVector N_D_t, NumericMatrix Beta_t, NumericMatrix W_t, NumericMatrix Eta_t ){
  
  int K = Eta_t.ncol();
  int D_t = Eta_t.nrow();
  int V = Beta_t.ncol();
  
  int nr = W_t.nrow();
  //I use the Rcpp vectors and matrices so that everything is consistent and compatible 
  // with the Rcpp::sample function.
  Rcpp::List Retx; 
  
  Rcpp::NumericVector Prob(K);
  Rcpp::NumericVector CumProb(K);
  
  Rcpp::NumericMatrix Y_kv(K,V);  
  Rcpp::NumericMatrix X_dk(D_t,K);
  
  Rcpp::NumericVector Z(nr);
  Rcpp::NumericVector U(nr);
  U = Rcpp::runif(nr);
  
  Rcpp::NumericVector Beta_Sum(K);
  for(int k = 0; k<K; k++) Beta_Sum(k) = Rcpp::sum( exp( Beta_t(k,_) ) );
  
  for(int i=0; i<nr; i++){ 
    int term_index = W_t(i,0) - 1;
    int doc_index =  W_t(i,1) - 1;
    
    for(int k = 0; k<K; k++){  
      Prob(k) = exp(Eta_t(doc_index,k))*exp(Beta_t(k,term_index))/Beta_Sum(k); 
      
      if(k==0){
        CumProb(k) = Prob(k);
      }
      else{
        CumProb(k) = CumProb( k - 1) + Prob(k);
      }
      
    }

    // this is my own R::sample function based on uniform draws and the inverse CDF  
    
    for(int k = 0; k<K; k++){
      Prob(k) = Prob(k)/CumProb(K - 1);
      CumProb(k) = CumProb(k)/CumProb(K - 1);

      if( (k == 0) && ( U(i)<CumProb(k) ) ){
        Z(i) = k+1;
        break;
      }
      else if( ( U(i) > CumProb(k-1) ) && ( U(i)<CumProb(k) ) ){
        Z(i) = k+1;
        break;
      }
    }
  
    int top_index = Z(i) - 1;
    Y_kv(top_index,term_index)++;
    X_dk(doc_index,top_index)++;
  }
  
  Retx.push_back(Z);

  NumericMatrix Kappa_Beta(K,V);
  IntegerVector ny(K);
  for(int k = 0; k<K; k++){
    ny(k) = Rcpp::sum( Y_kv(k,_) );
    Kappa_Beta(k,_) = Y_kv(k,_) - .5*ny(k);
  }

  Retx.push_back(Kappa_Beta);
  
  NumericMatrix Kappa_Alpha(D_t,K);
  for(int d = 0; d<D_t; d++) Kappa_Alpha(d,_) = X_dk(d,_) - .5*N_D_t(d);
  Retx.push_back(Kappa_Alpha);
  Retx.push_back(ny);

  
  return Retx;
}
