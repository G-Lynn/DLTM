#include <RcppArmadillo.h>
#include <fstream>
#include <iostream>
#include <string>

// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::export]]
arma::mat Beta_k_FFBS(const std::string& fname, arma::vec index, int m, int B, int thin, double m_beta_kv0, double sigma2_beta_kv0, double sigma2, arma::mat Kappa_k, arma::mat Zeta_k, arma::mat Beta_k){
  
  int t_T = Beta_k.n_cols;  // extract the number of time points from the columns of V x t.T matrix \beta_k
  int V = Beta_k.n_rows;    // extract the number of terms in vocabulary from the rows of V x t.T matrix \beta_k

  
  arma::mat expC = arma::sum(arma::exp(Beta_k),0);  // vector for the \sum_{j}^{V} \beta_{k,j,t}
  
  for(int v = 0; v<V; v++){ // loop for each vocabulary term (v) separately
    // Begin Forward Filtering
    int vv = index(v)-1;
    arma::vec m_kv(t_T);  // declare vector for posterior mean of Beta_{k,v,t} | Beta_{k,-v,t}, Zeta_{k,v,t}, Kappa_Beta...
    arma::vec sigma2_kv(t_T); // declare vector  for posterior variance of Beta_{k,v,t} | Beta_{k,-v,t} 
    arma::mat expCMv = expC - arma::exp(Beta_k.row(vv) ); // subtract the vector (in t) exp(Beta_{k,v,t}) so that we have \sum_{j \neq v} exp(Beta_{k,j,t})
    arma::mat c = arma::log( expCMv );  // Define C = \log \sum_{j \neq v} exp(\beta_{k,j,t})
    
    
    // t = 1
    //What about a discount factor approach here?
    double rho2 = sigma2_beta_kv0 + sigma2;  // \rho2 = \sigma^2_{k,v,t}
    sigma2_kv(0) = 1/( Zeta_k(vv,0) + 1/rho2 ); // time 1 posterior variance as defined in the paper
    m_kv(0) = sigma2_kv(0)*( Kappa_k(vv,0) + Zeta_k(vv,0)*c(0) + m_beta_kv0/rho2 ); // time 1 posterior mean as defined in the paper
    //Rcpp::Rcout << v << "," << 0 << "; 1: " << Kappa_k(v,0) << "; 2a " << Zeta_k(v,0) << "; 2b " << c(0) << "; 3: " << m_beta_kv0/rho2 <<std::endl;
    
    // Forward Filtering for t = 2:t_T
    for(int t = 1; t<t_T; t++){
      rho2 = sigma2_kv(t-1) + sigma2;  // time t forecasted variance
      sigma2_kv(t) = 1/( Zeta_k(vv,t) + 1/rho2 ); // time t posterior variance
      m_kv(t) = sigma2_kv(t)*( Kappa_k(vv,t) + Zeta_k(vv,t)*c(t) + m_kv(t-1)/rho2 ); // time t posterior mean 
      //Rcpp::Rcout << v << "," << t << "; 1: " << Kappa_k(v,t) << "; 2: " << Zeta_k(v,t)*c(t) << "; 3: " << m_kv(t-1)/rho2 <<std::endl;
      //Rcpp::Rcout << v << "," << t <<  "; mean: " << m_kv(t) << "; variance: " << sigma2_kv(t) <<std::endl;
    }

    // Now begin Backward sampling
    Beta_k(vv,t_T-1) = R::rnorm( m_kv(t_T-1), sqrt( sigma2_kv(t_T-1) ) );    
    
    for(int t = (t_T-2); t>=0; t--){
      double sigma2_tilde = 1/( 1/sigma2 + 1/sigma2_kv(t) ); // backward sampling variance
      double m_tilde = sigma2_tilde*( Beta_k(vv,t+1)/sigma2 + m_kv(t)/sigma2_kv(t) ); //backward sampling mean
      Beta_k(vv,t) = R::rnorm(m_tilde, sqrt(sigma2_tilde) );
    }
    
    expC = expCMv + arma::exp(Beta_k.row(vv) ); // reconstruct expC = \sum_{j=1}^V e^{\beta_{k,j,t}} = \sum_{j \neq v} e^{\beta_{k,j,t}} + e^{\beta_{k,v,t}}
    
  }
  
  std::ofstream outFile;  // construct an iostream for writing the sampled values of Beta
  if( (m>B) && (m % thin == 0) ) outFile.open( fname.c_str(), std::ofstream::app);  // only open the steam if post-burnin 
  
  for(int v = 0; v<V; v++){
    
    if( (m>B) && (m%thin ==0) ) outFile << v + 1;
    
    for(int t = 0; t<t_T; t++){
      double beta_constrained = Beta_k(v,t) - Beta_k( (V-1), t ); //
      
      if( (m>B) && (m%thin ==0) ) outFile << "," << beta_constrained;  // write \beta_{k,v,t} separated by commas
    }
    
    if( (m>B) && (m%thin ==0) ) outFile << std::endl;  // each each line at t=t.T
  }
  
  if( (m>B) && (m%thin==0) ) outFile.close();   // close iostream
  
  return Beta_k;
}
