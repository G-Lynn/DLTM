#include <RcppArmadillo.h>
#include <fstream>
#include <iostream>
#include <string>

// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::export]]
arma::mat Alpha_k_FFBS(const std::string& fname, int mm, int B, int thin, int t_T, double a2, arma::vec delta, arma::vec m0, arma::mat C0, Rcpp::List FF, arma::mat GG, Rcpp::List Eta_k){
  int p = C0.n_rows;
  //create vectors for m, a
  arma::mat Alpha_k(p,t_T);
  arma::mat m(p,t_T);
  arma::mat a(p,t_T);
  
  Rcpp::List C;
  Rcpp::List R_inv;  
  
  double a2_inv = (1.0/a2);
  std::ofstream outFile;
  if( (mm>B) && (mm % thin ==0) ) outFile.open( fname.c_str(), std::ofstream::app);
  
  
  for(int t = 0; t<t_T; t++){
  
    arma::mat F_t = FF[t];
    arma::vec Eta_kt = Eta_k[t];
    arma::mat P_star(p,p);
  
    if(t == 0 ){
      a.col(t) = GG*m0;
      P_star = GG*C0*GG.t();
    }else{
      arma::mat C_tm1 = C[t - 1];
      a.col(t) = GG*m.col(t - 1);
      P_star = GG*C_tm1*GG.t();
    }
    
    //Rcpp::Rcout << "C_{t-1}: " << P_star << std::endl;
    arma::mat P_inv = P_star.i();
    arma::vec f_t = F_t * a.col(t);
    arma::mat R_t = P_star;    //if i were to think about not discounting it would be here  R_t = P_t + W_t
    R_t.diag() = R_t.diag() + delta;
    arma::mat R_t_inv = R_t.i();
    R_inv.push_back( R_t_inv );      // and here.  W_t = diag( (1-delta)/delta C_{t-1}[1,1], .00001)
    
    //Rcpp::Rcout << "R_t: " << R_t << std::endl;

    arma::mat Q_t = F_t * R_t * F_t.t();    
    Q_t.diag() = Q_t.diag() + a2;
    
    //Woodbury Sherman Matrix Inversion
    arma::mat Q_inv_t = - a2_inv * F_t * arma::inv(R_t_inv + a2_inv * F_t.t() * F_t ) * a2_inv * F_t.t();
    Q_inv_t.diag() = Q_inv_t.diag() + a2_inv;
    
    arma::mat A_t = R_t * F_t * Q_inv_t;
    //Rcpp::Rcout << "A_t: " << A_t << std::endl;
    //Rcpp::Rcout << "R_t: " << R_t << std::endl;
    arma::mat C_t = R_t - A_t * Q_t * A_t.t();
    C.push_back(C_t);
    
    m.col(t) = a.col(t) + A_t * (Eta_kt - f_t );
  }
  
  //now do backward sampling

  for(int t = (t_T - 1); t>=0; t--){
    arma::mat C_t = C[t];
    
    if(t == (t_T - 1) ){
      Alpha_k.col(t) = m.col(t) + arma::trans( arma::randn(1,p) * arma::chol(C_t) );
    }else{
      arma::mat R_inv_tp1 = R_inv[t + 1];
      arma::vec b = m.col(t) + C_t*GG.t()* R_inv_tp1 * (Alpha_k.col(t + 1) - a.col( t + 1) );
      arma::mat B = C_t - C_t*GG.t()*R_inv_tp1*GG*C_t;
      Alpha_k.col(t) = b + arma::trans( arma::randn(1,p) * arma::chol(B) );
    }
  }//end of backward sampling
  
  if( (mm>B) && (mm % thin ==0) ){
      for(int i = 0; i<p; i++){
        outFile << (i+1) <<",";
        for(int t = 0; t<(t_T-1); t++){
            outFile << Alpha_k(i,t) << ","; 
        }
        outFile<<Alpha_k(i,(t_T-1) ) << std::endl;
      }
    }
    
  if( ( mm>B ) && (mm % thin ==0) ) outFile.close();
  return Alpha_k;  
}
