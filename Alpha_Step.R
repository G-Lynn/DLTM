#' Alpha Step
#' 
#' This function samples the complete set of alpha_{k,t} p x 1 vectors for the K independent DLMs
#' that model the topic proportions in documents.  
#' @import parallel
#' @param MC.Cores Number of cores for parallelization
#' @param K Number of topics
#' @param t.1 the first time point
#' @param t.T Number of points in time-series
#' @param a2 Variance of Eta_kt | Alpha_kt
#' @param delta Discount factor for computing sequence of Sigma_k,t matrices
#' @param m_alpha_k0 Prior mean of alpha_k.  A p x 1 vector
#' @param C_alpha_k0 Prior covariance matrix of alpha_k.  A p x p covariance matrix
#' @param FF Time-indexed List where each element of the list is a D_t x p design matrix for state-space model on Eta.
#' @param G A p x p system matrix for specific dynamic behavior including trends, periodic behavior, etc
#' @param Eta_t Time-indexed list where each element in the list is a D_t x K matrix.  D_t is the number of documents at
#' time t.  Each entry in the matrix corresponds to Eta_{d,k,t}, the log-odds of the topic proportion for topic k, document d, and time t.
#' @return Alpha_k Topic-indexed List where each element of the list is a p x t.T matrix where each column of the matrix 
#' corresponds to the state vector Alpha_{k,t}. 
#' @examples 
#' K = 10 #number of topics
#' t.T = 25 #number of time points
#' p = 2  #dimension of the state space for Alpha
#' m_alpha_k0 = matrix(c(0,0), nrow = 2, ncol = 1)  #prior mean  
#' C_alpha_k0 = .01*diag(p) #prior covariance matrix
#' delta = .95 #Discount factor for computing sequence of Sigma_{k,t} variance matrices
#' a2 = .01  #variance of eta | alpha is a^2
#' ALPHA =  Alpha_Step(MC.Cores = 2, K, t.T, a2, delta, m_alpha_k0, C_alpha_k0, FF, G, Eta_k )

Alpha_Step<-function(MC.Cores, K, fnames_Alpha, m, B, thin, t.T, a2, delta, m_alpha_k0, C_alpha_k0, FF, G, Eta_k){
  return( mclapply(1:K, function(k) Alpha_k_FFBS(fnames_Alpha[k], m, B, thin, t.T, a2, delta[[k]], m_alpha_k0[[k]], C_alpha_k0[[k]], FF, G[[k]], Eta_k[[k]]),mc.cores = MC.Cores ) )
}
