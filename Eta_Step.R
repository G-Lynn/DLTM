#' Eta Step
#' 
#' This function samples the complete set of Eta_{d,k,t} log-odds topic proportions for all topics k in all documents d
#' at all time points 1:T
#' @import parallel
#' @param MC.Cores Number of cores used in parallelization
#' @param K Number of topics
#' @param a2 Variance of Eta_{d,k,t} | Alpha_{k,t}
#' @param D Vector which contains number of documents in corpus at each time point
#' @param FF Time-indexed List where each element of the list is a D_t x p design matrix for state-space model on Eta.
#' @return Alpha_t Time-indexed List where each element of the list is a p x K matrix where each column of the matrix 
#' corresponds to the state vector Alpha_{k,t}.
#' @param Omega_t Time-indexed List where each elelemtn of the list is a D_t x K matrix.  D_t is the number of documents at time t.
#' Each entry in the matrix corresponds to the auxilary Omega_{d,k,t} Polya-Gamma random variables for data augmentation with Eta_{d,k,t}. 
#' @param Kappa_Eta_t Time-indexed List where each element of the list is a D_t x K matrix.  D_t is the number of documents at 
#' time t.  Each entry in the matrix corresponds to the functional response Kappa_{d,k,t} for each document and topic at time t.
#' @param Eta_t Time-indexed list where each element in the list is a D_t x K matrix.  D_t is the number of documents at
#' time t.  Each entry in the matrix corresponds to Eta_{d,k,t}, the log-odds of the topic proportion for topic k, document d, and time t.
#' @return Eta_t Time-indexed list where each element in the list is a D_t x K matrix.  D_t is the number of documents at
#' time t.  Each entry in the matrix corresponds to Eta_{d,k,t}, the log-odds of the topic proportion for topic k, document d, and time t.
#' @examples 
#' K = 10
#' t.T = 25
#' a2 = .01
#' Eta_t = Eta_Step(MC.Cores = 2, K,a2,D,FF,Alpha_t,Omega_t,Kappa_Eta_t,Eta_t)

Eta_Step<-function(MC.Cores, K,fnames_Eta, eta_index, m, B, thin, a2,D,FF,Alpha_t,Omega_t,Kappa_Eta_t,Eta_t){
  return( mclapply(1:t.T, function(t) Eta_dkt(fnames_Eta[t], eta_index, m, B, thin, a2,FF[[t]],Alpha_t[[t]],Omega_t[[t]], Kappa_Eta_t[[t]], Eta_t[[t]]),mc.cores = MC.Cores ) )
}

