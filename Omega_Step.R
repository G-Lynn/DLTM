#' Omega Step
#' 
#' This function samples the complete set of Omega_{d,k,t} auxiliary Polya-Gamma random variables
#' for the data augmentation of topic proportions Eta_{d,k,t}.  
#' @import parallel
#' @param MC.Cores Number of cores for parallelization
#' @param t.T Number of time points
#' @param N.D Time-indexed list where each element in the list is a vector of the word counts in 
#' the documents at time=t.
#' @param Eta_t Time-indexed list where each element in the list is a D_t x K matrix.  D_t is the number of documents at
#' time t.  Each entry in the matrix corresponds to Eta_{d,k,t}, the log-odds of the topic proportion for topic k, document d, and time t.
#' @return Omega_t Time-indexed List where each element of the list is a D_t x K matrix.  D_t is the number of documents at time t.
#' Each entry in the matrix corresponds to the auxilary Omega_{d,k,t} Polya-Gamma random variables for data augmentation with Eta_{d,k,t}. 

#' @examples
#' 
#' K = 10
#' t.T = 25
#' a2 = .01
#' p = 2
#' Omega_t = Omega_Step(MC.Cores = 2, t.T, N.D, Eta_t)

Omega_Step<-function(MC.Cores, t.T,N.D, Eta_t){
  return( mclapply(1:t.T, function(t) Omega_dkt(N.D[[t]], Eta_t[[t]] ), mc.cores = MC.Cores ) )
}
