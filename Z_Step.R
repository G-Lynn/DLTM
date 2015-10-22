#' Z Step
#' 
#' This function samples the complete set of z_{n,d,t} topic assignments for all of the words in the corpus.  Additionally,
#' it populates the functional response matrices KAPPA_ETA, KAPPA_BETA, and ny which depend on the number of words assigned to each topic.   
#' @import parallel
#' @param MC.Cores Number of cores for parallelization
#' @param t.T Number of time points
#' @param N.D Time-indexed list where each element in the list is a vector of the word counts in 
#' the documents at time=t.
#' @param Beta_t Time-indexed list where each element in the list is a K x V matrix of state-parameters associated with each Beta_{k,v,t}
#' @param W Time-indexed list where each element in the list is a matrix of documents in the corpus.  The first column of the matrix are the words in the vocabulary corresponding to w_{n,d,t}.
#' The second column of the matrix is the document (d,t) from which each word comes.  
#' @param Eta_t Time-indexed list where each element in the list is a D_t x K matrix.  D_t is the number of documents at
#' time t.  Each element of the list Eta_{d,k,t} corresponds to the log-odds of the topic proportion for topic k, document d, and time t.
#' @return ZKappa_t A time-indexed list where each element of the list is itself a list.  From this list we construct
#' Kappa_Beta_k, Kappa_Eta_t, ny, and Z_t.  
#' 
#' @examples
#' 
#' K = 10
#' t.T = 25
#' a2 = .01
#' ZKappa = Z_Step(MC.Cores = 2, t.T, N.D, Beta_t, W, Eta_t)

Z_Step<-function(MC.Cores,t.T,N.D, Beta_t, W, Eta_t){
  return( mclapply(1:t.T, function(t) Z_ndt(N.D[[t]],Beta_t[[t]] ,W[[t]], Eta_t[[t]] ), mc.cores= MC.Cores ) )
}
