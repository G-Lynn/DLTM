#' Zeta Step
#' 
#' This function samples the complete set of Zeta_{k,v,t} auxiliary Polya-Gamma random variables
#' for the data augmentation of Vocabulary term state-space parameters Beta_{k,v,t}.
#' @import parallel
#' @param MC.Cores Number of cores for parallelization
#' @param K the number of topics
#' @param ny K x t.T matrix of the total number of words in corpus assigned to each topic k at time t
#' @param Beta_k Topic-indexed list where each element of the list is a V x t.T matrix of state-parameters Beta_{k,v,t}
#' @return Zeta_k Topic-indexed list where each element of the list is a V x t.T matrix of Polya-Gamma auxiliary random variables Zeta_{k,v,t}.  
#' @examples 
#' 
#' K = 10 #number of topics
#' t.T = 25 #number of time points
#' a2 = .01  #variance of eta | alpha is a^2
#' ZKappa = Z_Step(MC.Cores = 2, t.T, N.D, Beta_t, W, Eta_t)
#' ny = matrix(nrow = K, ncol = t.T)
#' for(t in 1:t.T) ny[,t] = ZKappa[[t]][[4]]
#' Zeta_k = Zeta_Step(MC.Cores = 2, K,ny,Beta_k)



Zeta_Step<-function(MC.Cores, K, V, ny, ny_thresh, Beta_k){
  return(mclapply(1:K, function(k) Zeta_kvt(ny[k,],Beta_k[[k]]),mc.cores = MC.Cores ))
}
