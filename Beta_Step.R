#' Beta Step
#' 
#' This function samples the complete set of Beta_{k,t} V x 1 vectors for the K independent state-space
#' models that model the dynamic probabilities over the vocabulary.  
#' @import parallel
#' @param MC.Cores Number of cores for parallelization
#' @param K Number of topics
#' @param m_beta_kv0 Prior mean for each beta_{k,v,0}.
#' @param sigma2_beta_kv0 Prior variance for each beta_{k,v,0}
#' @param sigma2 Variance of beta_{k,v,t} | beta_{k,v,t-1}
#' @param Kappa_Beta_k Topic-indexed list where each element of the list is a V x t.T matrix of functional responses y_kv - .5*n_k as defined in paper
#' @param Zeta_k Topic-indexed list where each element of the list is a V x t.T matrix of Polya-Gamma auxiliary random variables Zeta_{k,v,t}.  
#' @param Beta_k Topic-indexed list where each element of the list is a V x T matrix of state-parameters where each entry corresponds to Beta_{k,v,t}.
#' Sampling for Beta_{k,v,t} is conditional all others (Beta_{k,-v,t}) so it is a necessary input to the function and one that must be updated at each iteration
#' inside the function for conditioning on the most current values
#' @return Beta_k Topic-indexed list where each element of the list is a V x T matrix of state-parameters where each entry corresponds to Beta_{k,v,t}.
#' @examples 
#' 
#' K = 10
#' t.T = 25
#' Beta_k = Beta_Step(MC.Cores = 2, K, m_beta_kv0 = 0, sigma2_beta_kv0 =.01, sigma2 =.001, Kappa_Beta_k, Zeta_k, Beta_k)



Beta_Step<-function(MC.Cores, K, fnames_Beta, beta_index, m, B, thin, m_beta_kv0, sigma2_beta_kv0, sigma2, Kappa_Beta_k, Zeta_k, Beta_k){
  return( mclapply(1:K, function(k) Beta_k_FFBS(fnames_Beta[k],beta_index,m,B,thin,m_beta_kv0, sigma2_beta_kv0, sigma2, Kappa_Beta_k[[k]], Zeta_k[[k]], Beta_k[[k]]) , mc.cores=MC.Cores ) )
}
