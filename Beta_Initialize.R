#' Beta Initialization
#' 
#' This function initializes the dynamic state-vector Beta for state-space model 
#' of dynamic probabilities over the vocabulary words 
#' 
#' @param m_kv0 Prior mean of beta_{kv} at time t=0
#' @param sigma2_init Variance of Initialization
#' @param V Number of words in the vocabulary
#' @param t.T Number of points in time-series
#' @return A V x t.T matrix where each entry is the initialized parameter Beta_{v,t}
#' @examples 
#' Beta_Initialize(m_kv0 = 0, sigma2_init = .01, V = V, t.T = 25 )


Beta_Initialize<-function(m_kv0, sigma2_init,sigma2,V,t.T){
  Beta = matrix(0,nrow = V, ncol = t.T )
  for(t in 1:t.T){
    for(v in 1:V ){
      if(t==1){
        Beta[v,t] = m_kv0 + rnorm(1,0,sqrt(sigma2_init) ) 
      }else{
        Beta[v,t] = Beta[v,t-1] + rnorm(1,0,sqrt(sigma2) )
      }
    }
  }
  return(Beta)
}
