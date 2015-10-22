#' Alpha Initialization
#' 
#' This function initializes the dynamic state-vector Alpha for state-space model 
#' of dynamic topic proportions 
#' 
#' 
#' @param m0 Prior mean
#' @param G System Matrix
#' @param sigma2_init Variance of Initialization
#' @param t.T Number of points in time-series
#' @return Matrix where each column is the initialized state-vector Alpha_t
#' @examples 
#' Alpha_Initialize(m0 = matrix(c(0,0), nrow =2), G = matrix(c(1,1,0,1), nrow = 2, byrow = T), sigma2_init = .01, t.T = 25 )

Alpha_Initialize<-function(m0, G, C0, delta, t.T){
  p = dim(m0)[1]
  Alpha = matrix(nrow = p, ncol = t.T)
  
  for(t in 1:t.T){
    if(t == 1){
      Alpha[,t] = G%*%m0 + mvrnorm(1,m0,C0 )
    }else{
      Alpha[,t] = G%*%Alpha[,t-1] + rnorm(p, rep(0,p),delta )
    }
  }
  
  return(Alpha)
}


