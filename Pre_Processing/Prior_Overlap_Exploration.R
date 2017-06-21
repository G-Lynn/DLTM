rm(list = ls())
library(bayesm)
#This is the topic overlap
V = 1000
K = 15
nSamples = 1000
nGrid = 50
TV_Top = matrix(nrow = nSamples-1,ncol = nGrid)
sigma2_kv0 = seq(.01,2.5,len = nGrid)

for(ii in 1:nGrid){
  beta = matrix(nrow = nSamples, ncol = V)
  for(i in 1:nSamples){
    beta[i,] = rnorm(V,0,sqrt(sigma2_kv0[ii]) )
  }

  p_V = exp(beta)/apply(exp(beta),1,sum)
  #hist(p_V[,1])
  #abline(v = 1/V)
  
  for(i in 2:nSamples){
    TV_Top[i-1,ii] = 1-.5*sum(abs(p_V[i,] - p_V[1,]))
  }
}

plot(sigma2_kv0, apply(TV_Top,2,mean), type = "l" )


#p = 4
p = 1
a2 = .25
#delta2 = c(.005,.00001,.001,.001) #a2/10
delta2 = a2/10
#F_dkt = c(1,0,1,0) #1
F_dkt = 1
omega = 2*pi/12

#G = matrix(0,nrow = p, ncol = p)
#G[1:2,1:2] = matrix(c(1,1,0,1),ncol = 2, nrow = 2, byrow = T)
#G[3:4,3:4] = matrix(c(cos(omega), sin(omega), -sin(omega), cos(omega) ), byrow=T, ncol=2)
G = 1

#C_0 = c(.01,.0000001,.1,.1) #rep(.1,p)
C_0 = rep(.1,p)
m_0 = rep(0,p)

KK = c(3,15,100)
a2 = seq(0.25,5,len = 25)
TV_Doc = matrix(nrow = length(KK), ncol = length(a2))

for(jj in 1:length(KK)){
  K = KK[jj]
  for(kk in 1:length(a2)){
    alpha = list()
    for(k in 1:K) alpha[[k]] = matrix(nrow = nSamples, ncol = p)
    eta_1 = eta_2 = matrix(nrow = nSamples, ncol = K)

      for(i in 1:nSamples){
        for(k in 1:K){
          alpha_0 = rnorm(p,m_0,sqrt(C_0))
          alpha[[k]][i,] = G%*%alpha_0 + rnorm(p,0,sqrt(delta2))
          eta_1[i,k] = F_dkt%*%alpha[[k]][i,] + rnorm(1,0,sqrt(a2[kk]))
          eta_2[i,k] = F_dkt%*%alpha[[k]][i,] + rnorm(1,0,sqrt(a2[kk]))
        }
      }
      #var( alpha[[k]]%*%t(t(F_dkt)) )
      #hist(Doc1[,2])
      #abline(h = .8, col = "blue")
      #abline(v = 1/K, col = "red")
      
      Doc1 = exp(eta_1)/apply(exp(eta_1),1,sum) #t( replicate(nSamples, c(rdirichlet( rep(1/K,K) ) ) ) ) #exp(eta_1)/apply(exp(eta_1),1,sum)
      Doc2 = exp(eta_2)/apply(exp(eta_2),1,sum) #t( replicate(nSamples, c(rdirichlet( rep(1/K,K) ) ) ) ) #exp(eta_2)/apply(exp(eta_2),1,sum)

      TV_Doc[jj,kk] = mean( 1 - .5*apply(abs(Doc1-Doc2),1,sum) )
  }
}

hist(Doc1[,2])
abline(v = 1/K, col = "red")

plot(a2,TV_Doc[1,], ylim = c(0,1), type = "l", ylab = "Document overlap")
lines(a2,TV_Doc[2,], col = "red")
lines(a2,TV_Doc[3,], col = "blue")
abline(v=.25)
#lines(a2,TV_Doc[4,], col = "darkgreen")


