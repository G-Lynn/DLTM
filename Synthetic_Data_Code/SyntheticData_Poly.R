#Create synthetic data
rm(list=ls())
data_set = "Quadratic"
t.T = 5
K = 3
V = 1000
p = 3
nWords = 150
nDocs = 1000
sigma = sqrt(.001)
delta = .01
a2 = .5
C0 = .025
m0 = rep(0,p)

G = matrix(0,nrow = p, ncol = p)
G[upper.tri(G,diag=T)]=1
FF = matrix(c(1,rep(0,p-1)), byrow = T, nrow = 1)

D = rpois(t.T, nDocs)
N.D = list()
for(t in 1:t.T) N.D[[t]] = rpois(D[t], nWords)

Beta_k = list()
Alpha_k = list()

for(k in 1:K){
  Beta_k[[k]] = matrix(0,nrow = V, ncol = t.T)
  Alpha_k[[k]] = matrix(0,nrow = p, ncol = t.T)
  
  for(v in 1:V ) {
    if(k==1){
      if(v<=333){
        Beta_k[[k]][v,1] = .5
      }else{
        Beta_k[[k]][v,1] = -.5
      }
    }
    if(k==2){
      if(v>=334 & v<=667){
        Beta_k[[k]][v,1] = .5
      }else{
        Beta_k[[k]][v,1] = -.5
      }
      
    }
    if(k==3){
      if(v>=668 & v<=1000){
        Beta_k[[k]][v,1] = .5
      }else{
        Beta_k[[k]][v,1] = -.5
      }
      
    } 
    
    for(t in 2:t.T){
      Beta_k[[k]][v,t] = Beta_k[[k]][v,t-1] + rnorm(1,0,sigma)
    }
  }
  
  if(k==1) Alpha_k[[k]][,1] = rnorm(p,m0,sqrt(C0 + delta) )  
  if(k==2) Alpha_k[[k]][,1] = rnorm(p,m0,sqrt(C0 + delta) ) 
  if(k==3) Alpha_k[[k]][,1] = rnorm(p,m0, sqrt(C0 + delta) )
  for(t in 2:t.T){
      Alpha_k[[k]][,t] = G%*%Alpha_k[[k]][,t-1] + rnorm(p,0,sqrt(delta) )
  }
  
}



Eta_t = list()
for(t in 1:t.T){
  Eta_t[[t]] = matrix(0, nrow = D[t], ncol = K)
  for(d in 1:D[t]){
    for(k in 1:K){
      Eta_t[[t]][d,k] = FF%*%Alpha_k[[k]][,t] + rnorm(1,0, sqrt(a2) )
    }
  }
}

Z_t = list()
W = list()
#Now generate the documents and words. 

y_kv = list()
x_dk = list()
ny = matrix(nrow = K, ncol = t.T)
Kappa_Beta_k = list()
Kappa_Eta_t = list()
for(k in 1:K) Kappa_Beta_k[[k]] = matrix(nrow = V, ncol = t.T)

for(t in 1:t.T){
  y_kv[[t]] = matrix(0,nrow = K, ncol = V )
  x_dk[[t]] = matrix(0,nrow = D[t], ncol = K)
  Z_t[[t]] = rep(NA, sum( N.D[[t]] ) )
  W[[t]] = matrix(NA, nrow = sum(N.D[[t]]), ncol = 2)
  index = 0
  for(d in 1:D[t]){
    for(n in 1:N.D[[t]][d] ){
      index = index + 1
      prob_Z = exp(Eta_t[[t]][d,])
      prob_Z = prob_Z / sum(prob_Z)
      Z_t[[t]][ index ] = sample(1:K, 1, prob = prob_Z)
      prob_V = exp(Beta_k[[ Z_t[[t]][index] ]][,t])
      prob_V = prob_V / sum(prob_V)
      W[[t]][index,1] = sample(1:V, 1, prob = prob_V )
      W[[t]][index,2] = d
      
      y_kv[[t]][ Z_t[[t]][index], W[[t]][index,1] ] = 1 + y_kv[[t]][ Z_t[[t]][index], W[[t]][index,1] ]
      x_dk[[t]][ W[[t]][index,2], Z_t[[t]][index] ] = 1 + x_dk[[t]][ W[[t]][index,2], Z_t[[t]][index] ]
    }
  }
}

for(t in 1:t.T){
  ny[,t] = apply(y_kv[[t]], 1 , sum)
  for(k in 1:K) Kappa_Beta_k[[k]][,t] = y_kv[[t]][k,] - .5*ny[k,t]
  Kappa_Eta_t[[t]] = matrix(nrow = D[t], ncol = K)
  for(d in 1:D[t]) Kappa_Eta_t[[t]][d,] = x_dk[[t]][d,] - .5*N.D[[t]][d]
}

for(k in 1:K) Alpha_k[[k]] = Alpha_k[[k]]-Alpha_k[[K]]

Eta_truth = list()
for(t in 1:t.T) Eta_truth[[t]] = Eta_t[[t]]-Eta_t[[t]][,K]

Beta_truth = list()
for(k in 1:K) Beta_truth[[k]] = Beta_k[[k]] - matrix(rep(Beta_k[[k]][V,], times=V), nrow = V, byrow=T)

Z_truth = Z_t
Alpha_truth = Alpha_k
Kappa_Eta_truth = Kappa_Eta_t
Kappa_Beta_truth = Kappa_Beta_k
ny_truth = ny

save(file = paste("~/DLTM/Data",data_set,".RData",sep=""), t.T, D, N.D, K, V, ny_truth, Eta_truth, Kappa_Eta_truth, Beta_truth, Kappa_Beta_truth, Alpha_truth, Z_truth, W )
