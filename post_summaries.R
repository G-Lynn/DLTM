#rm(list=ls())
#load("~/SCIOME/Code/SynData1.RData")
#load("~/SCIOME/Code/SynDataLinear.RData")
##load(file = "~/SCIOME/Data/DynCorp_Pubmed_85_15.RData" )
#t.1 = 1
#t.T = 5
#t.T = length(1985:2014)  #the total number of time points in the corpus
#K=3
#stem = "~/SCIOME/Code/"
#init = 5
#nSamples = 3000
#B = 0
#burnIn = 2000
#a2 = .025
#p = 1
#F_dkt = matrix(c(1), nrow = 1)
#p = 2
#F_dkt = matrix(c(1,0), nrow = 1)

#library(parallel)

############

Beta = list()
Prob_Beta = list()
Prob_Beta_CI = list()
for(t in 1:t.T) Prob_Beta[[t]] = matrix(nrow = K, ncol = V)
for(t in 1:t.T) Prob_Beta_CI[[t]] = list() 
for(k in 1:K){
  Beta[[k]]=list()
  tmp = read.csv(paste(stem,"MCMC_Samples/Init_",init,"/Beta_",k,".csv",sep=""),header=F,stringsAsFactors=F)
  for(t in 1:t.T){
    Beta[[k]][[t]] = matrix(0,nrow = nSamples,ncol = V)
    for(i in 1:nSamples){
      index_1 = (i-1)*V + 1
      index_2 = (i-1)*V + V
      Beta[[k]][[t]][i,] = tmp[index_1:index_2,(t+1)]
    }
    Beta[[k]][[t]] = Beta[[k]][[t]][(burnIn+1):nSamples,]
    Prob_Beta[[t]][k,] = apply(  exp(Beta[[k]][[t]])/apply(exp(Beta[[k]][[t]]),1,sum) ,2, mean)
    Prob_Beta_CI[[t]][[k]] = apply(  exp(Beta[[k]][[t]])/apply(exp(Beta[[k]][[t]]),1,sum) ,2, quantile, c(.025, .975) )
 
  }
}

save(file = paste(stem,"MCMC_Summaries/Beta_",init,".RData",sep=""),Prob_Beta, Prob_Beta_CI)
rm(Beta,tmp,Prob_Beta,index_1,index_2)

Eta = list()
Prob_Eta = list()

for(t in 1:t.T){
  Prob_Eta[[t]] = matrix(0,nrow=D[t],ncol = K)
}

for(t in 1:t.T){
  tmp = read.csv(paste(stem,"MCMC_Samples/Init_",init,"/Eta_",t,".csv",sep=""),header=F,stringsAsFactors=F)

  Eta[[t]] = list()
  for(d in 1:D[t]){
    Eta[[t]][[d]] = tmp[tmp[,1]==d,2:(K+1)]
    Eta[[t]][[d]] = Eta[[t]][[d]][(burnIn+1):nSamples,]
    Prob_Eta[[t]][d,] = apply(  exp(Eta[[t]][[d]])/apply(exp(Eta[[t]][[d]]),1,sum) ,2, mean) 
  }
}

save(file = paste(stem,"MCMC_Summaries/Eta_",init,".RData",sep=""),Prob_Eta)
rm(Eta,Prob_Eta,tmp)

#I still need to figure out the indexing on Alpha.  
Alpha_mean=list()
Alpha_K = read.csv(paste(stem,"MCMC_Samples/Init_",init,"/Alpha_",K,".csv",sep=""), header=F,stringsAsFactors=F)
Alpha_K = Alpha_K[(burnIn*p+1):(nSamples*p),]


for(k in 1:(K-1)){
    tmp = read.csv(paste(stem,"MCMC_Samples/Init_",init,"/Alpha_",k,".csv",sep=""), header=F,stringsAsFactors=F)
    tmp = tmp[(burnIn*p+1):(nSamples*p),]
    
    
    Alpha_mean[[k]] = matrix( unlist( mclapply(1:p, function(i) apply( (tmp[tmp[,1]==i,2:(t.T+1)] - Alpha_K[Alpha_K[,1]==i,2:(t.T+1)] ),2,mean), mc.cores = 8 ) ), byrow=T, nrow = p ) 
}
save(file = paste(stem,"MCMC_Summaries/Alpha_",init,".RData",sep=""),Alpha_mean)
rm(Alpha_mean)

Alpha_t = list()
for(t in 1:t.T) Alpha_t[[t]] = matrix(nrow = (nSamples-burnIn), ncol = K)

for(k in 1:K){
  tmp = read.csv(paste(stem,"MCMC_Samples/Init_",init,"/Alpha_",k,".csv",sep=""), header=F,stringsAsFactors=F)
  tmp = tmp[(burnIn*p+1):(nSamples*p),]

  for(i in 1:(nSamples-burnIn)){
    index_l = p*(i-1) + 1
    index_u = p*i
    eta = F_dkt%*%as.matrix(tmp[index_l:index_u,2:(t.T+1)]) + rnorm(1,0,sqrt(a2))
    for(t in 1:t.T) Alpha_t[[t]][i,k] = eta[t]
  }
}

P.Z.mean = CI.025 = CI.975 = matrix(nrow = K, ncol = t.T)

for(t in 1:t.T){
  P.z = exp(Alpha_t[[t]])/apply(exp(Alpha_t[[t]]),1,sum)
  P.Z.mean[,t] = apply(P.z, 2, mean)
  CI.025[,t] = apply(P.z, 2, quantile, .025)
  CI.975[,t] = apply(P.z, 2, quantile, .975)
}

save(file = paste(stem,"MCMC_Summaries/Prob_Z_",init,".RData",sep=""),P.Z.mean, CI.025, CI.975)
