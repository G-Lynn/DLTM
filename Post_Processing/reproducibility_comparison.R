rm(list=ls())
K = 3
t.T = 5

nSim = 5
#alpha may not be reproducible here because what is differenced will be different depending
#on what is labeled as topic K.  We set topic K to zero by differencing, but topic K does not always
#correspond to the same thing across different MCMC runs.  This problem is solved at the simplex
#level but not for underlying parameters.  

for(i in 1:nSim){
  load(paste("~/SCIOME/Code/Reproducibility/Alpha_",i,".RData",sep=""))
  assign(paste("Alpha_mean_",i,sep=""),Alpha_mean)
  load(paste("~/SCIOME/Code/Reproducibility/Eta_",i,".RData",sep=""))
  assign(paste("Prob_Eta_",i,sep=""),Prob_Eta)
  load(paste("~/SCIOME/Code/Reproducibility/Beta_",i,".RData",sep=""))
  assign(paste("Prob_Beta_",i,sep=""),Prob_Beta)
}

TV_Beta_truth = matrix(nrow=K,ncol=K)
TV_Beta = list()
for(k in 1:K) TV_Beta[[k]] = matrix(nrow = (nSim-1),ncol = t.T )

for(i in 2:nSim){
  Prob_Beta = get(paste("Prob_Beta_",i,sep=""))
  Prob_Eta = get(paste("Prob_Eta_",i,sep=""))
  for(k in 1:K){ 
      for(kk in 1:K) TV_Beta_truth[k,kk] = .5*sum( abs( Prob_Beta[[1]][k,] - Prob_Beta_1[[1]][kk,] ) )
  }
  topic_matching = sapply(1:K, function(k) which(TV_Beta_truth[,k]==min(TV_Beta_truth[,k])) )

  for(t in 1:t.T){
    Prob_Beta[[t]] = Prob_Beta[[t]][topic_matching,]
    Prob_Eta[[t]] = Prob_Eta[[t]][,topic_matching]
  }
  
  for(k in 1:K){
    for(t in 1:t.T){
      TV_Beta[[k]][(i-1),t] = .5*sum( abs( Prob_Beta[[t]][k,] - Prob_Beta_1[[t]][k,] ) )
    }
  }
  assign(paste("Prob_Beta_",i,sep=""),Prob_Beta)
  assign(paste("Prob_Eta_",i,sep=""),Prob_Eta)
}

MAX_TV = matrix(nrow = K, ncol = t.T)

for(k in 1:K){
  MAX_TV[k,] = apply(TV_Beta[[k]],2,max)
}
rownames(MAX_TV) = factor(1:K)

pdf("~/SCIOME/Writing/Figures/TV_Beta_Max.pdf")
sa <- stack(as.data.frame( t(MAX_TV) ))
names(sa)[2] = "Topic"
sa$x <- rep(seq_len(ncol(MAX_TV)), nrow(MAX_TV))
ggplot(data = sa, aes(x=x))+
  geom_point(aes(y=values, group = Topic, color = Topic, shape = Topic), size = 5 )+
  ylim(0,.075) + xlab("Time") + ylab("TV Distance") + theme(axis.text=element_text(size=20, color="black"),axis.title=element_text(size=24,face="bold"),legend.text=element_text(size=20) )
dev.off()



#First document at each time point.
TV_Eta = list()
tv = NULL
time = NULL
for(t in 1:t.T){
  nDocs = dim(Prob_Eta[[t]])
  TV_Docs = matrix(nrow = nDocs, ncol = (nSim-1))
  for(i in 2:nSim){
    Prob_Eta = get(paste("Prob_Eta_",i,sep=""))
    TV_Docs[,(i-1)] = .5*apply(abs(Prob_Eta[[t]] - Prob_Eta_1[[t]]),1,sum)
  }
  TV_Eta[[t]]  = apply(TV_Docs,1,max)
  n.tv = length(TV_Eta[[t]])
  tv = c(tv,TV_Eta[[t]])
  time = c(time,rep(t,n.tv) )
}


pdf("~/SCIOME/Writing/Figures/TV_Eta_Max.pdf")
df = data.frame(TV = tv, Time = factor(time))
ggplot(df, aes(x=Time, y=TV)) + geom_boxplot(fill="light blue") + ylab("Max TV Distance") + theme(axis.text=element_text(size=20, color="black"),axis.title=element_text(size=24,face="bold"), legend.text=element_text(size=20)) 
dev.off()
