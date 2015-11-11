rm(list=ls())
install.packages(c("ggplot2"))
library(ggplot2)

#User defined inputs
init = 101
MCMC_directory = "~/SCIOME/Code/MCMC_Summaries/"  #directory for mcmc SUMMARIES, not raw samples
data_directory = "~/DLTM/Data/"                  #directory where data is stored
variational_directory = "~/DLTM/Variational/"
output_directory = "~/DLTM/Figures/"
Model = "RW"                                 #model for the data generating process


########################################

load(paste(data_directory,"SynData",Model,".RData",sep=""))
load(paste(MCMC_directory,"Beta_", init, ".RData", sep="") )
load(paste(MCMC_directory,"Eta_", init, ".RData", sep="") )
load(paste(MCMC_directory,"Alpha_", init, ".RData", sep="") )
load(paste(MCMC_directory,"Prob_Z_",init,".RData",sep="") )


#Read in and align my estimates to the truth
Prob_Eta_truth = list()
for(t in 1:t.T){
  Prob_Eta_truth[[t]] = exp(Eta_truth[[t]])/apply(exp(Eta_truth[[t]]),1,sum)
}

Prob_Beta_truth = list()
for(t in 1:t.T){
  Prob_Beta_truth[[t]] = matrix(0,nrow = K, ncol = V)
  for(k in 1:K){
    Prob_Beta_truth[[t]][k,] = exp(Beta_truth[[k]][,t])/sum(exp(Beta_truth[[k]][,t]) )
  }
}


#Now insert zeros appropriately to augment the truth
for(t in 1:t.T){
  Prob_Beta_truth[[t]] = rbind(Prob_Beta_truth[[t]], matrix(0, nrow = 3, ncol = V) )
  Prob_Eta_truth[[t]] = cbind(Prob_Eta_truth[[t]], matrix(0, nrow = D[t], ncol = 3))
}

K = 6
TV_Beta_truth = matrix(nrow=K,ncol=K)
for(k in 1:K){ 
  for(kk in 1:K) TV_Beta_truth[k,kk] = .5*sum( abs( Prob_Beta[[1]][k,] - Prob_Beta_truth[[1]][kk,] ) )
}

excluded = NULL
topic_matching = rep(NA,K)
for(k in 1:3){
  if(k==1){
    topic_matching[k] = which(TV_Beta_truth[,k]== min(TV_Beta_truth[,k]))
  }else{
    topic_matching[k] = which(TV_Beta_truth[,k]== min(TV_Beta_truth[-excluded,k]) )
  }
  excluded = c(excluded, topic_matching[k])
}
topic_matching[4:6] = (1:K)[-excluded]
print(topic_matching)

Prob_Beta_CI_tmp = Prob_Beta_CI

for(t in 1:t.T){
  Prob_Beta[[t]][1:K,] = Prob_Beta[[t]][1:K,][topic_matching,]
  Prob_Eta[[t]][,1:K] = Prob_Eta[[t]][,1:K][,topic_matching]
  for(k in 1:K) Prob_Beta_CI[[t]][[k]] = Prob_Beta_CI_tmp[[t]][[topic_matching[k] ]]
}

P.Z.mean = P.Z.mean[topic_matching,]
CI.025 = CI.025[topic_matching,]
CI.975 = CI.975[topic_matching,]

DTM_variational = paste("model_run_",tolower(Model),"_K6",sep="") 

# now read in Blei's estimates and align them to the truth
a = scan(paste(variational_directory,DTM_variational,"/lda-seq/gam.dat", sep="") )
b = matrix(a, ncol = K, byrow = T)
rs = apply(b,1,sum)
PB = b/rs
Prob_Eta_Blei = list()
for(t in 1:t.T){
  if(t==1){
    index_1 = 1
    index_2 = D[t]
  }else{
    index_1 = sum(D[1:(t-1)])+1
    index_2 = sum(D[1:t])
  }
  Prob_Eta_Blei[[t]] = PB[index_1:index_2,]
}

Prob_Beta_Blei = list()
for(t in 1:t.T) Prob_Beta_Blei[[t]] = matrix(0,nrow = K, ncol = V)

for(k in 1:K){
  a = scan(paste(variational_directory, DTM_variational,"/lda-seq/topic-00",(k-1),"-var-e-log-prob.dat", sep="") )
  b = matrix(a,ncol=t.T,byrow=T) 
  for(t in 1:t.T) Prob_Beta_Blei[[t]][k,] = exp(b[,t])
}

for(t in 1:t.T) Prob_Beta_Blei[[t]] = Prob_Beta_Blei[[t]]/apply(Prob_Beta_Blei[[t]],1,sum)

TV_Beta_Blei_truth = matrix(nrow=K,ncol=K)
for(k in 1:K){ 
  for(kk in 1:K) TV_Beta_Blei_truth[k,kk] = .5*sum( abs( Prob_Beta_Blei[[1]][k,] - Prob_Beta_truth[[1]][kk,] ) )
}

excluded = NULL
topic_matching_Blei = rep(NA,K)
for(k in 1:3){
  if(k==1){
    topic_matching_Blei[k] = which(TV_Beta_Blei_truth[,k]== min(TV_Beta_Blei_truth[,k]))
  }else{
    topic_matching_Blei[k] = which(TV_Beta_Blei_truth[,k]== min(TV_Beta_Blei_truth[-excluded,k]) )
  }
  excluded = c(excluded, topic_matching_Blei[k])
}
topic_matching_Blei[4:6] = (1:K)[-excluded]
print(topic_matching_Blei)

for(t in 1:t.T){
  Prob_Beta_Blei[[t]] = Prob_Beta_Blei[[t]][topic_matching_Blei,]
  Prob_Eta_Blei[[t]] = Prob_Eta_Blei[[t]][,topic_matching_Blei]
}

t = 5
head(Prob_Eta_truth[[t]])
head(Prob_Eta[[t]])
head(Prob_Eta_Blei[[t]])

Eta_TV = matrix(nrow = 2, ncol = t.T)
for(t in 1:t.T){
  Eta_TV[1,t] = mean( .5*apply( abs( Prob_Eta[[t]] - Prob_Eta_truth[[t]]),1, sum ) ) 
  Eta_TV[2,t] = mean( .5*apply( abs( Prob_Eta_Blei[[t]] - Prob_Eta_truth[[t]]),1, sum ) )
}


t=5
p1 = .5*apply( abs( Prob_Eta[[t]] - Prob_Eta_truth[[t]]),1, sum )
p2 = .5*apply( abs( Prob_Eta_Blei[[t]] - Prob_Eta_truth[[t]]),1,sum )
ID = c("MCMC", "Variational")
df = data.frame(Method = rep(ID,each=D[t]), TV = c(p1,p2) )


pdf(paste( output_directory,"Eta_TV_Benchmark_", init, ".pdf", sep="") )
g = ggplot(df) + geom_histogram(aes(x=TV, fill=Method), color="grey50", alpha=.5, position = "identity")
g + theme(axis.text=element_text(size=20, color="black"),axis.title=element_text(size=24,face="bold"), legend.text=element_text(size=20))
dev.off()


########################################################################
## Compare estimation of Beta


TV_Beta = matrix(nrow = K, ncol = t.T)
TV_Beta_Blei = matrix(nrow = K,ncol=t.T)
for(t in 1:t.T){
  for(k in 1:K){
    TV_Beta[k,t] = .5*sum( abs( Prob_Beta[[t]][k,] - Prob_Beta_truth[[t]][k,] ) )
    TV_Beta_Blei[k,t] = .5*sum( abs( Prob_Beta_Blei[[t]][k,] - Prob_Beta_truth[[t]][k,] ) )
  }
}



pdf(paste( output_directory,"Beta_TV_Benchmark_", init, ".pdf", sep="") )
sa = data.frame(Time = rep(1:t.T, times = 2*K), TV = c(TV_Beta[1,], TV_Beta_Blei[1,], TV_Beta[2,], TV_Beta_Blei[2,], TV_Beta[3,], TV_Beta_Blei[3,]), Method = rep( rep(c("MCMC","Variational"),each=t.T ), times = K), Topic = factor( rep( 1:K, each = 2*t.T ) ), Group = rep(1:(2*K), each=t.T) )
ggplot(data = sa, aes(x=Time))+
  geom_point(aes(y=TV, group = Group, color = Method, shape = Topic), size = 5 )+
  ylim(0,.3) + xlab("Time") + ylab("TV Distance") + theme(axis.text=element_text(size=20, color = "black"),axis.title=element_text(size=24,face="bold"), legend.text=element_text(size=20))
dev.off()



t = 1
v = 1:V


for(k in 1:K){
  pdf(paste( output_directory,"Post_Mean_", init, "_",k,".pdf", sep="") )
  Comparison = data.frame(v, MCMC = Prob_Beta[[t]][k,], Truth = Prob_Beta_truth[[t]][k,], Variational = Prob_Beta_Blei[[t]][k,], CI.025 = Prob_Beta_CI[[t]][[k]][1,], CI.975 = Prob_Beta_CI[[t]][[k]][2,]) 
  g = ggplot(Comparison, aes(x=v) ) + 
    geom_point(aes(y=MCMC, color = "MCMC")) + 
    geom_segment(mapping = aes(x = v, y=CI.025, xend = v, yend = CI.975, color = "MCMC"),size = .5, alpha = .10 ) +
    geom_point(aes(y =Truth, color = "Truth")) +
    geom_point(aes(y=Variational, color = "Variational") )+
    scale_color_manual("", breaks = c("MCMC", "Truth", "Variational"), values =c("MCMC" = "blue", "Truth" = "black", "Variational" = "orange") ) + 
    xlab("Vocabulary Term") + 
    ylab("Probability") + 
    theme(axis.text=element_text(size=20, color="black"),axis.title=element_text(size=24,face="bold"), legend.text=element_text(size=20))
  print(g)
  dev.off()
}


Z_prob_truth = matrix(nrow = K*t.T, ncol = 2)

for(k in 1:K){
  for(t in 1:t.T){
    index = (k-1)*t.T + t 
    Z_prob_truth[index,1] = sum(Z_truth[[t]]==k)/length(Z_truth[[t]])
    Z_prob_truth[index,2] = k
  }
}

pdf(paste(output_directory,"Topic_Proportions_", init, ".pdf", sep="") )

Comparison = data.frame(Time = rep(1:t.T, times = K), Truth = Z_prob_truth[,1], Probability = c(  t(P.Z.mean) ), CI.025 = c( t(CI.025) ), CI.975 = c( t(CI.975) ), Topic = factor(rep(1:K, each=t.T)) ) 
g = ggplot(Comparison, aes(x=Time) ) + 
  geom_line(aes(y=Probability, group = Topic, color = Topic), size = 3) + 
  geom_line(aes(y=Truth, group = Topic, color = Topic), size = 3, linetype=3)+
  #geom_segment(mapping = aes(x = Time, y=CI.025, xend = Time, yend = CI.975, color = Topic),size = 3, alpha = .40 ) +
  geom_line(aes(x = Time, y=CI.025, group = Topic, color = Topic), size = 1, alpha = .40, linetype=2 ) +
  geom_line(aes(x = Time, y=CI.975, group = Topic, color = Topic), size = 1, alpha = .40, linetype=2 ) +
  xlab("Time") + 
  ylab("Probability")
g + theme(axis.text=element_text(size=20, color = "black"),axis.title=element_text(size=24,face="bold"), legend.text=element_text(size=20))


dev.off()


