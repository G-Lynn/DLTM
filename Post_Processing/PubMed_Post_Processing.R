rm(list=ls())
library(ggplot2)
init = 20115
MCMC_directory = "~/SCIOME/Code/Reproducibility/"
output_directory = "~/DLTM/Figures/"
data_directory = "~/DLTM/Data/"
Corpus_name = "Pubmed_85_15"
t.T = 30
K = 15

load(paste(MCMC_directory,"Beta_",init,".RData", sep=""))
load(paste(MCMC_directory,"Eta_",init,".RData", sep=""))
load(paste(MCMC_directory,"Alpha_",init,".RData", sep=""))
load(paste(MCMC_directory,"Prob_Z_",init,".RData", sep=""))
load(file=paste(data_directory,"Vocab_Map_",Corpus_name,".RData",sep=""))

V = dim(Prob_Beta[[1]])[2]
for(t in 1:t.T){
  colnames(Prob_Beta[[t]]) = Vocab_Map[,1]
}




t = t.T
v = 1:V

for(k in 1:K){
     pdf(paste( output_directory,"Post_Mean_", init, "_",k,".pdf", sep="") )
     Comparison = data.frame(v, MCMC = Prob_Beta[[t]][k,], CI.025 = Prob_Beta_CI[[t]][[k]][1,], CI.975 = Prob_Beta_CI[[t]][[k]][2,])
     g = ggplot(Comparison, aes(x=v) ) +
     geom_point(aes(y=MCMC, color = "MCMC")) +
     geom_segment(mapping = aes(x = v, y=CI.025, xend = v, yend = CI.975, color = "MCMC"),size = .5, alpha = .10 ) +
     scale_color_manual("", breaks = c("MCMC"), values =c("MCMC" = "blue") ) +
     xlab("Vocabulary Term") +
     ylab("Probability") +
     theme(axis.text=element_text(size=20, color="black"),axis.title=element_text(size=24,face="bold"), legend.text=element_text(size=20))
     print(g)
     dev.off()
}



pdf(paste(output_directory,"Topic_Proportions_",init,".pdf", sep="") )
Comparison = data.frame(Time = rep(1:t.T, times = K), Probability = c( t(P.Z.mean) ), CI.025 = c( t(CI.025) ), CI.975 = c( t(CI.975) ), Topic = factor(rep(1:K, each=t.T)) ) 
g = ggplot(Comparison, aes(x=Time) ) + 
  geom_line(aes(y=Probability, group = Topic, color = Topic), size = 4) + 
  geom_line(aes(x = Time, y=CI.025, group = Topic, color = Topic), size = 1, alpha = .80, linetype=2 ) +
  geom_line(aes(x = Time, y=CI.975, group = Topic, color = Topic), size = 1, alpha = .80, linetype=2 ) +
  xlab("Time") + 
  ylab("Probability")
g + theme(axis.text=element_text(size=20, color = "black"),axis.title=element_text(size=24,face="bold"), legend.text=element_text(size=20))
dev.off()
