rm(list=ls())
library(ggplot2)

# User defined inputs
K = 5
run = 2
init = paste(K,run,sep="_")

# all directories must end with /
MCMC_directory = "~/Desktop/DLTM_check/DLTM/MCMC_Summaries/"  #directory for mcmc SUMMARIES
output_directory = "~/Desktop/DLTM_check/DLTM/Figures/"       #directory for figures
data_directory = "~/Desktop/DLTM_check/DLTM/Data/"            #directory for data

Corpus_name = "Listings"                      #corpus name.  probably doesn't need to be changed.  
t.T = 112  #total number of time points

# End of user defined inputs

load(paste(MCMC_directory,"Beta_",init,".RData", sep=""))
load(paste(MCMC_directory,"Eta_",init,".RData", sep=""))
load(paste(MCMC_directory,"Alpha_",init,".RData", sep=""))
load(paste(MCMC_directory,"Prob_Z_",init,".RData", sep=""))

Vocab_Map = read.csv(file=paste(data_directory,"Vocab_Map_Short",Corpus_name,".csv",sep=""), header=T, stringsAsFactors=F)
Abbreviations = Vocab_Map[!is.na(Vocab_Map$Root_Index),c("Index","Root_Index")]
Vocab_Map = Vocab_Map[is.na(Vocab_Map$Root_Index),]
V = dim(Prob_Beta[[1]])[2]

for(t in 1:t.T){
  colnames(Prob_Beta[[t]]) = Vocab_Map[,1]
}

t = t.T
v = 1:V
thresh = 20
TopWords = as.data.frame(matrix(nrow=thresh, ncol = K))
for(k in 1:K){
     pdf(paste( output_directory,"Post_Mean_", init, "_",k,".pdf", sep="") )
     Comparison = data.frame(v, MCMC = Prob_Beta[[t]][k,], CI.025 = Prob_Beta_CI[[t]][[k]][1,], CI.975 = Prob_Beta_CI[[t]][[k]][2,])
     x = Comparison[order(Comparison[,2], decreasing=2)[1:thresh],1]
     y = Comparison[order(Comparison[,2], decreasing=2)[1:thresh],2]
     g = ggplot(Comparison, aes(x=v) ) +
     geom_point(aes(y=MCMC, color = "MCMC")) +
     geom_segment(mapping = aes(x = v, y=CI.025, xend = v, yend = CI.975, color = "MCMC"),size = .5, alpha = .10 ) +
     scale_color_manual("", breaks = c("MCMC"), values =c("MCMC" = "blue") ) +
     xlab("Vocabulary Term") +
     ylab("Probability") +
     #annotate("text", x = x+200, y = y, label = Vocab_Map[x,2] ) + 
     theme(axis.text=element_text(size=20, color="black"),axis.title=element_text(size=24,face="bold"), legend.text=element_text(size=20), axis.text.x=element_text(angle=-90))
     print(g)
     dev.off()
     
     TopWords[,k] = Vocab_Map[x,2]
}

write.csv(file = paste(output_directory,"TopWords_",init,".csv",sep=""), TopWords )

index = 1:K
pdf(paste(output_directory,"Topic_Proportions_",init,".pdf", sep="") )
Comparison = data.frame(Time = rep(1:t.T, times = K), Probability = c( t(P.Z.mean[index,]) ), CI.025 = c( t(CI.025[index,]) ), CI.975 = c( t(CI.975[index,]) ), Topic = factor(rep(1:K, each=t.T)) ) 
g = ggplot(Comparison, aes(x=Time) ) + 
  geom_line(aes(y=Probability, group = Topic, color = Topic), size = .5) + 
  #geom_line(aes(x = Time, y=CI.025, group = Topic, color = Topic), size = 1, alpha = .80, linetype=2 ) +
  #geom_line(aes(x = Time, y=CI.975, group = Topic, color = Topic), size = 1, alpha = .80, linetype=2 ) +
  xlab("Time") + 
  ylab("Probability")+
  theme(axis.text=element_text(size=20, color = "black"),axis.title=element_text(size=24,face="bold"), legend.text=element_text(size=20))
  print(g)
dev.off()

# .5*apply(abs(Prob_Beta_1[[t.T]] - Prob_Beta_2[[t.T]]),1,sum)


