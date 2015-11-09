rm(list=ls())
library(googleVis)
library(ggplot2)

load("~/SCIOME/Code/Reproducibility/Beta_201.RData")
load("~/SCIOME/Code/Reproducibility/Eta_201.RData")
load("~/SCIOME/Code/Reproducibility/Alpha_201.RData")
load("~/SCIOME/Code/Reproducibility/Prob_Z_201.RData")

Corpus_name = "Pubmed_85_15"
load(file=paste("~/SCIOME/Data/Vocab_Map_",Corpus_name,".RData",sep=""))
t.T = 30
K = 3
V = dim(Prob_Beta[[1]])[2]
for(t in 1:t.T){
  colnames(Prob_Beta[[t]]) = Vocab_Map[,1]
}

Topic_1 = Topic_2 = Topic_3 = matrix(nrow = 10, ncol = 6)
years = c("1989", "1994", "1999", "2004","2009", "2014")
colnames(Topic_1) = colnames(Topic_2) = colnames(Topic_3) = years
for(t in 1:6){
  Topic_1[,t] = names( sort( Prob_Beta[[5*t]][1,], decreasing=T ) )[1:10]
  Topic_2[,t] = names( sort( Prob_Beta[[5*t]][2,], decreasing=T ) )[1:10]
  Topic_3[,t] = names( sort( Prob_Beta[[5*t]][3,], decreasing=T ) )[1:10]
}


library(xtable)

xtable(Topic_1)
xtable(Topic_2)
xtable(Topic_3)
Brain = Hospital = Lung = Lesions = rep(NA,t.T)


for(t in 1:t.T){
  Brain[t] = Prob_Beta[[t]][3,"brain"]
  Hospital[t] = Prob_Beta[[t]][3,"hospital"]
  Lung[t] = Prob_Beta[[t]][3,"lung"]
  Lesions[t] = Prob_Beta[[t]][3,"lesions"]
}
years = 1984+1:t.T
pdf("~/SCIOME/Writing/Figures/Topic_3_Kwds_PubMed.pdf")
Comparison = data.frame(Year = years, Hospital = Hospital, Brain = Brain, Lung = Lung, Lesions = Lesions)
g = ggplot(Comparison, aes(x=Year) ) + 
  geom_line(aes(y=Hospital, color = "Hospital"), size =2) + 
  geom_line(aes(y=Brain, color = "Brain"), size = 2) +
  geom_line(aes(y=Lung, color = "Lung"), size = 2) +
  geom_line(aes(y=Lesions, color = "Lesions"), size = 2) +
  scale_color_manual("", breaks = c("Hospital", "Brain", "Lung", "Lesions"), values =c("Hospital" = "blue", "Brain" = "black", "Lung" = "orange", "Lesions" = "purple") ) + 
  xlab("Year") + 
  ylab("Probability")
g + theme(axis.text=element_text(size=20, color = "black"),axis.title=element_text(size=24,face="bold"), legend.text=element_text(size=20))
dev.off()


pdf("~/SCIOME/Writing/Figures/Topic_Proportions_201.pdf" )
Comparison = data.frame(Time = rep(1:t.T, times = K), Probability = c( t(P.Z.mean) ), CI.025 = c( t(CI.025) ), CI.975 = c( t(CI.975) ), Topic = factor(rep(1:K, each=t.T)) ) 
g = ggplot(Comparison, aes(x=Time) ) + 
  geom_line(aes(y=Probability, group = Topic, color = Topic), size = 4) + 
  geom_line(aes(x = Time, y=CI.025, group = Topic, color = Topic), size = 1, alpha = .80, linetype=2 ) +
  geom_line(aes(x = Time, y=CI.975, group = Topic, color = Topic), size = 1, alpha = .80, linetype=2 ) +
  xlab("Time") + 
  ylab("Probability")
g + theme(axis.text=element_text(size=20, color = "black"),axis.title=element_text(size=24,face="bold"), legend.text=element_text(size=20))
dev.off()
