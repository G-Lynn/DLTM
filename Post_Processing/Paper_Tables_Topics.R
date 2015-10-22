rm(list=ls())
library(googleVis)
load("~/SCIOME/Code/Reproducibility/Beta_202.RData")
load("~/SCIOME/Code/Reproducibility/Eta_202.RData")

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

Brain = rep(NA,t.T)
Data = rep(NA, t.T)
Mice = rep(NA, t.T)

for(t in 1:t.T){
  Brain[t] = Prob_Beta[[t]][3,"brain"]
  Mice[t] = Prob_Beta[[t]][3,"mice"]
  Data[t] = Prob_Beta[[t]][3,"data"]
}
years = 1984+1:t.T
pdf("~/SCIOME/Writing/Figures/Topic_3_Kwds_PubMed.pdf")
Comparison = data.frame(Year = years, Brain = Brain, Mice = Mice, Data = Data)
g = ggplot(Comparison, aes(x=Year) ) + 
  geom_line(aes(y=Brain, color = "Brain"), size =2) + 
  geom_line(aes(y=Mice, color = "Mice"), size = 2) +
  geom_line(aes(y=Data, color = "Data"), size = 2) +
  scale_color_manual("", breaks = c("Brain", "Mice", "Data"), values =c("Brain" = "blue", "Mice" = "black", "Data" = "orange") ) + 
  xlab("Year") + 
  ylab("Probability")
g + theme(axis.text=element_text(size=20, color = "black"),axis.title=element_text(size=24,face="bold"), legend.text=element_text(size=20))
dev.off()
