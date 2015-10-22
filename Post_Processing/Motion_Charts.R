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

for(k in 1:K){
  Topic = data.frame(Term = rep(NA, V*t.T), Year = rep(NA,V*t.T), Probability = rep(NA,V*t.T))
  for(t in 1:t.T){
    for(v in 1:V){
      index = (t-1)*V + v
      Topic[index,"Term"] = Vocab_Map[v,1]
      Topic[index,"Year"] = 1984+t
      Topic[index,"Probability"] = Prob_Beta[[t]][k,v]
    }
  }

  Motion=gvisMotionChart(Topic, 
                       idvar="Term", 
                       timevar="Year")
  cat(Motion$html$chart, file=paste("~/.public_html/Topic_Visualization/Topic_",k,"_LL.html",sep="") )
}



Z_prob = data.frame(Topic = rep(NA, K*t.T), Year = rep(NA,K*t.T), Probability = rep(NA,K*t.T))

for(t in 1:t.T){
  for(k in 1:K){
    index = (t-1)*K + k
    Z_prob[index,"Topic"] = k
    Z_prob[index,"Year"] = 1984+t
    Z_prob[index,"Probability"] = Prob_Z[[t]][1,k]
  }
}


Motion=gvisMotionChart(Z_prob, 
                       idvar="Topic", 
                       timevar="Year")
cat(Motion$html$chart, file=paste("~/.public_html/Topic_Visualization/Topic_Probs_LL.html",sep="") )




Z_prob = matrix(nrow = K*t.T, ncol = 4)
for(t in 1:t.T){
  for(k in 1:K){
    index = (t-1)*K + k 
    Z_prob[index,1] = Prob_Z[[t]][1,k]
    Z_prob[index,2] = Prob_Z[[t]][2,k]
    Z_prob[index,3] = Prob_Z[[t]][3,k]
    Z_prob[index,4] = k
  }
}

pdf("~/SCIOME/Writing/Figures/PubMed_LL_Trends.pdf")
Comparison = data.frame(Time = rep(1:t.T, each = K), Probability = Z_prob[,1], CI.025 = Z_prob[,2], CI.975 = Z_prob[,3], Topic = factor(Z_prob[,4]) ) 
g = ggplot(Comparison, aes(x=Time) ) + 
  geom_line(aes(y=Probability, group = Topic, color = Topic), size = 4) + 
  #geom_segment(mapping = aes(x = Time, y=CI.025, xend = Time, yend = CI.975, color = Topic),size = 3, alpha = .40 ) +
  geom_line(aes(x = Time, y=CI.025, group = Topic, color = Topic), size = 1, alpha = .80, linetype=2 ) +
  geom_line(aes(x = Time, y=CI.975, group = Topic, color = Topic), size = 1, alpha = .80, linetype=2 ) +
  xlab("Time") + 
  ylab("Probability")
g + theme(axis.text=element_text(size=20, color = "black"),axis.title=element_text(size=24,face="bold"), legend.text=element_text(size=20))
dev.off()
