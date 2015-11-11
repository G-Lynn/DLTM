rm(list=ls())
library(googleVis)
init = 20115
MCMC_directory = "~/SCIOME/Code/Reproducibility/"
data_directory = "~/DLTM/Data/"
html_path = "~/.public_html/Topic_Visualization/"
Corpus_name = "Pubmed_85_15"

load(file=paste(MCMC_directory,"Beta_",init,".RData", sep=""))
load(file=paste(MCMC_directory,"Eta_",init,".RData", sep=""))
load(file=paste(data_directory,"Vocab_Map_",Corpus_name,".RData",sep=""))

t.T = 30
K = 15
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
  cat(Motion$html$chart, file=paste(html_path,"Topic_",k,".html",sep="") )
}



