rm(list=ls())
library(googleVis)

# User defined inputs

init = 1

# all directories must end with /
MCMC_directory = "~/DLTM-master/MCMC_Summaries/"   #directory for mcmc SUMMARIES
data_directory = "~/DLTM-master/Data/"             #directory for data 
html_path = "~/.public_html/Topic_Visualization/"  #directory to write HTML files
Corpus_name = "Pubmed_85_15"                       #name of corpus.  probably doesn't need to be changed. 

t.T = 30 #total number of time poitns
K = 15 #number of topics

# End of user defined inputs ##########################################

load(file=paste(MCMC_directory,"Beta_",init,".RData", sep=""))
load(file=paste(MCMC_directory,"Eta_",init,".RData", sep=""))
load(file=paste(data_directory,"Vocab_Map_",Corpus_name,".RData",sep=""))


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



