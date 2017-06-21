rm(list = ls())
MCMC_directory = "~/Desktop/DLTM_check/DLTM/MCMC_Summaries/"  #directory for mcmc SUMMARIES
output_directory = "~/Desktop/DLTM_check/DLTM/Figures/"       #directory for figures
data_directory = "~/Desktop/DLTM_check/DLTM/Data/"            #directory for data
variational_directory = "~/Desktop/DLTM_check/DLTM/Variational/Listings_run/"
K = 15
V = 912
t.T = 112
Corpus_name = "Listings"
Prob_Beta_Blei = list()
for(t in 1:t.T) Prob_Beta_Blei[[t]] = matrix(0,nrow = K, ncol = V)

for(k in 1:K){
  if(k<11){
    a = scan(paste(variational_directory,"/lda-seq/topic-00",(k-1),"-var-e-log-prob.dat", sep="") )
  }else{
    a = scan(paste(variational_directory,"/lda-seq/topic-0",(k-1),"-var-e-log-prob.dat", sep="") )
  }
  b = matrix(a,ncol=t.T,byrow=T) 
  for(t in 1:t.T) Prob_Beta_Blei[[t]][k,] = exp(b[,t])
}

for(t in 1:t.T) Prob_Beta_Blei[[t]] = Prob_Beta_Blei[[t]]/apply(Prob_Beta_Blei[[t]],1,sum)

Vocab_Map = read.csv(file=paste(data_directory,"Vocab_Map_Short",Corpus_name,".csv",sep=""), header=T, stringsAsFactors=F)
Abbreviations = Vocab_Map[!is.na(Vocab_Map$Root_Index),c("Index","Root_Index")]
Vocab_Map = Vocab_Map[is.na(Vocab_Map$Root_Index),]

for(t in 1:t.T){
  colnames(Prob_Beta_Blei[[t]]) = Vocab_Map[,1]
}

t = t.T
v = 1:V
thresh = 20
TopWords = as.data.frame(matrix(nrow=thresh, ncol = K))
for(k in 1:K){
  pdf(paste( output_directory,"Post_Mean_Variational_",k,".pdf", sep="") )
  Comparison = data.frame(v, MCMC = Prob_Beta_Blei[[t]][k,])
  x = Comparison[order(Comparison[,2], decreasing=2)[1:thresh],1]
  y = Comparison[order(Comparison[,2], decreasing=2)[1:thresh],2]
  g = ggplot(Comparison, aes(x=v) ) +
    geom_point(aes(y=MCMC, color = "MCMC")) +
    scale_color_manual("", breaks = c("MCMC"), values =c("MCMC" = "blue") ) +
    xlab("Vocabulary Term") +
    ylab("Probability") +
    #annotate("text", x = x+200, y = y, label = Vocab_Map[x,2] ) + 
    theme(axis.text=element_text(size=20, color="black"),axis.title=element_text(size=24,face="bold"), legend.text=element_text(size=20), axis.text.x=element_text(angle=-90))
  print(g)
  dev.off()
  
  TopWords[,k] = Vocab_Map[x,2]
}

write.csv(file = paste(output_directory,"Variational_TopWords_",init,".csv", sep=""), TopWords)




