rm(list = ls())
stem = "/Users/christophergl1/Desktop/DLTM/DLTM-master/" 
load(file = paste(stem,"Data/DynCorp_Listings.RData",sep="") )

t.T = length(Dates)
Vocab.Rate = matrix(0,nrow = t.T, ncol = V)
colnames(Vocab.Rate) = Vocab_Map_Final$Full
Months = as.numeric(format(Dates, "%m"))

for(t in 1:t.T){
  index = as.numeric(names( table(W[[t]]) ) )
  Vocab.Rate[t,index] = as.numeric(table(W[[t]]))
  Vocab.Rate[t,] = Vocab.Rate[t,] / sum(Vocab.Rate[t,])
}


for(v in 1:V){
  tmp = rep(NA,t.T)
  tmp2 = rep(NA,t.T)
  tmp[Months==1] = Vocab.Rate[Months==1,v]
  tmp2[Months==7] = Vocab.Rate[Months==7,v]

  
  pdf(paste("~/Desktop/DLTM/DLTM-master/Figures/Term_",v,".pdf",sep=""))
  #plot(Dates,Vocab.Rate[,v], type = "l", main = colnames(Vocab.Rate)[v], ylab="Empirical rate")
  #points(Dates,tmp, pch = 21, col = "red", bg = "red")
  #points(Dates,tmp2,pch=21,col="blue",bg="blue")
  df = data.frame(Dates, Freq = Vocab.Rate[,v])
  p<-ggplot(df, aes(Dates)) + geom_line(aes(Dates,Freq), color = "blue")+
  ylab("Empirical Frequency")+
  scale_x_continuous("Year", 2008:2016)+
  theme(axis.text=element_text(size=10, color="black"),axis.title=element_text(size=24,face="bold"), legend.text=element_text(size=20))
  print(p)
  dev.off()
}




