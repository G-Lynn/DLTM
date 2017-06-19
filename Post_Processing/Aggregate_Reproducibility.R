Agg_1 = read.csv(file = paste(output_directory,"AggTopic_",1,".csv", sep=""))
Agg_2 = read.csv(file = paste(output_directory,"AggTopic_",2,".csv", sep=""))

.5*sum(abs(Agg_1[,2] - Agg_2[,2]))

load(file = paste(output_directory,"AggDoc_",1,".RData", sep="") )
Agg_Doc_1 = Agg_Doc

Agg_Doc_2 = load(file = paste(output_directory,"AggDoc_",2,".RData", sep="") )
Agg_Doc_2 = Agg_Doc

Doc_TV = c()
for(t in 1:t.T){
 Doc_TV = c(Doc_TV,.5*apply(abs(Agg_Doc_1[[t]] - Agg_Doc_2[[t]]),1,sum) )
}

png("~/Desktop/Doc_TV.png")
hist(Doc_TV)
dev.off()
