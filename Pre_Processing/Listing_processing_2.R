rm(list=ls())
library(zoo)
Corpus_name = "Listings"
Data = readRDS(file = "~/Desktop/DLTM/Z_Listings_Seattle.rds")
load("~/Desktop/DLTM/DLTM-master/Data/Listings.RData")
DatesYM = as.yearmon(Data$FirstPostDate)
Data$DatesYM = DatesYM
Dates = unique(DatesYM) 
Dates = sort(Dates)
nDates = length(Dates)
save(file = "~/Desktop/DLTM/DLTM-master/Data/Dates.RData", Dates)
#Years = sort( unique(text$Year) )
YM.index = list()
DynCorpus = list()
D = rep(NA,nDates)
W = list()
N.D = list()
Vocab.Trunc.index = which(Vocab.counts>=25)
Vocab.Trunc = Vocabulary[Vocab.Trunc.index,]
Vocab.Trunc = Vocab.Trunc[-which(Vocab.Trunc[,1]=="\xfc\xbe\x99\x96\x88\xbc"),] #Some nonsense characters that got passed through
V = dim(Vocab.Trunc)[1]
Vocab.counts.t = matrix(nrow = nDates,ncol = V)


for(t in 1:nDates){
  YM.index[[t]] = which(Data$DatesYM==Dates[t])
  N.docs.t = length(YM.index[[t]])
  for(v in 1:V){
    N.v =0
    for(d in 1:N.docs.t){
      if(is.null(Listings[[ YM.index[[t]][d] ]]$Description) | length(Listings[[ YM.index[[t]][d] ]]$Description) < 50){next}
      N.v = N.v + sum( Listings[[ YM.index[[t]][d] ]]$Description_Numeric==Vocab.Trunc.index[v])
    }
    Vocab.counts.t[t,v] = N.v
  }
}

save(file=paste("~/Desktop/DLTM/DLTM-master/Data/Vocab_Counts_",Corpus_name,".RData",sep=""),Vocab.counts.t)
load(file=paste("~/Desktop/DLTM/DLTM-master/Data/Vocab_Counts_",Corpus_name,".RData",sep=""))
lower_thresh = 0 #15 seems to work
upper_thresh = 5000 #3000 seemed to work well.
#upper_thresh = max(Vocab.counts.t)
Min.Vocab.t = apply(Vocab.counts.t,2,min)
Max.Vocab.t = apply(Vocab.counts.t,2,max)
Tot.Vocab.t = apply(Vocab.counts.t,2,sum)

sum(Min.Vocab.t >= lower_thresh & Max.Vocab.t <= upper_thresh)
colnames(Vocab.counts.t) = Vocab.Trunc[,1] 

Vocab.Trunc.index_1 = which(Min.Vocab.t>=lower_thresh & Max.Vocab.t <= upper_thresh)
Vocab_Map = Vocab.Trunc[Vocab.Trunc.index_1,]
Vocab.Trunc.index_1 = Vocab.Trunc.index_1[ is.na( as.numeric(Vocab_Map[,1])) ]
Vocab_Map = Vocab_Map[ is.na( as.numeric(Vocab_Map[,1])), ]

Vocab_Map = cbind(Vocab_Map,Vocab.Trunc.index_1,Min.Vocab.t[Vocab.Trunc.index_1],Max.Vocab.t[Vocab.Trunc.index_1],Tot.Vocab.t[Vocab.Trunc.index_1] )
colnames(Vocab_Map) = c("Full", "Stem", "Index", "Min", "Max", "Total")
#write this to CSV to hand select terms
write.csv(file = paste("~/Desktop/DLTM/DLTM-master/Data/Vocab_Map_Long",Corpus_name,".csv",sep=""), x = Vocab_Map )
#Manually identify vocabulary terms.  Create column called Keep.
#Read back in and remove un-necessary terms
Vocab_Map = read.csv(paste("~/Desktop/DLTM/DLTM-master/Data/Vocab_Map_Long",Corpus_name,".csv",sep=""), header=T, stringsAsFactors=F)
Vocab_Map = Vocab_Map[Vocab_Map[,"Keep"]==1,]

write.csv(file = paste("~/Desktop/DLTM/DLTM-master/Data/Vocab_Map_Short",Corpus_name,".csv",sep=""), x = Vocab_Map )
#Manually identify abbreviations and words that point to the same root.
Vocab_Map = read.csv(paste("~/Desktop/DLTM/DLTM-master/Data/Vocab_Map_Short",Corpus_name,".csv",sep=""), header=T, stringsAsFactors=F)
V = dim(Vocab_Map)[1]
save(file=paste("~/Desktop/DLTM/DLTM-master/Data/Vocab_Map_",Corpus_name,".RData",sep=""),V,Vocab_Map)

