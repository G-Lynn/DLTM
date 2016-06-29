rm(list=ls())
#Corpus_name = "Epigen"
Corpus_name = "Pubmed_85_15"
text = read.csv(paste("~/SCIOME/Data/",Corpus_name,".csv",sep=""),header=T,stringsAsFactors=F)
load(file = paste("~/SCIOME/Data/",Corpus_name,".RData",sep=""))


#Years = sort( unique(text$Year) )
Years = 1985:2014
nYears = length(Years)
year.index = list()
DynCorpus = list()
D = rep(NA,nYears)
W = list()
N.D = list()
Vocab.Trunc.index = which(Vocab.counts>=25)
Vocab.Trunc = Vocabulary[Vocab.Trunc.index,]
V = length(Vocab.Trunc.index)
Vocab.counts.t = matrix(nrow = nYears,ncol = V)

for(t in 1:nYears){
  year.index[[t]] = which(text$Year==Years[t])
  N.docs.t = length(year.index[[t]])
  for(v in 1:V){
    N.v =0
    for(d in 1:N.docs.t){
      if(is.null(Corpus[[ year.index[[t]][d] ]][[6]] ) | length(Corpus[[ year.index[[t]][d] ]][[6]]) < 50){next}
      N.v = N.v + sum( Corpus[[ year.index[[t]][d] ]]$Abstract.numeric==Vocab.Trunc.index[v])
    }
    Vocab.counts.t[t,v] = N.v
  }
}

#save(file=paste("~/SCIOME/Data/Vocab_Counts_",Corpus_name,".RData",sep=""),Vocab.counts.t)
load(file=paste("~/SCIOME/Data/Vocab_Counts_",Corpus_name,".RData",sep=""))
lower_thresh = 10 #15 seems to work
upper_thresh = 5000 #3000 seemed to work well.
#upper_thresh = max(Vocab.counts.t)
Min.Vocab.t = apply(Vocab.counts.t,2,min)
Max.Vocab.t = apply(Vocab.counts.t,2,max)
sum(Min.Vocab.t >= lower_thresh & Max.Vocab.t <= upper_thresh)
colnames(Vocab.counts.t) = Vocab.Trunc[,1] 

Vocab.Trunc.index_1 = which(Min.Vocab.t>=lower_thresh & Max.Vocab.t <= upper_thresh)
Vocab_Map = Vocab.Trunc[Vocab.Trunc.index_1,]
Vocab.Trunc.index_1 = Vocab.Trunc.index_1[ is.na( as.numeric(Vocab_Map[,1])) ]
Vocab_Map = Vocab_Map[ is.na( as.numeric(Vocab_Map[,1])), ]

Vocab_Map = cbind(Vocab_Map,Vocab.Trunc.index_1,Min.Vocab.t[Vocab.Trunc.index_1],Max.Vocab.t[Vocab.Trunc.index_1])

#write this to CSV to hand select terms
write.csv(file = paste("~/SCIOME/Data/Vocab_Map_",Corpus_name,".csv",sep=""), x = Vocab_Map )
#Manually identify vocabulary terms
#Read back in and remove un-necessary terms
Vocab_Map = read.csv(paste("~/SCIOME/Data/Vocab_Map_",Corpus_name,".csv",sep=""), header=T, stringsAsFactors=F)
Vocab_Map = Vocab_Map[Vocab_Map[,"Keep"]==1,2:4]
names(Vocab_Map) = c("Full", "Stem", "Index")
V = dim(Vocab_Map)[1]
save(file=paste("~/SCIOME/Data/Vocab_Map_",Corpus_name,".RData",sep=""),V,Vocab_Map)

