rm(list=ls())
library(zoo)
Corpus_name = "Listings"
text = readRDS(file = "~/Desktop/DLTM/Z_Listings_Seattle.rds")
load(file = paste("~/Desktop/DLTM/DLTM-master/Data/",Corpus_name,".RData",sep=""))
load(file=paste("~/Desktop/DLTM/DLTM-master/Data/Vocab_Map_",Corpus_name,".RData",sep=""))

#Years = sort( unique(text$Year) )
DatesYM = as.yearmon(text$FirstPostDate)
text$DatesYM = DatesYM

Dates = unique(DatesYM) 
Dates = sort(Dates)
nDates = length(Dates)

ym.index = list()
DynCorpus = list()
D = rep(NA,nDates)
W = list()
N.D = list()
doc_length_thresh = 20 #this is like saying the CLT only takes 20 samples to kick in.  Aggressive


Abbreviations = Vocab_Map[!is.na(Vocab_Map$Root_Index),c("Index","Root_Index")]
Vocab_Map = Vocab_Map[is.na(Vocab_Map$Root_Index),]
Vocab_index = as.numeric( Vocab_Map$Index )
V = dim(Vocab_Map)[1]
for(t in 1:nDates){
  ym.index[[t]] = which(text$DatesYM==Dates[t])
  nWords = NULL
  WordDoc = NULL
  index = 0
  N.docs.t = length(ym.index[[t]])
  
  for(i in 1:N.docs.t){
    #replace abbreviations with root
    if( any( is.element(Listings[[ ym.index[[t]][i] ]]$Description_Numeric,Abbreviations$Index) ) ){
      #print(paste("t = ",t, "; i = ", i))
      abbrev_index = which(is.element(Listings[[ ym.index[[t]][i] ]]$Description_Numeric,Abbreviations$Index))
      for(ii in abbrev_index){
        Listings[[ ym.index[[t]][i] ]]$Description_Numeric[ii] = Abbreviations$Root_Index[Abbreviations$Index == Listings[[ ym.index[[t]][i] ]]$Description_Numeric[ii] ]
      }
    }
    
    nW_it = length( Listings[[ ym.index[[t]][i] ]]$Description_Numeric )
    if(is.null(Listings[[ ym.index[[t]][i] ]]$Description_Numeric ) | nW_it < 50){next}
    #words in the abstract that are only elements of the Truncated vocabulary
    word_index = which( is.element(Listings[[ ym.index[[t]][i] ]]$Description_Numeric, Vocab_index) )
    n_words_trunc = length(word_index)
    if(n_words_trunc<doc_length_thresh) next
    index = index + 1
    
    
    #the words themselves as the index in the full vocabulary
    words = Listings[[ ym.index[[t]][i] ]]$Description_Numeric[word_index]
    #relabel the words by the order they take in the Truncated Vocabulary
    Words = sapply(1:n_words_trunc, function(i) which(Vocab_index==words[i]))
    #I think that I can thin over-represented words here.  
    
    
    #append the Words to the time-specific matrix keeping the document they came from
    WordDoc = rbind( WordDoc, data.frame(Words = Words, Doc = rep(index, n_words_trunc)) )
    nWords = c(nWords, n_words_trunc )
    
  }
  D[t] = length(nWords)
  W[[t]]  = as.matrix(WordDoc)
  N.D[[t]] = nWords
}

#How many documents are there?  
Vocab_Map_Final = Vocab_Map

save(file = paste("~/Desktop/DLTM/DLTM-master/Data/DynCorp_",Corpus_name,".RData",sep=""),W,D,N.D,V,Vocab_index, nDates, Vocab_Map_Final, Dates)

