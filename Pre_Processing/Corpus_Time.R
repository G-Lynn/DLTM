
rm(list=ls())
Corpus_name = "Pubmed_85_15"
text = read.csv(paste("~/SCIOME/Data/",Corpus_name,".csv",sep=""),header=T,stringsAsFactors=F)
load(file = paste("~/SCIOME/Data/",Corpus_name,".RData",sep=""))
load(file=paste("~/SCIOME/Data/Vocab_Map_",Corpus_name,".RData",sep=""))

#Years = sort( unique(text$Year) )
Years = 1985:2014
nYears = length(Years)
year.index = list()
DynCorpus = list()
D = rep(NA,nYears)
W = list()
N.D = list()
doc_length_thresh = 20 #this is like saying the CLT only takes 20 samples to kick in.  Aggressive
Vocab_index = as.numeric( Vocab_Map[,3] )
for(t in 1:nYears){
  year.index[[t]] = which(text$Year==Years[t])
  nWords = NULL
  WordDoc = NULL
  index = 0
  N.docs.t = length(year.index[[t]])

  for(i in 1:N.docs.t){
    nW_it = length( Corpus[[ year.index[[t]][i] ]]$Abstract.numeric )
    if(is.null(Corpus[[ year.index[[t]][i] ]][[6]] ) | nW_it < 50){next}
    #words in the abstract that are only elements of the Truncated vocabulary
    word_index = which( is.element(Corpus[[ year.index[[t]][i] ]]$Abstract.numeric, Vocab_index) )
    n_words_trunc = length(word_index)
    if(n_words_trunc<doc_length_thresh) next
    index = index + 1
    
    
    #the words themselves as the index in the full vocabulary
    words = Corpus[[ year.index[[t]][i] ]]$Abstract.numeric[word_index]
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

save(file = paste("~/SCIOME/Data/DynCorp_",Corpus_name,".RData",sep=""),W,D,N.D,V,Vocab_index)

