# EDA for text
rm(list=ls())
.libPaths(c(.libPaths(),"/home/grad/cdg28/R/x86_64-redhat-linux-gnu-library/3.1"))
library(SnowballC)
library(parallel)
Corpus_name = "Pubmed_85_15"
#Corpus = "Epigen"
text = read.csv(paste("~/SCIOME/Data/",Corpus_name,".csv",sep=""),header=T,stringsAsFactors=F)
Stop.Words = read.csv("~/SCIOME/Data/StopWords.csv",header=F, stringsAsFactors=F)
Stop.Words = as.character(Stop.Words[1,])

D = dim(text)[1]
Corpus = list()
document_prototype = list(PMID = 0, Year = NA, Title = "", Authors = "", Journal = "", Abstract = NA )

Vocabulary = matrix(c("the","the"), nrow = 1)
Numerals = as.character(0:10)
Characters = c(letters[1:26], LETTERS[1:26])
Stop.Words = c(Stop.Words, "", Numerals, Characters,"na")

for(d in 1:D){
  abstract = paste(text[d,"Title"],text[d,"Title"],text[d,"Title"], text[d,"Abstract"])
  abstract = gsub("[[:punct:]]", " ", abstract)
  abstract = gsub(" ",",",abstract)
  abstract = gsub("\\s", "", abstract) 

  abstract = tolower(abstract)
  abstract = unlist( strsplit(abstract, c(",") ) )
  abstract = abstract[!is.element(abstract,Stop.Words)]
  #message(d)
  if(length(abstract)<30 | any( is.na(abstract) ) | is.null(abstract) ) next
  Corpus[[d]] = document_prototype
  Corpus[[d]]$PMID = text[d,"PMID"]
  Corpus[[d]]$Year = text[d,"Year"]
  Corpus[[d]]$Title = text[d,"Title"]
  Corpus[[d]]$Authors = text[d,"Authors"]
  Corpus[[d]]$Journal = text[d,"Journal"]
  Full = abstract
  Corpus[[d]]$Abstract = wordStem(abstract)
  Vocabulary = rbind(Vocabulary, cbind(Full, Corpus[[d]]$Abstract) )
  Vocabulary = Vocabulary[!duplicated(Vocabulary[,2]),]
}

for(d in 1:D){
  n.D = length(Corpus[[d]]$Abstract)
  Corpus[[d]][[7]] = sapply(1:n.D, function(i) which(Corpus[[d]]$Abstract[i] == Vocabulary[,2] ) )
  names(Corpus[[d]])[7] = "Abstract.numeric"
}


# For now keep all of the words.  This map Abstract Numeric is the numeric representation of the document
nWords = dim(Vocabulary)[1]
Vocab.counts = sapply(1:nWords, function(i) sum( sapply(1:D, function(d)  sum( is.element(Corpus[[d]]$Abstract,Vocabulary[i,2]) ) ) ) )
#Vocabulary = Vocabulary[Vocab.counts>3,]
save(file = paste("~/SCIOME/Data/",Corpus_name,".RData",sep=""), Corpus, Vocabulary, Vocab.counts)

