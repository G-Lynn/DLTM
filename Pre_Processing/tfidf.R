rm(list=ls())
load("~/SCIOME/Data/Epigen.RData")

library(parallel)
Vocabulary = Vocabulary[which(Vocab.counts>9),]
nWords = dim(Vocabulary)[1]
print(nWords)
nDocs = length(Corpus)

tf.assign<-function(i){
  row = sapply(1:nDocs, function(d) sum( is.element(Corpus[[d]]$Abstract, Vocabulary[,2]) ) )
  row = matrix(row,nrow=1)
  return(row)
}

tf = matrix( unlist( mclapply(1:nWords, function(i) tf.assign(i), mc.cores = 8 ) ), byrow=T, nrow = nWords)
tf.binary = (tf>0)
df = apply(tf.binary, 1, sum)

tf.idf = tf/df
var.tf.idf = apply(tf.idf,1,var)
Vocab.Trimmed = Vocabulary[var.tf.idf>quantile(var.tf.idf,prob = c(.75))]
save(file = "~/SCIOME/Data/tfidf.RData", tf, var.tf.idf,Vocabulary, Vocab.Trimmed)

#load(file = "~/SCIOME/Data/tfidf.RData")
#Vocab.ordered = data.frame( Vocab = Vocabulary[order(var.tf.idf, decreasing=TRUE)], Var = sort(var.tf.idf, decreasing = TRUE), Counts = Vocab.counts[order(var.tf.idf, decreasing=TRUE)]  )
#hist(var.tf.idf)
#summary(var.tf.idf)

#Vocab.1 = data.frame(Vocabulary[Vocab.counts == 1] )


#ant = read.csv("~/Dropbox/antonyms_new_database_v2.csv",header=T,stringsAsFactors=F)
