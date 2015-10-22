rm(list=ls())
library(xlsx)
Corpus_name = "Pubmed_85_15"
Data = read.xlsx(file="~/SCIOME/Data/Pubmed_85_15.xlsx",sheetIndex=1)
write.csv(file="~/SCIOME/Data/Pubmed_85_15.xlsx",x=Data,row.names=F)
