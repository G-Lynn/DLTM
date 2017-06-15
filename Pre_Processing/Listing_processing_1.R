rm(list = ls())
library(zoo)
library(SnowballC)

Data = readRDS(file = "~/Desktop/DLTM/Z_Listings_Seattle.rds")
load("~/Desktop/DLTM/DLTM-master/Pre_Processing/stopWords.RData")
DatesYM = as.yearmon(Data$FirstPostDate)
Data$DatesYM = DatesYM
#bin these by month year. 

Listings = list()
listing_prototype = list(YearMon = NA, ListPrice = NA, SalePrice = NA, SqFeet = NA, Bedroom = NA, Bathroom = NA, Neighborhood = NA, Description = NA, Description_Numeric = NA )

Dates = unique(DatesYM) 
Dates = sort(Dates)
nDates = length(Dates)
nListings = rep(NA,nDates)

for(i in 1:nDates){
  nListings[i] = length( Data$DatesYM[Data$DatesYM==Dates[i]] )
}

pdf("~/Desktop/DLTM/Listings_v_Time.pdf")
plot(Dates,nListings, type = "l")
dev.off()

D = dim(Data)[1]
Vocabulary = matrix(c("home","home"), nrow = 1)
for(i in 1:D){
  listing = Data$ListingDescription[i]
  listing = gsub("[[:punct:]]", " ", listing)
  listing = gsub(" ",",",listing)
  listing = gsub("\\s", "", listing)
  
  listing = tolower(listing)
  listing = unlist( strsplit(listing, c(",") ) )
  listing = listing[!is.element(listing, stopWords)]
  listing = listing[listing!=""]
  Full = listing
  
  if(length(listing)<30 | any( is.na(listing) ) | is.null(listing) ) next
  Listings[[i]] = listing_prototype
  Listings[[i]]$YearMon = Data$DatesYM[i]

  Listings[[i]]$ListPrice = Data$OrigListingPrice[i]
  Listings[[i]]$SalePrice = Data$SaleDollarCnt[i]
  Listings[[i]]$SqFeet = Data$FinishedSquareFeet[i]
  Listings[[i]]$Bedroom = Data$BedroomCnt[i]
  Listings[[i]]$Bathroom = Data$BathroomCnt[i]
  Listings[[i]]$Neighborhood = Data$NeighborhoodName[i]
  Listings[[i]]$Description = wordStem(listing)
  
  Vocabulary = rbind(Vocabulary, cbind(Full, Listings[[i]]$Description) )
  Vocabulary = Vocabulary[!duplicated(Vocabulary[,2]),]
}

for(i in 1:D){
  n.D = length(Listings[[i]]$Description)
  Listings[[i]]$Description_Numeric = sapply(1:n.D, function(ii) which(Listings[[i]]$Description[ii] == Vocabulary[,2] ) )
}

nWords = dim(Vocabulary)[1]
Vocab.counts = sapply(1:nWords, function(i) sum( sapply(1:D, function(d)  sum( is.element(Listings[[d]]$Description,Vocabulary[i,2]) ) ) ) )
#Vocabulary = Vocabulary[Vocab.counts>3,] #Keep only words that appear more than 10 times.  Seems reasonable.  
save(file = "~/Desktop/DLTM/DLTM-master/Data/Listings.RData", Listings, Vocabulary, Vocab.counts)


