rm(list=ls())
load("~/Desktop/DLTM_check/DLTM/Data/DynCorp_Listings.RData")
t.T = length(D)
foo.mult = "~/Desktop/DLTM_check/DLTM/Variational/Listings-mult.dat"
foo.seq = "~/Desktop/DLTM_check/DLTM/Variational/Listings-seq.dat"
#foo.mult = "~/Desktop/dtm_release/dtm/example/SynHarmonic-mult.dat"
#foo.seq = "~/Desktop/dtm_release/dtm/example/SynHarmonic-seq.dat"
file.remove(foo.mult)
file.remove(foo.seq)
cat(file = foo.seq, paste(t.T,"\n",sep="") )
for(t in 1:t.T){
  cat(file = foo.seq, paste(D[t],"\n",sep=""),append=T)
  for(d in 1:D[t]){
    W_dt = W[[t]][W[[t]][,2]==d, 1 ]
    index =sort( unique(W_dt) )
    nUnique = length(index)
    cat(nUnique, file = foo.mult, append=T)
    for(n in index){
      count = sum( W_dt == n )
      cat(paste(" ",n-1,":",count,sep=""), file = foo.mult, append=T)
    }
    cat("\n", file = foo.mult, append=T)
  }
  
}
