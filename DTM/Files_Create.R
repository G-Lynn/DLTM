rm(list=ls())
load("~/SCIOME/Code/SynDataLinear.RData")
foo.mult = "~/Desktop/dtm_release/dtm/example/SynLin-mult.dat"
foo.seq = "~/Desktop/dtm_release/dtm/example/SynLin-seq.dat"
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