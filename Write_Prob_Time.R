Write_Prob_Time<-function(fnames_Eta_t, Eta_t){
  Prob_t = exp(Eta_t)/apply(exp(Eta_t),1,sum)
  write.table(file = fnames_Eta_t, x = Prob_t, sep=",",append=T,col.names=F)
}