Doc_Completion_Prob<-function(init, stem, nCores, t.T, N.D.oos, W.oos, Eta_t, Beta_t){
  
  ZKappa.oos = Z_Step(MC.Cores = nCores, t.T, N.D.oos, Beta_t, W.oos, Eta_t)
  Doc.Completion.Prob = rep(NA, t.T)
  Z_t.oos = list()
  for(t in 1:t.T){
    Z_t.oos[[t]] = ZKappa.oos[[t]][[1]]
    denom = apply(exp(Beta_t[[t]]), 1, sum)
    Doc.Completion.Prob[t] = sum( log( sapply(1:sum(N.D.oos[[t]]), function(i) exp(Beta_t[[t]][Z_t.oos[[t]][i],W.oos[[t]][i,1] ])/ denom[Z_t.oos[[t]][i]] ) ) )
  }
  write.table(t(Doc.Completion.Prob), file = paste(stem,"MCMC_Samples/Init_",init,"/Doc_Comp_Prob.csv", sep=""), append = T, col.names = F, row.names = F )
}
