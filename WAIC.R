rm(list = ls())
library(parallel)
stem = "~/DLTM/DLTM/"
#load(paste(stem,"Data/DynCorp_Listings.RData",sep=""))
load(paste(stem,"Data/SynDataRW.RData",sep=""))
nSamples = 1000
burnIn = 1000
K = 6
logsum<-function(lx){
    retx = max(lx) + log( sum (exp(lx - max(lx) ) ) )
    return(retx)
}   

logmean<-function(lx){
    retx = logsum(lx) - log(length(lx) )
    return(retx)
}

#t.T = nDates

run = 1
#init = paste(K,run,sep="_")
init = paste("syn",K,sep="_")
Beta_MCMC = p_V = list()
for(m in 1:nSamples){
    p_V[[m]] = list()
    for(t in 1:t.T){
        p_V[[m]][[t]] = matrix(nrow=K,ncol=V)
    }
}


SKIP = burnIn*V
for(k in 1:K){
#ptm = proc.time()
    tmp = read.csv(paste(stem,"MCMC_Samples/Init_",init,"/Beta_",k,".csv",sep=""),header=F,stringsAsFactors=F,skip=SKIP,nrows=nSamples*V)
   

    #print(proc.time()-ptm)
    #print(dim(tmp))
    for(m in 1:nSamples){
        for(t in 1:t.T){ 
            index_l = 1 + (m-1)*V
            index_h = m*V  
            beta_k = tmp[index_l:index_h,t+1]
            p_V[[m]][[t]][k,] = exp(beta_k)/sum(exp(beta_k) )
        }
        rm(beta_k)
    }
    rm(tmp)
}

#Now read in the document level things.  
Eta = list()
p_Doc = list()
log.Lik = list()
for(t in 1:t.T){
    Eta[[t]] = p_Doc[[t]] = list()
    log.Lik[[t]] = matrix(nrow = nSamples, ncol = D[t]) 
    tmp = read.csv(paste(stem,"MCMC_Samples/Init_",init,"/Eta_",t,".csv",sep=""),header=F,stringsAsFactors=F,skip=burnIn*D[t],nrows=nSamples*D[t] )
    for(d in 1:D[t]){
        #print(head(tmp))
        eta_d = tmp[tmp[,1]==d,2:(K+1)]
        eta_d = exp(eta_d) / apply(exp(eta_d),1,sum)
        p_Doc[[t]][[d]] = matrix(nrow = nSamples, ncol = V)
        #print(Eta[[t]][[d]][1,])
        for(m in 1:nSamples){
            p_Doc[[t]][[d]][m,] = apply( diag(eta_d[m,]) %*% p_V[[m]][[t]],2,sum)
            log.Lik[[t]][m,d] = sum(log(p_Doc[[t]][[d]][m,W[[t]][W[[t]][,2]==d, 1] ] ) )

        }
       rm(eta_d) 
    } 
rm(tmp)
}


lpd.hat = sum( sapply(1:t.T, function(t) sum( apply(log.Lik[[t]],2,function(lx) logmean(lx) ) ) ) )
p.hat.WAIC = sum( sapply(1:t.T, function(t) sum( apply(log.Lik[[t]],2,var) ) ) )
elpd.hat.WAIC = lpd.hat - p.hat.WAIC
WAIC = -2*elpd.hat.WAIC

print(WAIC)
save(file = paste(stem, "MCMC_Summaries/WAIC_syn",K,".RData",sep=""), WAIC)




    

