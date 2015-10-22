rm(list=ls())
nSamples = 100000

library(BayesLogit)
library(rbenchmark)
library(Rcpp)
library(ggplot2)

sourceCpp("~/SCIOME/Code/Cpp/rpgApprox.cpp")
rpg.NIPS<-function(nSamples,n,m,rho){
  z.m = rpg(nSamples, h = m, z = rho)
  E.y = n/(2*rho)*tanh(rho/2)
  E.z = m/(2*rho)*tanh(rho/2)
  y.m = sqrt(n/m) * (z.m - E.z) + E.y  
  return(y.m)
}

n = 150
m = 5
rho = -1
y = rpg(nSamples,h=n, z = rho)
z.cpp = rpgApproxCpp(nSamples,n,rho)
y.m = rpg.NIPS(nSamples,n,m,rho)

p1 = hist(y, breaks = 50, col = "black", freq = FALSE)
p2 = hist(y.m, breaks = 50, col = "red", add=T, freq = FALSE)
p3 = hist(z.cpp, breaks = 50, col = "blue", add=T, freq = FALSE)


ID = c("Gaussian", "Polya-Gamma")
df = data.frame(Method = rep(ID,each=nSamples), PG = c(z.cpp,y) )


pdf("~/Dropbox/Writing/Figures/rpg_Gaussian_100_m1.pdf")
g = ggplot(df) + geom_histogram(aes(x=PG, fill=Method), color="grey50", alpha=.5, position = "identity")
g + theme(axis.text = element_text(size=20, color = "black"), axis.title=element_text(size=24,face="bold"),legend.text=element_text(size=20) )
dev.off()

ID = c("Chen et al.", "Polya-Gamma")
df = data.frame(Method = rep(ID,each=nSamples), PG = c(y.m,y) )


pdf("~/Dropbox/Writing/Figures/rpg_Chen_100_m1.pdf")
g = ggplot(df) + geom_histogram(aes(x=PG, fill=Method), color="grey50", alpha=.5, position = "identity")
g + theme(axis.text = element_text(size=20, color = "black"), axis.title=element_text(size=24,face="bold"),legend.text=element_text(size=20) )
dev.off()



n = 100
m = 5
#there is a huge discontinuity between 7 and 8.  at rho = 7 good approx.  at rho = 8...terrible
rho = 8
y = rpg(nSamples,h=n, z = rho)
z.cpp = rpgApproxCpp(nSamples,n,rho)
y.m = rpg.NIPS(nSamples,n,m,rho)

ID = c("Gaussian", "Polya-Gamma")
df = data.frame(Method = rep(ID,each=nSamples), PG = c(z.cpp,y) )


pdf("~/Dropbox/Writing/Figures/rpg_Gaussian_100_8.pdf")
g = ggplot(df) + geom_histogram(aes(x=PG, fill=Method), color="grey50", alpha=.5, position = "identity")
g + theme(axis.text = element_text(size=20, color = "black"), axis.title=element_text(size=24,face="bold"), legend.text=element_text(size=20))
dev.off()

ID = c("Chen et al.", "Polya-Gamma")
df = data.frame(Method = rep(ID,each=nSamples), PG = c(y.m,y) )


pdf("~/Dropbox/Writing/Figures/rpg_Chen_100_8.pdf")
g = ggplot(df) + geom_histogram(aes(x=PG, fill=Method), color="grey50", alpha=.5, position = "identity")
g + theme(axis.text = element_text(size=20, color = "black"), axis.title=element_text(size=24,face="bold"), legend.text=element_text(size=20))
dev.off()

m = 5
nSamples = 1000
N.D = rpois(nSamples,150)
rho = rnorm(nSamples, 0, 1)

Comp.Table = benchmark(BayesLogit = rpg(nSamples,N.D,rho), Gaussian = rpgApproxCpp(nSamples,N.D,rho), Chen = rpg.NIPS(nSamples,N.D,m,rho), order="relative", columns = c("test","replications","elapsed","relative"))

library(xtable)
xtable(Comp.Table)
