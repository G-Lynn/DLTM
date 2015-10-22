rm(list=ls())
library(BayesLogit)

#this is a test script to examine the truncated density of the PG(b,c) random 
#variable.

b = 100 #count parameter of PG in logistic regression.  I am specifically interested in positive integer cases of b
c = 8 #tilting parameter of PG.  I am observing very different behavior between c = 7.9 and c = 8.0
N = 100 #truncation level for infinite sum

#term_n is the n^th term of the infinite sum starting from n = 0 and going to N
term_n <-function(x,b,c,n){
  #since I am interested in cases where b is an integer I use the Gamma(b) = (b+1)! expression
  #and then simplify to make computation more manageable
  return( (-1)^n * prod( (n+2):(n+b+1) ) * (2*n+b)/sqrt( 2*pi*x^3 ) * exp(-(2*n+b)^2/(8*x) - c^2/2 *x ) )
}

#density function of the PG
f_x<-function(x,b,c,N){
  #Use log representation for the scaling factor 2^{b-1}/ Gamma(b) and then exponentiate
  log_B = (b-1)*log(2) - sum( log( 1:(b+1) ) )
  (cosh(c/2))^b * exp(log_B) * sum(sapply(0:N, function(n) term_n(x,b,c,n) ) ) 
}

nGrid = 1000 #examine the density on a grid with 1000 points
density_9 = rep(NA,nGrid) #density values for b = 100, c = 8
density_8 = rep(NA,nGrid) #density values for b = 100, c = 8
density_7 = rep(NA,nGrid) #density values for b = 100, c= 7
density_6 = rep(NA,nGrid) #density values for b = 100, c = 6

x = seq(from = 5, to = 12, len = nGrid) #evaluate the density on interval [5,12]

#compute the density values for each value of c
for(i in 1:nGrid){
  density_9[i] = f_x(x[i],b,9.0,N)
  density_8[i] = f_x(x[i],b,8.0,N)
  density_7[i] = f_x(x[i],b,7.0,N)
  density_6[i] = f_x(x[i],b,6.0,N)
}

#plot of samples of PG(b=100,c=6.0) against the computed density.  Matches exactly
plot(x,density_6, xlim = c(7,10), ylim = c(0,2))
hist( rpg(10000,h=b,z=6.0),add=T, freq=F)

#plot of samples from PG(b=100,c=7.0) against the computed density.  Matches exactly
plot(x,density_7, xlim = c(6,8.5), ylim = c(0,2))
hist( rpg(10000,h=b,z=7.0),add=T, freq=F)

# Somewhere between c = 7 and c = 8 the density and the sampling start to disagree.
# It gets worse as c increases

#plot of samples from PG(b=100,c=8.0) against the computed density.  Does not match.
plot(x,density_8, xlim = c(5,8), ylim = c(0,5))
hist( rpg(10000,h=b,z=8.0),add=T, freq=F)

#plot of samples from PG(b=100,c=9.0) against computed density.  Does not match.
plot(x,density_9, xlim = c(5,7), ylim = c(0,6))
hist( rpg(10000,h=b,z=9.0),add=T, freq=F)