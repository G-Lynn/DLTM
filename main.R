rm(list=ls())
#install.packages(c("Rcpp","RcppArmadillo","parallel") )
library(MASS)           #required for the multivariate normal sampler
library(parallel)       #required for mclapply and parallel computation 
library(Rcpp)           #required for compiling C++ code into R functions
library(RcppArmadillo)  #required for Matrix operations in C++

#User defined inputs
stem = "/home/grad/cdg28/DLTM/DLTM/"  # path to the DLTM directory.  Needs to be full path.  No ~/ or .. allowed
#stem = "/Users/christophergl1/Desktop/DLTM_check/DLTM/"
nCores = 8                # Number of cores to use for parallel computation
K = 3                    #the number of topics in the corpus
init = paste("syn",K,sep="_")                  #the MCMC initialization for comparing multiple runs.
Real_Data = FALSE;         #Boolean to use real data or synthetic data
Misspecify_K = TRUE;     #Boolean to misspecify K

#Choose the model for the data
Model = "RW"; p = 1       #Model and dimension of state space
#Model = "Linear"; p = 2 
#Model = "Quadratic"; p = 3
#Model = "Harmonic"; p = 2; 

B = 0 #the number of samples to discard before writing to output
burnIn = 1000 #2000 #4000 #the number of samples to discard before computing summaries
nSamples = 2000 #3000 #6000#
thin = 100 #100 #preferred
N.MC = B + thin*nSamples  #total number of MCMC iterations.

if(Misspecify_K == T) K_prime = K

#-------Load Data-------------------------------------------- ----------------------
#Real Data.  Only load one of these files.
if(Real_Data==TRUE){
    load(file = paste(stem,"Data/DynCorp_Pubmed_85_15.RData",sep="") )
    t.1 = 1
    t.T = length(1985:2014)  #the total number of time points in the corpus
}else{
    load(file = paste(stem,"Data/SynData",Model,".RData",sep="") )  #DTM synthetic data
    K = K_prime
}
#K = 6  #to mis-specify K for synthetic data, enter it here.  It needs to be entered after the synthetic data is loaded because the synthetic data files contain the true number of topics.

#split W into held out sample
oo.sample.pct = .25
W.oos = list()
N.D.oos = list()
for(t in 1:t.T){
  W.oos[[t]] = matrix(nrow = 0, ncol = 2)
  N.D.oos[[t]] = rep(NA,D[t])
  for(d in 1:D[t]){
    n.oos = floor(oo.sample.pct*N.D[[t]][d])
    N.D.oos[[t]][d] = n.oos
    N.D[[t]][d] = N.D[[t]][d] - n.oos
    
    doc.index = which(W[[t]][,2] == d )
    index = sample(doc.index, size = n.oos,  replace=F )
    tmp = matrix(nrow = n.oos, ncol = 2)
    
    tmp[,1] = W[[t]][index,1]
    tmp[,2] = rep(d,n.oos)
    W.oos[[t]] = rbind(W.oos[[t]], tmp)
    W[[t]] = W[[t]][-index,]
  }
}
#-------Load MCMC Wrappers for parallelization and C++ functions ----------------------

#Now load required functions

#Alpha step
source( paste(stem,"Alpha_Step.R",sep="") )
sourceCpp(paste(stem,"Cpp/Alpha_k_FFBS.cpp",sep="") )

# Beta step
source(paste(stem,"Beta_Step.R",sep="") )
sourceCpp(paste(stem, "Cpp/Beta_k_FFBS.cpp", sep="") )

# Eta step
source(paste(stem,"Eta_Step.R",sep="") )
sourceCpp(paste(stem,"Cpp/Eta_dkt.cpp",sep="") )

# Omega step
source(paste(stem,"Omega_Step.R",sep="") )
sourceCpp(paste(stem,"Cpp/Omega_dkt.cpp",sep="") )

# Zeta Step
source(paste(stem,"Zeta_Step.R",sep="") )
sourceCpp(paste(stem,"Cpp/Zeta_kvt.cpp",sep="") )

# Z (Topic assignment) Step
source(paste(stem,"Z_Step.R",sep="") )
sourceCpp(paste(stem,"Cpp/Z_ndt.cpp",sep="") )

#--------Utility functions ----------------
source(paste(stem,"Alpha_Initialize.R",sep="") )       #initialize Alpha
source(paste(stem, "Beta_Initialize.R",sep="") )        #initialize Beta
sourceCpp(paste(stem,"Cpp/rpgApprox.cpp",sep="") )     # Approximate PG sampler in C++
source(paste(stem,"Write_Prob_Time.R",sep="") )        # Wrapper for parallelization with write.table
source(paste(stem,"Doc_Prob.R", sep="") )


########################################################################################
message(paste("Init = ", init) )
message(paste("K = ", K) )
message(paste("V = ", V) )

#Specify hyper-parameters

#---Beta---------------------------------
#prior at t = 0 for each v-th term in the k-th topic
m_beta_kv0 = 0 #mean of m_kv0
sigma2_beta_kv0 = 1 # 1 works well for K=6 topic mis-specification

#sigma2 the variance of beta_t | beta_{t-1}
sigma2 = .01 # .001 #works for V = 1000


#---ALPHA--------------------------------
#prior at t=0 for each alpha^{p x 1} state vector in the k-th topic

#m_alpha_k0 = matrix(rep(0,p), nrow = p, ncol = 1)  #prior mean  
#C_alpha_k0 = .1*diag(p) 
#delta = rep(.025,p)

m_alpha_k0 = list()
C_alpha_k0 = list()
delta = list()

for(k in 1:K){
  m_alpha_k0[[k]] = matrix(rep(0,p), nrow = p, ncol = 1)  #prior mean
  #C_alpha_k0[[k]] = matrix(c(.01,0,0,.0000001), nrow = p, ncol = p)
  C_alpha_k0[[k]] = .1*diag(p) 
  #delta[[k]] = c(.005,.00001)
  delta[[k]] = rep(.025,p)
}

a2 = .25  


#set up for document level Design Matrices at each time
#---SYSTEM MATRICES----------------------
FF = list()   #Since each time has its own matrix, we create a list
F_dkt = c(1,rep(0,p-1)) #the combination of F and G will correspond to a locally linear trend

## FF_kt is a D_t x p matrix.  We assume it is the same for all k.  

for(t in 1:t.T){
  FF[[t]] = matrix(rep(F_dkt,times = D[t]) ,nrow = D[t], ncol = p , byrow=T)
}


GG = list()
for(k in 1:K){
  GG[[k]] = matrix(0,nrow = p, ncol = p)
  GG[[k]][upper.tri(GG[[k]],diag=T)]=1
  if(Model == "Harmonic") GG[[k]] = matrix(c(cos(omega[k]), sin(omega[k]), -sin(omega[k]), cos(omega[k]) ), byrow=T, ncol=p)
}

########################################################################################
#MCMC specifics and initialization

#---ALPHA Initialization-------------------------------
Alpha_k = Alpha_t = list()

for(k in 1:K ) Alpha_k[[k]] = Alpha_Initialize(m_alpha_k0[[k]], GG[[k]], C_alpha_k0[[k]]/100, delta[[k]], t.T)

for(t in 1:t.T ){
  Alpha_t[[t]] = matrix(nrow = p, ncol = K )
  for(k in 1:K){
    Alpha_t[[t]][,k] = Alpha_k[[k]][,t]
  }
}
#----BETA Initialization---------------------
Beta_k = Beta_t = list()
sigma2_beta_init = 2*sigma2_beta_kv0;  #last = .5 the initialization variance
for(k in 1:K){
  Beta_k[[k]] = Beta_Initialize(m_beta_kv0, sigma2_beta_init, sigma2, V, t.T)
}

# We will need both the list indexed by K, and the list indexed by t.  
for(t in 1:t.T){
  Beta_t[[t]] = matrix(0,nrow = K, ncol = V)
  for(k in 1:K){
    Beta_t[[t]][k,] = Beta_k[[k]][,t]   
  }
}

#----ETA and OMEGA Initialization---------------------
#OMEGA are the auxiliary PG random variables for ETA.  
Eta_t = Omega_t = Eta_k = list()
for(t in 1:t.T){
  Eta_t[[t]] = matrix(0,nrow = D[t], ncol = K )  
  Omega_t[[t]] = matrix(0,nrow = D[t],ncol = K )
  for(k in 1:K) Eta_t[[t]][,k] = FF[[t]]%*%Alpha_k[[k]][,t] + rnorm(D[t],0,sqrt(a2) )
  
  for(k in 1:K ){
    psi_k = Eta_t[[t]][,k] - log( apply( exp( Eta_t[[t]][,-k]) ,1,sum ) )  
    Omega_t[[t]][,k] = rpgApproxCpp(D[t], as.numeric(N.D[[t]]), z = psi_k )
  }  
}

for(k in 1:K ){
  Eta_k[[k]] = list()
  for(t in 1:t.T){
    Eta_k[[k]][[t]] = as.matrix( Eta_t[[t]][,k] )
  }
}

#---Z initialization-----------------------------------
Kappa_Beta_k = Kappa_Eta_t = Z_t = list()
ZKappa = Z_Step(MC.Cores = nCores, t.T, N.D, Beta_t, W, Eta_t)
ny = matrix(nrow = K, ncol = t.T)
for(k in 1:K) Kappa_Beta_k[[k]] = matrix(0,nrow=V,ncol=t.T)
for(t in 1:t.T){
    Z_t[[t]] = ZKappa[[t]][[1]]
    ny[,t] = ZKappa[[t]][[4]]
    Kappa_Eta_t[[t]] = ZKappa[[t]][[3]]
    for(k in 1:K) Kappa_Beta_k[[k]][,t] = ZKappa[[t]][[2]][k,]
}

#-----ZETA Initialization---------------------------
# Initialize Zeta as a list indexed by k.
Zeta_k = list()
Zeta_k = Zeta_Step(MC.Cores = nCores, K, V, ny, ny_thresh, Beta_k)

#save(file="~/SCIOME/Code/DLTM.RData", Alpha_k, Alpha_t, Beta_k, Beta_t, D, Eta_k, Eta_t, FF, G, Kappa_Beta_k, Kappa_Eta_t, N.D, Omega_t, V, W, Zeta_k)
########################################################################################
#MCMC

unlink(paste(stem,"MCMC_Samples/Init_",init,sep=""), recursive=T)
dir.create(paste(stem,"MCMC_Samples/Init_",init,sep=""))

fnames_Beta = paste(stem,"MCMC_Samples/Init_",init,"/Beta_",1:K,".csv",sep="")
fnames_Alpha = paste(stem,"MCMC_Samples/Init_",init,"/Alpha_",1:K,".csv",sep="")
fnames_Eta = paste(stem,"MCMC_Samples/Init_",init,"/Eta_",1:t.T,".csv",sep="")

ptm = proc.time()
for(m in 1:N.MC){
  #-----Beta Step-------------------------------------
  beta_index = sample(0:(V-1), V, replace=F)
  Beta_k = Beta_Step(MC.Cores = nCores, K, fnames_Beta, beta_index, m, B, thin, m_beta_kv0, sigma2_beta_kv0, sigma2, Kappa_Beta_k, Zeta_k, Beta_k)
  
  #-----Zeta Step------------------------------------    
  Zeta_k = Zeta_Step(MC.Cores = nCores, K, V, ny, ny_thresh, Beta_k)

  #-----Eta Step-------------------------------------
  for(t in 1:t.T ){
    for(k in 1:K ){
      Alpha_t[[t]][,k] = Alpha_k[[k]][,t]
    }
  }
  
  eta_index = sample(0:(K-1), K, replace=F)
  Eta_t = Eta_Step(MC.Cores = nCores, K,fnames_Eta, eta_index, m, B, thin, a2,D,FF,Alpha_t,Omega_t,Kappa_Eta_t,Eta_t)
  
  #-----Omega Step-----------------------------------
  Omega_t = Omega_Step(MC.Cores = nCores, t.T, N.D, Eta_t)

  #-----Alpha Step-----------------------------------
  for(k in 1:K ){
    Eta_k[[k]] = list()
    for(t in 1:t.T){
      Eta_k[[k]][[t]] = Eta_t[[t]][,k]
    }
  }
  Alpha_k =  Alpha_Step(MC.Cores = nCores, K, fnames_Alpha, m, B, thin, t.T, a2, delta, m_alpha_k0, C_alpha_k0, FF, GG, Eta_k )
  #-----Z Step---------------------------------------
  #Remember to update Beta_t 
  for(t in 1:t.T){
    for(k in 1:K ){
      Beta_t[[t]][k,] = Beta_k[[k]][,t]   
    }
  }
  
  ZKappa = Z_Step(MC.Cores = nCores, t.T, N.D, Beta_t, W, Eta_t)
  for(t in 1:t.T){
    Z_t[[t]] = ZKappa[[t]][[1]]
    ny[,t] = ZKappa[[t]][[4]]
    Kappa_Eta_t[[t]] = ZKappa[[t]][[3]]
    for(k in 1:K) Kappa_Beta_k[[k]][,t] = ZKappa[[t]][[2]][k,] 
  }
  
  if( (m>B) && (m %% thin == 0) ) Doc_Completion_Prob(init, stem, nCores, t.T, N.D.oos, W.oos, Eta_t, Beta_t)

  if(m%%1000==0){
     message(paste("Init: ",init,"; Sample: ",m,sep=""))
  }
}#end MCMC
message(proc.time()[3]-ptm[3])
source(paste(stem,"post_summaries.R",sep="") )

