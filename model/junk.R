#=========================================================================#
# Simulation Study
#  - Parallelized (for speed)
#  - random water needs
#  - stationary payment policy
#  - STATE = (Owed (O), Debt (D), Water Bill Component (B))
#  - POLICY = (Water Consumption (W), Expenditure on Bill (E))
#
# JP Dube, Oct. 6, 2024
#=========================================================================#


#---------+---------+---------+---------+---------+---------+
# PRELIMINARIES
#---------+---------+---------+---------+---------+---------+

# Set working directories
if (Sys.info()[4]=="JDUBE-LT3"){
  wd = "C:/Users/jdube/Box/PWB"
} else if(Sys.info()[4]=="jdube01") {
  wd = "/data/PWB"
} else {
  wd <- paste0(dirname(rstudioapi::getSourceEditorContext()$path), "/../..")}
setwd(wd)
wd <- getwd()
code_dir <- paste0(wd, "/code/model")
data_dir <- paste0(wd, "/data/raw/servus")
auxiliary_data_dir <- paste0(wd, "/data/auxiliary")
working_data_dir <- paste0(wd, "/data/analysis")
output_dir <- paste0(code_dir, "/output")

# Open libraries
#library(mpoly)
library(pracma)

# Open utility functions
source(paste(code_dir,"/Chebyshev.R",sep=""))
source(paste(code_dir,"/Chebyshevgrid.R",sep=""))
source(paste(code_dir,"/waterutility.R",sep=""))
source(paste(code_dir,"/statetrans.R",sep=""))
source(paste(code_dir,"/Value.R",sep=""))
source(paste(code_dir,"/mkpolicy.R",sep=""))
source(paste(code_dir,"/Value_stochastic.R",sep=""))
source(paste(code_dir,"/mkpolicy_stochastic.R",sep=""))


#---------+---------+---------+---------+---------+---------+
# LOCAL FUNCTIONS
#---------+---------+---------+---------+---------+---------+

# Modified Policy Iteration to compute value function and optimal policies
#  STOCHASTIC INCOME
DPsolve_stochastic = function(beta,mdraw,weights,Theta,Pricing,outCheby,code_dir=code_dir){
  start = Sys.time()
  R = length(mdraw)                 # length integration grid
  # (1) Obtain initial Guesses for optimal policy optimized under myopic model (beta=0)
  Lambda0 = matrix(0,nrow(outCheby$Proj),R)
  Astar0 <- foreach(
    jj = 1:(outCheby$pts*R),
    .combine = 'rbind',
    .packages = c("pracma")
  ) %dopar% {
    source(paste(code_dir,"/statetrans.R",sep=""))
    source(paste(code_dir,"/Value_stochastic.R",sep=""))
    source(paste(code_dir,"/mkpolicy_stochastic.R",sep=""))
    source(paste(code_dir,"/waterutility.R",sep=""))
    ii = floor((jj-1)/R)+1
    rr = jj-(ii-1)*R
    Emax = min(apply(outCheby$grid,1,sum)[ii],mdraw[rr])
    errAstatus= 1
    mkpolicy_stochastic(x0=matrix(c(0,Emax),ncol=2),beta=0,m=mdraw[rr],weights=weights,Theta=Theta,Pricing=Pricing,Chebygrid=outCheby$grid,Lambda=Lambda0,state=matrix(outCheby$grid[ii,],nrow=1),Emax=Emax,errAstatus=errAstatus,maxD=outCheby$maxD,maxB=outCheby$maxB,G=outCheby$G,K=outCheby$K,bound=outCheby$bound)
  }
  Astar = Astar0
  
  # (2) Contract Bellman equation for value function when applying myopic policies
  # pre-compute state transitions for given policies
  Lambda = Lambda1 = Lambda0
  outtrans=util=NULL
  for(rr in 1:R){
    index = (0:(outCheby$pts-1))*R+rr
    outtrans[[rr]] = statetrans(A=Astar0[index,],Pricing=Pricing,Chebygrid=outCheby$grid,state=outCheby$grid,maxD=outCheby$maxD,maxB=outCheby$maxB,G=outCheby$G,K=outCheby$K,bound=outCheby$bound)
    util[[rr]] = waterutility(W=outtrans[[rr]]$W,E=outtrans[[rr]]$E,O=outtrans[[rr]]$O,m=mdraw[rr],Theta=Theta)
  }
  V = matrix(0,outCheby$pts*R,1)
  errV = matrix(1,outCheby$pts,1)
  iter1 = 0
  while(max(abs(errV))>1.e-6){
    V1 = V*0
    Lambda1 = Lambda*0
    for(rr in 1:R){
      index = ((rr-1)*outCheby$pts+1):(rr*outCheby$pts)
      # Value Function Contraction
      Vdraw = outtrans[[rr]]$S1%*%Lambda
      V1[index] = util[[rr]] + beta*(Vdraw%*%weights)/sqrt(pi)
      Lambda1[,rr] = outCheby$Proj%*%V1[index]
    }
    errV = V-V1
    V = V1
    Lambda = Lambda1
    iter1 = iter1+1
  }
  print("Completed Initialization")
  print("Elapsed time: ")
  print(Sys.time()-start)
  
  # (3) Solve Full DP using policy iteration (method of successive approximations)
  errA = matrix(1,outCheby$pts*R,2)
  iter = 0
  errV = matrix(1,outCheby$pts*R,1)
  while( max(abs(errA))>1.e-6 ){
    Aindex = which(apply(abs(errA)>1.e-6,1,sum)>0)
    # (3a) Update Policy
    Astar1 = Astar
    Astar1temp <- foreach(
      j = 1:length(Aindex),
      .combine = 'rbind',
      .packages = c("pracma")
    ) %dopar% {
      source(paste(code_dir,"/statetrans.R",sep=""))
      source(paste(code_dir,"/Value_stochastic.R",sep=""))
      source(paste(code_dir,"/mkpolicy_stochastic.R",sep=""))
      source(paste(code_dir,"/waterutility.R",sep=""))
      jj = Aindex[j]
      ii = floor((jj-1)/R)+1
      rr = jj-(ii-1)*R
      Emax = min(apply(outCheby$grid,1,sum)[ii],mdraw[rr])
      errAstatus= max(abs(errA[(ii-1)*R+rr,]))
      mkpolicy_stochastic(x0=matrix(Astar[ii,],ncol=2),beta=beta,m=mdraw[rr],weights=weights,Theta=Theta,Pricing=Pricing,
                          Chebygrid=outCheby$grid,Lambda=Lambda,state=matrix(outCheby$grid[ii,],nrow=1),
                          Emax=Emax,errAstatus=errAstatus,maxD=outCheby$maxD,maxB=outCheby$maxB,G=outCheby$G,K=outCheby$K,bound=outCheby$bound)
      #mkpolicy_stochastic(x0=c(500,Emax),beta=beta,m=mdraw[rr],weights=weights,Theta=Theta,Pricing=Pricing,
      #                    Chebygrid=outCheby$grid,Lambda=Lambda,state=matrix(outCheby$grid[ii,],nrow=1),
      #                    Emax=Emax,errAstatus=errAstatus,maxD=outCheby$maxD,maxB=outCheby$maxB,G=outCheby$G,K=outCheby$K,bound=outCheby$bound)
    }
    successindex = apply(Astar1temp==-999,1,sum)==0
    Astar1[Aindex,][successindex,] = Astar1temp[successindex,]
    
    # (3b) Bellman contraction-mapping
    iter1 = 0
    # pre-compute state transitions for given policies
    outtrans=util=NULL
    for(rr in 1:R){
      index = (0:(outCheby$pts-1))*R+rr
      outtrans[[rr]] = statetrans(A=Astar1[index,],Pricing=Pricing,Chebygrid=outCheby$grid,state=outCheby$grid,maxD=outCheby$maxD,maxB=outCheby$maxB,G=outCheby$G,K=outCheby$K,bound=outCheby$bound)
      util[[rr]] = waterutility(W=outtrans[[rr]]$W,E=outtrans[[rr]]$E,O=outtrans[[rr]]$O,m=mdraw[rr],Theta=Theta)
    }
    while(max(abs(errV))>1.e-6){
      for(rr in 1:R){
        index = ((rr-1)*outCheby$pts+1):(rr*outCheby$pts)
        keepindex = which(apply(abs(matrix(errA[index,],ncol=2))>1.e-6,1,sum)>0)
        if (length(keepindex)>0){
          # Value Function Contraction
          Vdraw = outtrans[[rr]]$S1[keepindex,]%*%Lambda
          V1[index][keepindex] = util[[rr]][keepindex] + beta*(Vdraw%*%weights)/sqrt(pi)
          Lambda[,rr] = outCheby$Proj[,keepindex]%*%as.matrix(V1[index][keepindex])
        }
      }
      errV = V-V1
      V = V1
      iter1 = iter1+1
    }
    iter = iter+1
    errA = Astar-Astar1
    errA[Aindex,][successindex==FALSE,] = 1
    Astar[Aindex,][successindex,] = Astar1[Aindex,][successindex,]
    if(round(iter/50)==iter/50){
      #if(round(iter/1)==iter/1){
      nonC = sum( apply(abs(errA)>1.e-6,1,sum)>0 )
      print(paste("largest error in policy update: ",round(max(abs(errA)),8)))
      print(paste("# non-converged: ", nonC, " out of ", outCheby$pts*R, " (", nonC/(outCheby$pts*R)*100, "%)",sep=""))
      print(paste("# failed policy updates: ", sum(successindex==0), " out of ", outCheby$pts*R, " (", sum(successindex==0)/(outCheby$pts*R)*100, "%)",sep=""))
      print(paste("Completed Policy iteration #",as.character(iter)))
      print("Elapsed time: ")
      print(Sys.time()-start)
      save(Astar,Lambda,V,errA,errV,successindex,Aindex,Astar1temp,file=paste(code_dir,"temp.Rdata",sep=""))
    }
  }
  return(list(Lambda=Lambda,V=V1,Astar=Astar,iter=iter))
}


#---------+---------+---------+---------+---------+---------+
# Initialize
#---------+---------+---------+---------+---------+---------+
set.seed(42)
N = 500
T = 200

# create state grid
maxD = 400
maxB = 100
bound = matrix(0,2,2)
bound[2,] = cbind(maxD,maxB)

# Construct grid for Chebyshev approximation
#G = 50
G = 100                                    # Number Chebyshev nodes (grid points)
#K = 45                                         # Order of Chebyshev approximation
K = 15
outCheby = Chebyshev(G,K,bound)


# Draw gridpts and weights for Integration using Gauss-Hermite Polynomials
GH = gaussHermite(10)                           # Gauss-Hermite Polynomials


#---------+---------+---------+---------+---------+---------+
# Consumer Preference Parameters
#   2 segments (one with very low mean income)
#---------+---------+---------+---------+---------+---------+
# Set Parameter values
# (1) PWB Pricing and Fines
f = 10                                    # fixed price of water
p = 0.05                                  # linear price of water
dbar = 350                                # shut-off threshold for unpaid debt
Delta = 15                                # Penalty for unpaid debt
delta = .05                               # fine rate for overdue water bill
Pricing = as.matrix(c(p,f,dbar,delta,Delta))
# (2) Preferences
theta = c(0.01,-.00001,-10)               # water consumption utility
alpha = 0.01                              # marginal utility of income
gamma = 0.05                              # marginal utility of civic duty (paying bill on time)
m = c(400,25)                             # quarterly budget (i.e., have states with bill higher than income)
Theta = as.matrix(c(theta,alpha,gamma))
beta = 1/(1 + (1.1)^(1/4)-1)              # quarterly discount factor for annual interest rate of 5%
# (3) Stochastic Income
sigma2_m = 20                             # variance in income
GH$m = NULL
for(hh in 1:2){
  GH$m[[hh]] = m[hh] + GH$x*sqrt(2)*sqrt(sigma2_m)   # transformed GH nodes to fit income distribution: N(m,sigma2_m)
}
(1/sqrt(pi))*t(GH$m[[1]])%*%GH$w
(1/sqrt(pi))*t(GH$m[[2]])%*%GH$w
(1/sqrt(pi))*t((GH$m[[1]]-m[[1]])^2)%*%GH$w
(1/sqrt(pi))*t((GH$m[[2]]-m[[2]])^2)%*%GH$w



#---------+---------+---------+---------+---------+---------+
# Parallelization
#---------+---------+---------+---------+---------+---------+
library(foreach)
parallel::detectCores()
n.cores <- parallel::detectCores() - 1
#create the cluster
my.cluster <- parallel::makeCluster(
  n.cores, 
  type = "PSOCK"
)
#register it to be used by %dopar%
doParallel::registerDoParallel(cl = my.cluster)



#---------+---------+---------+---------+---------+---------+
# Solve Consumer DP for optimal payment policy 
#   (stochastic income)
#---------+---------+---------+---------+---------+---------+
# Compute Value Function
outV = output = NULL
for(hh in 1:2){
  outV[[hh]] = DPsolve_stochastic(beta=beta,mdraw=GH$m[[hh]],weights=as.matrix(GH$w),Theta=Theta,Pricing=Pricing,outCheby=outCheby,code_dir=code_dir)
  output[[hh]] = cbind(outCheby$grid,apply(outCheby$grid,1,sum),matrix(outV[[hh]]$Astar,ncol=2),outV[[hh]]$V)
}
save(outV,output,m,outCheby,sigma2_m,GH,beta,Theta,Pricing,file=paste(output_dir,"DPout_stochastic.Rdata",sep=""))



#---------+---------+---------+---------+---------+---------+
# Simulate periods 1 to T for stochastic income model
#---------+---------+---------+---------+---------+---------+
# initialize with zero debt
W=E=D=B=M = matrix(0,T,2)
#M = M + m[hh] + matrix(rnorm(T*2)*sqrt(sigma2_m),ncol=2)
for(hh in 1:2){
  for(tt in 1:T){
    state = matrix(c(D[tt,hh],B[tt,hh]),nrow=1)
    # Water Consumption and Expenditure
    Emax = min(sum(state),m)
    errAstatus= 1
    out = mkpolicy(x0=matrix(c(0,Emax),ncol=2),beta=beta,m=m[hh],Theta=Theta,Pricing=Pricing,Chebygrid=outCheby$grid,Lambda=outV[[hh]]$Lambda,state=state,Emax=Emax,errAstatus=errAstatus,maxD=outCheby$maxD,maxB=outCheby$maxB,G=outCheby$G,K=outCheby$K,bound=outCheby$bound)
    W[tt,hh] = out[1]
    E[tt,hh] = out[2]
    if(tt<T){
      B[tt+1,hh] = W[tt,hh]*p + f
      D[tt+1,hh] = sum(state)-E[tt,hh] + apply( cbind(B[tt,hh]-E[tt,hh],0), 1,max)*delta + (D[tt,hh]-(E[tt,hh]-B[tt,hh])>0)*Delta
    }
  }
}

cbind(D[,hh],B[,hh],(D+B)[,hh],W[,hh],E[,hh])
