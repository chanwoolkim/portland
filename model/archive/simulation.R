#=========================================================================#
# Simulation Study
#  - random water needs
#  - stationary payment policy
#  - STATE = (Owed (O), Debt (D), Water Bill Component (B))
#  - POLICY = (Water Consumption (W), Expenditure on Bill (E))
#
# JP Dube, Sept. 10, 2024
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


#---------+---------+---------+---------+---------+---------+
# LOCAL FUNCTIONS
#---------+---------+---------+---------+---------+---------+
statetrans = function(A,Pricing,Chebygrid,state){
  Sdim = ncol(Chebygrid)
  # Extract Pricing and Fines
  p = Pricing[1]; f = Pricing[2]; dbar = Pricing[3]; delta = Pricing[4]; Delta = Pricing[5]
  # extract states
  Ns = nrow(state)
  D = state[,1]
  B = state[,2]
  O = D+B
  # extract policies
  W = as.matrix(A[1:Ns])
  W[D>dbar] = 0                     # water shut-off kicks in
  E = as.matrix(A[(Ns+1):(2*Ns)])
  
  # future state via law of motion
  D1 = O-E + apply( cbind(B-E,0), 1,max)*delta + (D-(E-B)>0)*Delta
  B1 = W*p + f
  B1[D>dbar] = 0
  state1 = cbind(D1,B1)
  state1[state1[,1]>maxD,1] = maxD
  state1[state1[,2]>maxB,2] = maxB
  
  # Chebyshev Approximation of Future Value Function at Specific States
  # (1) 1st polynomial equals one
  rhox = list(matrix(1,Ns,K),matrix(1,Ns,K))
  # (2) 2nd polynomial = transform the states back onto the Chebyshev support (between zero and one)
  for(ss in 1:Sdim){
    rhox[[ss]][,2] = 2*(state1[,ss]-bound[1,ss])/(bound[2,ss]-bound[1,ss])-1
  }
  # (3) 3rd and higher polynomials = use recursion from Judd (1992): T(x)_{n+1} = 2*x*T(x)_{n} - t(x)_{n-1}
  for(nn in 3:K){
    for(ss in 1:Sdim){
      rhox[[ss]][,nn] = 2*rhox[[ss]][,2]*rhox[[ss]][,nn-1] - rhox[[ss]][,nn-2]
    }
  }
  # Use tensor product to create Chebyshev grid for specific states
  S1 = kronecker(rhox[[1]],matrix(1,1,K))*kronecker(matrix(1,1,K),rhox[[2]])
  return(list(W=W,E=E,O=O,B=B,S1=S1))
}


# Compute Value at given set of states, future value and policy
Value = function(A,beta,m,Theta,Pricing,Chebygrid,Lambda,state){
  outtrans = statetrans(A=A,Pricing=Pricing,Chebygrid=Chebygrid,state=state)
  # Approximate Future Value function  
  V = outtrans$S1%*%Lambda
  # Compute current Value in specific state for given policies
  V1 =  waterutility(outtrans$W,outtrans$E,outtrans$O,m,Theta) + beta*V
  return(-V1)
}


# Modified Policy Iteration to compute value function and optimal policies
#  DETERMINISTIC INCOME
DPsolve = function(beta,m,Theta,Pricing,outCheby){
  start = Sys.time()
  # (1) Obtain initial Guesses for optimal policy optimized under myopic model (beta=0)
  Lambda0 = matrix(0,nrow(outCheby$Proj),1)
  Astar0 = matrix(0,outCheby$pts,2)
  for(ii in 1:outCheby$pts){
    Emax = min(apply(outCheby$grid,1,sum)[ii],m)
    out = fmincon(x0=c(0,Emax),fn=Value,beta=0,m=m,Theta=Theta,Pricing=Pricing,
                  Chebygrid=outCheby$grid,Lambda=Lambda0,state=matrix(outCheby$grid[ii,],nrow=1),
                  maxiter = 20000,tol=1.e-6,lb = c(0,0),ub=c(Inf,Emax))
    #    if(out$convergence!=0) {
#          out = fmincon(x0=c(0,Emax),fn=Value,beta=0,m=m,Theta=Theta,Pricing=Pricing,
#                  Chebygrid=outCheby$grid,Lambda=Lambda,state=matrix(outCheby$grid[ii,],nrow=1),
#                  maxiter = 20000,tol=1.e-6,lb = c(0,0),ub=c(Inf,Emax))
#    }
    Astar0[ii,1] = out$par[1]
    Astar0[ii,2] = out$par[2]
  }
  Astar = Astar1 = Astar0

  # (2) Contract Bellman equatio for value function when applying myopic policies
  Lambda = Lambda1 = Lambda0
  iter1 = 0
  outtrans = statetrans(A=Astar0,Pricing=Pricing,Chebygrid=outCheby$grid,state=outCheby$grid)
  util = waterutility(outtrans$W,outtrans$E,outtrans$O,m=m,Theta)
  V = matrix(0,outCheby$pts,1)
  errV = matrix(1,outCheby$pts,1)
  while(max(abs(errV))>1.e-6){
      # Value Function Contraction
      Vdraw = outtrans$S1%*%Lambda
      V1 = util + beta*Vdraw
      #V1 = -Value(A=Astar1,beta=beta,m=m,Theta=Theta,Pricing=Pricing,
      #                  Chebygrid=outCheby$grid,Lambda=Lambda,state=outCheby$grid)
      Lambda1 = outCheby$Proj%*%V1
      errV = V-V1
    V = V1
    Lambda = Lambda1
    iter1 = iter1+1
  }
  
  # (3) Solve Full DP using policy iteration (method of successive approximations)
  iter = 0
  errA = matrix(1,outCheby$pts,2)
  while( max(abs(errA))>1.e-6 ){
    # (3a) Update Policy
    index = which(apply(abs(matrix(errA,ncol=2))>1.e-6,1,sum)>0)
    for(ii in index){
      Emax = min(apply(outCheby$grid,1,sum)[ii],m)
      out = fmincon(x0=Astar[ii,],fn=Value,beta=beta,m=m,Theta=Theta,Pricing=Pricing,
                    Chebygrid=outCheby$grid,Lambda=Lambda,state=matrix(outCheby$grid[ii,],nrow=1),
                    maxiter = 20000,tol=1.e-6,lb = c(0,0),ub=c(Inf,Emax))
      if(out$convergence!=0 & max(abs(errA[c(ii,outCheby$pts+ii)]))<1.e-5) {
        out = fmincon(x0=c(503,Emax),fn=Value,beta=beta,m=m,Theta=Theta,Pricing=Pricing,
                      Chebygrid=outCheby$grid,Lambda=Lambda,state=matrix(outCheby$grid[ii,],nrow=1),
                      maxiter = 20000,tol=1.e-6,lb = c(0,0),ub=c(Inf,Emax))
#        out = optim(par=c(100,Astar[outCheby$pts+ii]),fn=Value,beta=beta,m=m,Theta=Theta,Pricing=Pricing,
#                    Chebygrid=outCheby$grid,Lambda=Lambda,state=matrix(outCheby$grid[ii,],nrow=1),
#                    control = list(maxit = 20000,factr=1.e6),method ="L-BFGS-B",lower = c(0,0),upper=c(Inf,Emax))
      }
      Astar1[ii,1] = out$par[1]
      Astar1[ii,2] = out$par[2]
    }
    errA = matrix(Astar-Astar1,ncol=2)
    Astar = Astar1

    # (3b) Bellman contraction-mapping
    errV = matrix(1,outCheby$pts,1)
    iter1 = 0
    outtrans = statetrans(A=Astar1,Pricing=Pricing,Chebygrid=outCheby$grid,state=outCheby$grid)
    util = waterutility(outtrans$W,outtrans$E,outtrans$O,m=m,Theta)
    while(max(abs(errV))>1.e-6){
      # Value Function Contraction
      Vdraw = outtrans$S1[index,]%*%Lambda
      V1[index] = util[index] + beta*Vdraw
      #V1[index] = -Value(A=Astar1[c(index,index+outCheby$pts),],beta=beta,m=m,Theta=Theta,Pricing=Pricing,
      #                  Chebygrid=outCheby$grid,Lambda=Lambda,state=matrix(outCheby$grid[index,],ncol=ncol(outCheby$grid)))
      Lambda = outCheby$Proj%*%V1
      errV = V-V1
      V = V1
      iter1 = iter1+1
    }
    iter = iter+1
    if(round(iter/50)==iter/50){
      nonC = sum( apply(abs(errA)>1.e-6,1,sum)>0 )
      print(paste("largest error in policy update: ",round(max(abs(errA)),8)))
      print(paste("# non-converged: ", nonC, " out of ", outCheby$pts, " (", nonC/outCheby$pts*100, "%)",sep=""))
      print(paste("Completed Policy iteration #",as.character(iter)))
      print("Elapsed time: ")
      print(Sys.time()-start)
    }
  }
  return(list(Lambda=Lambda,V=V1,Astar=Astar,iter=iter))
}


# Compute Value at given set of states, future value and policy
Value_stochastic = function(A,beta,m,weights,Theta,Pricing,Chebygrid,Lambda,state){
  # Determine Future States
  outtrans = statetrans(A=A,Pricing=Pricing,Chebygrid=Chebygrid,state=state)
  # Approximate Future Value function  
  Vdraw = outtrans$S1%*%Lambda
  # Compute current Value in specific state for given policies
  V1 =  waterutility(outtrans$W,outtrans$E,outtrans$O,m,Theta) + beta*(Vdraw%*%weights)/sqrt(pi)
  return(-V1)
}


# Modified Policy Iteration to compute value function and optimal policies
#  STOCHASTIC INCOME
DPsolve_stochastic = function(beta,mdraw,weights,Theta,Pricing,outCheby){
  start = Sys.time()
  R = length(mdraw)                 # length integration grid
  # (1) Obtain initial Guesses for optimal policy optimized under myopic model (beta=0)
  Lambda0 = matrix(0,nrow(outCheby$Proj),R)
  V = matrix(0,outCheby$pts*R,1)
  Astar0 = matrix(0,outCheby$pts*R,2)
  for(ii in 1:outCheby$pts){
    for(rr in 1:R){
      Emax = min(apply(outCheby$grid,1,sum)[ii],mdraw[rr])
      out = fmincon(x0=c(503,Emax),fn=Value_stochastic,beta=beta,m=mdraw[rr],weights=weights,Theta=Theta,Pricing=Pricing,
                    Chebygrid=outCheby$grid,Lambda=Lambda0,state=matrix(outCheby$grid[ii,],nrow=1),
                    maxiter = 20000,tol=1.e-6,lb = c(0,0),ub=c(Inf,Emax))
      #out = optim(par=c(0,Emax),fn=Value_stochastic,beta=0,m=mdraw[rr],weights=weights,Theta=Theta,Pricing=Pricing,
      #            Chebygrid=outCheby$grid,Lambda=Lambda0,state=matrix(outCheby$grid[ii,],nrow=1),
      #            control = list(maxit = 20000,factr=1.e-6),method ="L-BFGS-B",lower = c(0,0),upper=c(Inf,Emax))
      #if(out$convergence!=0) {STOP}
      index = (rr-1)*outCheby$pts+ii
      Astar0[index,1] = out$par[1]
      Astar0[index,2] = out$par[2]
    }
  }
  Astar = Astar1 = Astar0
  Lambda = Lambda0
  errA = matrix(1,outCheby$pts*R,2)
  errV = matrix(1,outCheby$pts*R,1)
  iter1 = 0
  # (2) Contract Bellman equation for value function when applying myopic policies
  # pre-compute state transitions for given policies
  outtrans=util=NULL
  for(rr in 1:R){
    index = ((rr-1)*outCheby$pts+1):(rr*outCheby$pts)
    outtrans[[rr]] = statetrans(A=Astar0[index,],Pricing=Pricing,Chebygrid=outCheby$grid,state=outCheby$grid)
    util[[rr]] = waterutility(outtrans[[rr]]$W,outtrans[[rr]]$E,outtrans[[rr]]$O,m=mdraw[rr],Theta)
  }
  while(max(abs(errV))>1.e-6){
    V1 = V*0
    Lambda1 = Lambda*0
    for(rr in 1:R){
      index = ((rr-1)*outCheby$pts+1):(rr*outCheby$pts)
      # Value Function Contraction
      Vdraw = outtrans[[rr]]$S1%*%Lambda
      V1[index] = util[[rr]] + beta*(Vdraw%*%weights)/sqrt(pi)
      #  -Value_stochastic(A=matrix(Astar0[index,],ncol=1),beta=beta,m=mdraw[rr],weights=weights,Theta=Theta,Pricing=Pricing,
      #                              Chebygrid=outCheby$grid,Lambda=Lambda,state=outCheby$grid)
      Lambda1[,rr] = outCheby$Proj%*%V1[index]
    }
    errV = V-V1
    V = V1
    Lambda = Lambda1
    iter1 = iter1+1
  }
  
  # (3) Solve Full DP using policy iteration (method of successive approximations)
  iter = 0
  while( max(abs(errA))>1.e-6 ){
    # (3a) Update Policy
    for(rr in R){
      index = ((rr-1)*outCheby$pts+1):(rr*outCheby$pts)
      keepindex = which(apply(abs(matrix(errA[index,],ncol=2))>1.e-6,1,sum)>0)
      if (length(keepindex)>0){
        for(ii in keepindex){
          Emax = min(apply(outCheby$grid,1,sum)[ii],mdraw[rr])
          out = fmincon(x0=Astar[index,][ii,],fn=Value_stochastic,beta=beta,m=mdraw[rr],weights=weights,Theta=Theta,Pricing=Pricing,
                        Chebygrid=outCheby$grid,Lambda=Lambda,state=matrix(outCheby$grid[ii,],nrow=1),
                        maxiter = 20000,tol=1.e-6,lb = c(0,0),ub=c(Inf,Emax))
          #out = optim(par=Astar[index,][ii,],fn=Value_stochastic,beta=beta,m=mdraw[rr],weights=weights,Theta=Theta,Pricing=Pricing,
          #            Chebygrid=outCheby$grid,Lambda=Lambda,state=matrix(outCheby$grid[ii,],nrow=1),
          #            control = list(maxit = 20000,factr=1.e6),method ="L-BFGS-B",lower = c(0,0),upper=c(Inf,Emax))
          if(out$convergence!=0 & max(abs(errA[index,][ii,]))<1.e-5) {
            out = fmincon(x0=Astar[index,][ii,],fn=Value_stochastic,beta=beta,m=mdraw[rr],weights=weights,Theta=Theta,Pricing=Pricing,
                          Chebygrid=outCheby$grid,Lambda=Lambda,state=matrix(outCheby$grid[ii,],nrow=1),
                          maxiter = 20000,tol=1.e-6,lb = c(0,0),ub=c(Inf,Emax))
            #            out = optim(par=c(100,Astar[index,][ii,]),fn=Value_stochastic(),beta=beta,m=mdraw[rr],weights=weights,Theta=Theta,Pricing=Pricing,
#                        Chebygrid=outCheby$grid,Lambda=Lambda,state=matrix(outCheby$grid[ii,],nrow=1),
#                        control = list(maxit = 20000,factr=1.e6),method ="L-BFGS-B",lower = c(0,0),upper=c(Inf,Emax))
            }
          Astar1[index,][ii,1] = out$par[1]
          Astar1[index,][ii,2] = out$par[2]
        }
      }
    }
    
    # (3b) Bellman contraction-mapping
    errV = matrix(1,outCheby$pts*R,1)
    iter1 = 0
    # pre-compute state transitions for given policies
    outtrans=util=NULL
    for(rr in 1:R){
      index = ((rr-1)*outCheby$pts+1):(rr*outCheby$pts)
      outtrans[[rr]] = statetrans(A=Astar1[index,],Pricing=Pricing,Chebygrid=outCheby$grid,state=outCheby$grid)
      util[[rr]] = waterutility(outtrans[[rr]]$W,outtrans[[rr]]$E,outtrans[[rr]]$O,m=mdraw[rr],Theta)
    }
    while(max(abs(errV))>1.e-6){
      for(rr in 1:R){
        index = ((rr-1)*outCheby$pts+1):(rr*outCheby$pts)
        keepindex = which(apply(abs(matrix(errA[index,],ncol=2))>1.e-6,1,sum)>0)
        if (length(keepindex)>0){
          # Value Function Contraction
          Vdraw = outtrans[[rr]]$S1[keepindex,]%*%Lambda
          V1[index][keepindex] = util[[rr]][keepindex] + beta*(Vdraw%*%weights)/sqrt(pi)
          #V1[index][keepindex] = -Value_stochastic(A=Astar1[index,][keepindex,],beta=beta,m=mdraw[rr],weights=weights,Theta=Theta,Pricing=Pricing,
          #                                        Chebygrid=outCheby$grid,Lambda=Lambda,state=matrix(outCheby$grid[keepindex,],ncol=ncol(outCheby$grid)))
          Lambda[,rr] = outCheby$Proj[,keepindex]%*%as.matrix(V1[index][keepindex])
        }
      }
      errV = V-V1
      V = V1
      iter1 = iter1+1
    }
    iter = iter+1
    errA = matrix(Astar-Astar1,ncol=2)
    Astar = Astar1
    if(round(iter/50)==iter/50){
      nonC = sum( apply(abs(errA)>1.e-6,1,sum)>0 )
      print(paste("largest error in policy update: ",round(max(abs(errA)),8)))
      print(paste("# non-converged: ", nonC, " out of ", outCheby$pts, " (", nonC/outCheby$pts*100, "%)",sep=""))
      print(paste("Completed Policy iteration #",as.character(iter)))
      #print(paste("Elapsed time: ",as.character(as.character((Sys.time()-start)))," mins",sep=""))
      print("Elapsed time: ")
      print(Sys.time()-start)
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
# Solve Consumer DP for optimal payment policy 
#   (deterministic income)
#---------+---------+---------+---------+---------+---------+
# Compute Value Functions
outV = output = NULL
for(hh in 1:2){
  outV[[hh]] = DPsolve(beta=beta,m=m[hh],Theta=Theta,Pricing=Pricing,outCheby=outCheby)
  output[[hh]] = cbind(outCheby$grid,apply(outCheby$grid,1,sum),matrix(outV[[hh]]$Astar,ncol=2),outV[[hh]]$V)
}
save(outV,output,m,outCheby,file=paste(output_dir,"DPout_deterministic.Rdata",sep=""))


#---------+---------+---------+---------+---------+---------+
# Solve Consumer DP for optimal payment policy 
#   (stochastic income)
#---------+---------+---------+---------+---------+---------+
# Compute Value Function
outV = output = NULL
for(hh in 1:2){
  outV[[hh]] = DPsolve_stochastic(beta=beta,mdraw=GH$m[[hh]],weights=as.matrix(GH$w),Theta=Theta,Pricing=Pricing,outCheby=outCheby)
  output[[hh]] = cbind(outCheby$grid,apply(outCheby$grid,1,sum),matrix(outV[[hh]]$Astar,ncol=2),outV[[hh]]$V)
}
save(outV,output,m,outCheby,sigma2_m,GH,beta,Theta,Pricing,file=paste(output_dir,"DPout_stochastic.Rdata",sep=""))



#---------+---------+---------+---------+---------+---------+
# Simulate periods 1 to T for stochastic income model
#---------+---------+---------+---------+---------+---------+
# initialize with zero debt
W=E=D=B=M = matrix(0,T,2)
M = M + m + matrix(rnorm(T*2)*sqrt(sigma2_m),ncol=2)
for(tt in 1:T){
  state = matrix(c(D[tt],B[tt]),nrow=1)
  # Water Consumption and Expenditure
  Emax = sum(state)
  out = optim(par=c(503,Emax),fn=Value_stochastic,beta=beta,m=M[tt],weights=as.matrix(GH$w),Theta=Theta,Pricing=Pricing,
              Chebygrid=outCheby$grid,Lambda=outV$Lambda,state=state,
              control = list(maxit = 20000,factr=1.e6),method ="L-BFGS-B",lower = c(0,0),upper=c(Inf,M[tt]))
  if(out$convergence!=0){
  out = fmincon(x0=c(503,Emax),fn=Value_stochastic,beta=beta,m=M[tt],weights=as.matrix(GH$w),Theta=Theta,Pricing=Pricing,
                Chebygrid=outCheby$grid,Lambda=outV$Lambda,state=state,
                maxiter = 20000,tol=1.e6,lb = c(0,0),ub=c(Inf,M[tt]))
  }  
  if (out$convergence!=0){STOP}
  W[tt] = out$par[1]
  E[tt] = out$par[2]
  if(tt<T){
    B[tt+1] = W[tt]*p + f
    D[tt+1] = sum(state)-E[tt] + apply( cbind(B[tt]-E[tt],0), 1,max)*delta + (D[tt]-(E[tt]-B[tt])>0)*Delta
  }
}

cbind(D,B,D+B,W,E)
