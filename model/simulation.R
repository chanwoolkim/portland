#=========================================================================#
# Simulation Study
#  - random water needs
#  - stationary payment policy
#
# JP Dube, Sept. 10, 2024
#=========================================================================#

#---------+---------+---------+---------+---------+---------+
# PRELIMINARIES
#---------+---------+---------+---------+---------+---------+

if (Sys.info()[4]=="JDUBE-LT3"){
  wd = "C:/Users/jdube/Box/PWB"
  } else {
  wd <- paste0(dirname(rstudioapi::getSourceEditorContext()$path), "/../..")}
setwd(wd)
wd <- getwd()
code_dir <- paste0(wd, "/code/model")
data_dir <- paste0(wd, "/data/raw/servus")
auxiliary_data_dir <- paste0(wd, "/data/auxiliary")
working_data_dir <- paste0(wd, "/data/analysis")
output_dir <- paste0(code_dir, "/output")


#---------+---------+---------+---------+---------+---------+
# FUNCTIONS
#---------+---------+---------+---------+---------+---------+



#---------+---------+---------+---------+---------+---------+
# Initialize
#---------+---------+---------+---------+---------+---------+
set.seed(42)
N = 500
T = 50
f = 10                                    # fixed price of water
p = 0.05                                  # linear price of water
dbar = 100
delta = 0.05
Delta = 15
alpha = 1                                 # marginal utility of income
theta = 1                                 # marginal utility of water consumption
gamma = 3                                 # marginal utility of civic duty (paying bill on time)
beta = 1/(1 + (1.05)^(1/4)-1)             # quarterly discount factor for annual interest rate of 5%
m = 500                                   # quarterly budget
Theta = as.matrix(c(theta,alpha,gamma,delta,Delta,dbar))
w = B = O = E = matrix(0,T,N)
#omega = matrix(runif(T*N),rows=T)*100     #random water needs
#epsilon = matrix(runif(T*N),rows=T)       #random liquidity

# create state grid
Ogrid = as.matrix(seq(from=0,by=15,to=500))
Dgrid = as.matrix(seq(from=0,by=12,to=400))
Bgrid = as.matrix(seq(from=0,by=5,to=100))
stategridtemp = cbind( kronecker(Ogrid,matrix(1,length(Dgrid))) , as.matrix(rep(Dgrid,length(Ogrid)))  )
stategrid = cbind( kronecker(stategridtemp,matrix(1,nrow(Bgrid),1)) , as.matrix(rep(Bgrid,nrow(stategridtemp)))  )
#stategrid = cbind( kronecker(Ogrid,matrix(1,length(Bgrid),1)) , repmat(Bgrid,length(Ogrid),1)  )
Ns = nrow(stategrid)                     # number states


#---------+---------+---------+---------+---------+---------+
# Solve Consumer DP for optimal payment policy
#---------+---------+---------+---------+---------+---------+
library(mpoly)
G = 6;            # Number Chebyshev nodes
K = 6;            # Order of Chebyshev approximation
bounds = cbind(as.matrix(range(Ogrid)),as.matrix(range(Dgrid)),as.matrix(range(Bgrid)))
z = as.matrix(-cos( (2*(1:G)-1)/(2*G)*pi ))
xk = matrix(rep(t(z+1),3),ncol=3)*t(matrix(rep((bounds[2,]-bounds[1,]) /2,G),nrow=3))
Chebygridtemp = cbind( kronecker(xk[,1],matrix(1,G)) , as.matrix(rep(xk[,2],G))  )
Chebygrid = cbind( kronecker(Chebygridtemp,matrix(1,G,1)) , as.matrix(rep(xk[,3],G^2))  )

rhoz = matrix(1,G,K);
rhoz[,2] = z;
for( ii in 3:K){
rhoz[,ii]  = 2*z*rhoz[,ii-1]   - rhoz[,ii-2]
}
Chebpolytemp = kronecker(kronecker(rhoz,matrix(1,G)),matrix(1,1,K))*kronecker(matrix(1,1,K),kronecker(matrix(1,G),rhoz))
Chebpoly = kronecker(kronecker(Chebpolytemp,matrix(1,G)),matrix(1,1,K))*kronecker(matrix(1,1,K),kronecker(matrix(1,G^2,K),rhoz))
ProjChebpoly = solve(t(Chebpoly)%*%Chebpoly)*t(Chebpoly)
Chebygridpts = G^3;


Value = function(A,Theta,beta,Ns,stategrid,m,V,p,f,dbar){
  theta = Theta[1]
  alpha = Theta[2]
  gamma = Theta[3]
  delta = Theta[4]
  Delta = Theta[5]
  dbar = Theta[6]
  W = as.matrix(A[1:Ns])
  E = as.matrix(A[(Ns+1):(2*Ns)])
  W[stategrid[,2]>dbar] = 1.e-323           # water shut-off kicks in
  E[E>m] = m
  
  # future state via law of motion
  B1 = W*p + f
  O1 = Chebygrid[,1]-E + max(Chebygrid[,3]-E,0)*delta + (Chebygrid[,1]-(E-Chebygrid[,3])>0)*Delta + B1
  D1 = O1-B1
  O1[O1>max(Chebygrid[,1])] = max(Chebygrid[,1])
  D1[D1>max(Chebygrid[,2])] = max(Chebygrid[,2])
  B1[B1>max(Chebygrid[,3])] = max(Chebygrid[,3])
  state = cbind(O1,D1,B1)
  # Assign future state to nearest grid point
  #O11 = round(O1/15)*15
  #B11 = round(B1/5)*5
  #D11 = round(D1/12)*12
  #S1 = which(kronecker(O11,matrix(1,1,Ns))-kronecker(matrix(1,Ns,1),t(stategrid[,1]))==0 & kronecker(D11,matrix(1,1,Ns))-kronecker(matrix(1,Ns,1),t(stategrid[,2]))==0 & kronecker(B11,matrix(1,1,Ns))-kronecker(matrix(1,Ns,1),t(stategrid[,3]))==0)
  #S1 = ismember(cbind(O11,D11,B11),stategrid,rows=T,indices=T)
  #S1 = (O11/15)*nrow(Dgrid)*nrow(Bgrid) + (D11/12)*nrow(Bgrid) + (B11/5)

  # Chebyshev Approximation
  #(1) normalize the states
  rhox = list(matrix(1,Chebygridpts,K),matrix(1,Chebygridpts,K),matrix(1,Chebygridpts,K))
  for(ss in 1:3){
    rhox[[ss]][,2] = 2*(state[,ss]-bounds[1,ss])/(bound[2,ss]-bounds[1,ss])-1
  }
  for(ii in 3:Chebyorder){
    for(ss in 1:3){
      rhox[[ss]][,ii] = 2*rhox[[ss]][,ii]*rhox[ss][,ii-1] - rhox[[ss]][,ii-2]
    }
  }
  Xfuturetemp = cbind( kronecker(rhox[[1]],matrix(1,G)) , as.matrix(rep(rhox[[2]],G))  )
  Xfuture = cbind( kronecker(Xfuturetemp,matrix(1,G,1)) , as.matrix(rep(rhox[[3]],G^2))  )
  
  V = Xfuture%*%Lambda
  

  
  # Compute current value at given policies in A
  #V1 = theta*log(W) + alpha*(m-E) + gamma*(E>=stategrid[,1]) + beta*V[S1]
  V1 = (W^theta)*((m-E)^alpha)*exp(gamma*(E>=stategrid[,1])) + beta*V[S1]
  print(t(V1)%*%V1)
  return(t(V1)%*%V1)
}
Wstar0 = matrix(runif(Ns),ncol=1)
Estar0 = Vstar = matrix(0,Ns,1)
Astar = A0 = rbind(Wstar0,Estar0)
errV = errA = 1
out = optim(par=Astar,fn=Value,Theta=Theta,beta=beta,Ns=Ns,stategrid=stategrid,m=m,p=p,f=f,dbar=dbar,V=Vstar)


DPsolve = function(Theta,beta,Ns,stategrid,m){
  Wstar0 = matrix(runif(Ns),ncol=1)
  Estar0 = Vstar = matrix(0,Ns,1)
  Astar = A0 = rbind(Wstar0,Estar0)
  errV = errA = 1
  
  # Bellman iteration
  Astar = A0
  while( errV>1.e-6 & errA>1.e-6){
    #out = optim(par=matrix(0,2*Ns,1),fn=Value,Theta=Theta,beta=beta,Ns=Ns,stategrid=stategrid,m=m,p=p,f=f,dbar=dbar)
    out = optim(par=Astar,fn=Value,Theta=Theta,beta=beta,Ns=Ns,stategrid=stategrid,m=m,p=p,f=f,dbar=dbar,V=Vstar)
    errA = abs(Astar-out$x)
    errV = abs(V-out$f)
    Astar = out$x
    Vstar = out$f
  }
  return(Vstar=Vstar,Astar=Astar)
}



#---------+---------+---------+---------+---------+---------+
# Simulate periods 1 to T
#---------+---------+---------+---------+---------+---------+
while(t in 1:T){
  # Water Consumption
  w[t,] = omega[t,]
}