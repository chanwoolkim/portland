
mdraw = GH$m[[1]]
weights = as.matrix(GH$w)


R = length(mdraw)                 # length integration grid
# (1) Obtain initial Guesses for optimal policy optimized under myopic model (beta=0)
Lambda0 = matrix(0,nrow(outCheby$Proj),R)
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
V = matrix(0,outCheby$pts*R,1)
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
  nonC = which(abs(errV)>1.e-6)
  print(paste("largest error in Bellman update: ",round(max(abs(errV)),8)))
  print(paste("# non-converged: ", length(nonC), " out of ", (outCheby$pts*R), " (", length(nonC)/outCheby$pts/R*100, "%)",sep=""))
  print(paste("Completed Policy iteration #",as.character(iter1)))
}


# (3) Solve Full DP using policy iteration (method of successive approximations)
while( max(abs(errA))>1.e-6 ){
  # (3a) Update Policy
  for(rr in R){
    index = ((rr-1)*outCheby$pts+1):(rr*outCheby$pts)
    keepindex = which(apply(abs(matrix(errA[index,],ncol=2))>1.e-6,1,sum)>0)
    if (length(keepindex)>0){
      for(ii in keepindex){
        Emax = min(apply(outCheby$grid,1,sum)[ii],mdraw[rr])
#        out = fmincon(x0=c(503,Emax),fn=Value_stochastic,beta=beta,m=mdraw[rr],weights=weights,Theta=Theta,Pricing=Pricing,
#                      Chebygrid=outCheby$grid,Lambda=Lambda,state=matrix(outCheby$grid[ii,],nrow=1),
#                      maxiter = 20000,tol=1.e-6,lb = c(0,0),ub=c(Inf,Emax))
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
    outtrans[[rr]] = statetrans(A=Astar0[index,],Pricing=Pricing,Chebygrid=outCheby$grid,state=outCheby$grid)
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
  if(round(iter/10)==iter/10){
    nonC = sum(abs(errA)>1.e-6)
    print(paste("largest error in policy update: ",round(max(abs(errA)),8)))
    print(paste("# non-converged: ", nonC, " out of ", outCheby$pts, " (", nonC/outCheby$pts*100, "%)",sep=""))
    print(paste("Completed Policy iteration #",as.character(iter)))
  }
}

  
  
