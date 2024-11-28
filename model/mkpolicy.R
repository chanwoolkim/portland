#=========================================================================#
# call to optimal policy computation
#
# JP Dube, Oct. 6, 2024
#=========================================================================#

mkpolicy = function(x0,beta,m,Theta,Pricing,Chebygrid,Lambda,state,Emax,errAstatus,maxD,maxB,G,K,bound) {
  out = fmincon(x0=x0,fn=Value,beta=beta,m=m,Theta=Theta,Pricing=Pricing,
                Chebygrid=Chebygrid,Lambda=Lambda,state=state,maxD=maxD,maxB=maxB,G=G,K=K,bound=bound,
                maxiter = 20000,tol=1.e-6,lb = c(0,0),ub=c(Inf,Emax))
  if(out$convergence!=0 & errAstatus<1.e-5) {
    out = fmincon(x0=c(503,Emax),fn=Value,beta=beta,m=m,Theta=Theta,Pricing=Pricing,
                  Chebygrid=Chebygrid,Lambda=Lambda,state=state,maxD=maxD,maxB=maxB,G=G,K=K,bound=bound,
                  maxiter = 20000,tol=1.e-6,lb = c(0,0),ub=c(Inf,Emax))
  }
  while(out$convergence!=0) {
    out = fmincon(x0=c(runif(1),Emax),fn=Value,beta=beta,m=m,Theta=Theta,Pricing=Pricing,
                  Chebygrid=Chebygrid,Lambda=Lambda,state=state,maxD=maxD,maxB=maxB,G=G,K=K,bound=bound,
                  maxiter = 20000,tol=1.e-6,lb = c(0,0),ub=c(Inf,Emax))
  }
  return(out=matrix(out$par,ncol=2))
}
