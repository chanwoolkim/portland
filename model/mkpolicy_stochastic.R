#=========================================================================#
# call to optimal policy computation for stochastic problem
#
# JP Dube, Oct. 6, 2024
#=========================================================================#

mkpolicy_stochastic = function(x0,beta,m,weights,Theta,Pricing,Chebygrid,Lambda,state,Emax,errAstatus,maxD,maxB,G,K,bound) {

  out = try(fmincon(x0=x0,fn=Value_stochastic,beta=beta,m=m,weights=weights,Theta=Theta,Pricing=Pricing,
                Chebygrid=Chebygrid,Lambda=Lambda,state=state,maxD=maxD,maxB=maxB,G=G,K=K,bound=bound,
                maxiter = 20000,tol=1.e-6,lb = c(0,0),ub=c(Inf,Emax)))
  if(inherits(out, "try-error")){
    out = try(fmincon(x0=matrix(c(503,Emax),ncol=2),fn=Value_stochastic,beta=beta,m=m,weights=weights,Theta=Theta,Pricing=Pricing,
                      Chebygrid=Chebygrid,Lambda=Lambda,state=state,maxD=maxD,maxB=maxB,G=G,K=K,bound=bound,
                      maxiter = 20000,tol=1.e-6,lb = c(0,0),ub=c(Inf,Emax)))
  } else if(out$convergence!=0) {
    out = try(fmincon(x0=matrix(c(503,Emax),ncol=2),fn=Value_stochastic,beta=beta,m=m,weights=weights,Theta=Theta,Pricing=Pricing,
                  Chebygrid=Chebygrid,Lambda=Lambda,state=state,maxD=maxD,maxB=maxB,G=G,K=K,bound=bound,
                  maxiter = 20000,tol=1.e-6,lb = c(0,0),ub=c(Inf,Emax)))
  }
  if(inherits(out, "try-error")){
    out = try(fmincon(x0=matrix(c(0,Emax),ncol=2),fn=Value_stochastic,beta=beta,m=m,weights=weights,Theta=Theta,Pricing=Pricing,
                      Chebygrid=Chebygrid,Lambda=Lambda,state=state,maxD=maxD,maxB=maxB,G=G,K=K,bound=bound,
                      maxiter = 20000,tol=1.e-6,lb = c(0,0),ub=c(Inf,Emax)))
  } else if(out$convergence!=0) {
    out = try(fmincon(x0=matrix(c(0,Emax),ncol=2),fn=Value_stochastic,beta=beta,m=m,weights=weights,Theta=Theta,Pricing=Pricing,
                      Chebygrid=Chebygrid,Lambda=Lambda,state=state,maxD=maxD,maxB=maxB,G=G,K=K,bound=bound,
                      maxiter = 20000,tol=1.e-6,lb = c(0,0),ub=c(Inf,Emax)))
  }
  iter=0
  while(inherits(out, "try-error") & iter<=50){
    out = try(fmincon(x0=matrix(c(runif(1)*500,Emax),ncol=2),fn=Value_stochastic,beta=beta,m=m,weights=weights,Theta=Theta,Pricing=Pricing,
                      Chebygrid=Chebygrid,Lambda=Lambda,state=state,maxD=maxD,maxB=maxB,G=G,K=K,bound=bound,
                      maxiter = 20000,tol=1.e-6,lb = c(0,0),ub=c(Inf,Emax)))
    iter = iter+1
  }
  if(!inherits(out, "try-error") & errAstatus<1.e-4){
    while(out$convergence!=0 ) {
      out = try(fmincon(x0=matrix(c(runif(1)*500,Emax),ncol=2),fn=Value_stochastic,beta=beta,m=m,weights=weights,Theta=Theta,Pricing=Pricing,
                        Chebygrid=Chebygrid,Lambda=Lambda,state=state,maxD=maxD,maxB=maxB,G=G,K=K,bound=bound,
                        maxiter = 20000,tol=1.e-6,lb = c(0,0),ub=c(Inf,Emax)))
      while(inherits(out, "try-error")){
        out = try(fmincon(x0=matrix(c(runif(1)*500,Emax),ncol=2),fn=Value_stochastic,beta=beta,m=m,weights=weights,Theta=Theta,Pricing=Pricing,
                          Chebygrid=Chebygrid,Lambda=Lambda,state=state,maxD=maxD,maxB=maxB,G=G,K=K,bound=bound,
                          maxiter = 20000,tol=1.e-6,lb = c(0,0),ub=c(Inf,Emax)))
      }
    }
  }
  if(inherits(out, "try-error")) {
    par = matrix(c(-999,-999),ncol=2)
    out = list(par=par)
  }
  return(out=matrix(out$par,ncol=2))
}
