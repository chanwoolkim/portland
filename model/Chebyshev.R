#=========================================================================#
# Function to create Chebyshev Polynomials to Approximate a Function
#  in up to three dimensions.
#
# Arguments:
#   * G: # nodes
#   * K: Order of approximation (K>3)
#   * bound: Upper and lower bounds for each dimension (cols=dim)
#
# JP Dube, Sept. 2024
#=========================================================================#

Chebyshev = function(G,K,bound){
  if(K<3) {stop("K must be at least 3")}
  Sdim = ncol(bound)
  
  #---------+---------+---------+---------+---------+---------+
  # (1) Construct the grid
  #---------+---------+---------+---------+---------+---------+
  z = as.matrix(-cos( (2*(1:G)-1)/(2*G)*pi ))                                                     # nodes
  xk = matrix(rep(t(z+1),Sdim),ncol=Sdim)*t(matrix(rep((bound[2,]-bound[1,]) /2,G),nrow=Sdim))    # transform nodes into state space dimensions
  if (Sdim==1){
    Chebygrid = as.matrix(xk[,1])
  } else if (Sdim==2) {
    Chebygrid = cbind( kronecker(xk[,1],matrix(1,G)) , as.matrix(rep(xk[,2],G))  )
  } else {
    Chebygridtemp = cbind( kronecker(xk[,1],matrix(1,G)) , as.matrix(rep(xk[,2],G))  )
    Chebygrid = cbind( kronecker(Chebygridtemp,matrix(1,G,1)) , as.matrix(rep(xk[,3],G^2))  )
  }
  
  #---------+---------+---------+---------+---------+---------+
  # (2) Construct the polynomials
  #---------+---------+---------+---------+---------+---------+
  rhoz = matrix(1,G,K)
  rhoz[,2] = z
  for( ii in 3:K){
    rhoz[,ii]  = 2*z*rhoz[,ii-1] - rhoz[,ii-2]
  }
  if (Sdim==1){
    Chebpoly = as.matrix(rhoz)
  } else if (Sdim==2) {
    Chebpoly = kronecker(kronecker(rhoz,matrix(1,G)),matrix(1,1,K))*kronecker(matrix(1,1,K),kronecker(matrix(1,G),rhoz))
  } else {
    Chebpolytemp = kronecker(kronecker(rhoz,matrix(1,G)),matrix(1,1,K))*kronecker(matrix(1,1,K),kronecker(matrix(1,G),rhoz))
    Chebpoly = kronecker(kronecker(Chebpolytemp,matrix(1,G)),matrix(1,1,K))*kronecker(matrix(1,1,K),kronecker(matrix(1,G^2,K),rhoz))
  }
  
  #---------+---------+---------+---------+---------+---------+
  # (3) Construct Projection Matrix
  #---------+---------+---------+---------+---------+---------+
  ProjChebpoly = solve(crossprod(Chebpoly))%*%t(Chebpoly)
  Chebygridpts = G^ncol(Chebygrid)
  
  return(list(grid=Chebygrid,poly=Chebpoly,Proj=ProjChebpoly,pts=Chebygridpts,G=G,K=K,bound=bound,maxD=bound[2,1],maxB=bound[2,2]))
}
