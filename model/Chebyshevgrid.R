#=========================================================================#
# Function to transform set of points for Chebyshev Approximation
#
# Arguments:
#   * Xgrid: Set of points
#   * K: Order of approximation (K>3)
#   * bound: Upper and lower bounds for each dimension (cols=dim)
#
# JP Dube, Sept. 2024
#=========================================================================#

Chebyshevgrid = function(Xgrid,K,bound){
  if(K<3) {stop("K must be at least 3")}
  Sdim = ncol(Xgrid)
  rhox = NULL
  for(ss in 1:Sdim){rhox[[ss]]=matrix(1,nrow(Xgrid),K)}
  for(ss in 1:Sdim){
    rhox[[ss]][,2] = 2*(Xgrid[,ss]-bound[1,ss])/(bound[2,ss]-bound[1,ss])-1
  }
  # 3rd and higher polynomials = use recursion from Judd (1992): T(x)_{n+1} = 2*x*T(x)_{n} - t(x)_{n-1}
  for(rr in 3:K){
    for(ss in 1:Sdim){
      rhox[[ss]][,rr] = 2*rhox[[ss]][,2]*rhox[[ss]][,rr-1] - rhox[[ss]][,rr-2]
    }
  }
  # Use tensor product to create Chebyshev grid for specific states
  if(Sdim==1){
    XgridCheby = rhox[[1]]
  } else if (Sdim==2){
    XgridCheby = kronecker(rhox[[1]],matrix(1,1,K))*kronecker(matrix(1,1,K),rhox[[2]])
  } else{
    Xgridtemp = kronecker(rhox[[1]],matrix(1,1,K))*kronecker(matrix(1,1,K),rhox[[2]])
    XgridCheby = kronecker(Xgridtemp,matrix(1,1,K))*kronecker(matrix(1,1,K^2),rhox[[3]])
  }
  
  return(XgridCheby)
}