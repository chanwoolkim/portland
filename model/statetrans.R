#=========================================================================#
# State transitions
#
# JP Dube, Oct. 6, 2024
#=========================================================================#

statetrans = function(A,Pricing,Chebygrid,state,maxD,maxB,G,K,bound){
  Sdim = ncol(Chebygrid)
  # Extract Pricing and Fines
  p = Pricing[1]; f = Pricing[2]; dbar = Pricing[3]; delta = Pricing[4]; Delta = Pricing[5]
  # extract states
  Ns = nrow(state)
  D = state[,1]
  B = state[,2]
  O = D+B
  # extract policies
  W = as.matrix(A[1:Ns,1])
  W[D>dbar] = 0                     # water shut-off kicks in
  E = as.matrix(A[1:Ns,2])

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
  return(list(W=W,E=E,O=O,B=B,S1=S1,state1=state1))
}
