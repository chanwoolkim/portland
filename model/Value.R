#=========================================================================#
# Compute Value at given set of states, future value and policy
#
# JP Dube, Oct. 6, 2024
#=========================================================================#

Value = function(A,beta,m,Theta,Pricing,Chebygrid,Lambda,state,maxD,maxB,G,K,bound){
  outtrans = statetrans(A=A,Pricing=Pricing,Chebygrid=Chebygrid,state=state,maxD=maxD,maxB=maxB,G=G,K=K,bound=bound)
  # Approximate Future Value function  
  V = outtrans$S1%*%Lambda
  # Compute current Value in specific state for given policies
  V1 =  waterutility(outtrans$W,outtrans$E,outtrans$O,m,Theta) + beta*V
  return(-V1)
}
