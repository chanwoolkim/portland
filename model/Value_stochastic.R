#=========================================================================#
# Compute Value at given set of states, future value and policy
#
# JP Dube, Oct. 6, 2024
#=========================================================================#

Value_stochastic = function(A,beta,m,weights,Theta,Pricing,Chebygrid,Lambda,state,maxD,maxB,G,K,bound){
  # Determine Future States
  outtrans = statetrans(A=A,Pricing=Pricing,Chebygrid=Chebygrid,state=state,maxD=maxD,maxB=maxB,G=G,K=K,bound=bound)
  # Approximate Future Value function  
  Vdraw = outtrans$S1%*%Lambda
  # Compute current Value in specific state for given policies
  V1 =  waterutility(outtrans$W,outtrans$E,outtrans$O,m,Theta) + beta*(Vdraw%*%weights)/sqrt(pi)
  return(-V1)
}
