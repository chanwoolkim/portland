#=========================================================================#
# Water Utility Function
# Arguments:
#   * W = Water Consumption
#   * E = expenditure on bilk
#   * O = total bill owed
#   * D = unpaid debt
#   * m = budget
#   * Theta = tastes
#
# JP Dube, September 23, 2024
#=========================================================================#

waterutility = function(W,E,O,m,Theta) {
  # Extract Preferences
  theta = Theta[1:3]; alpha = Theta[4]; gamma = Theta[5]
  #  u = theta[1]*W + theta[2]*W^2 + theta[3]*(W==0) + alpha*(m-E) - gamma*(E-O)^2
  #  u = as.matrix( theta[1]*W + theta[2]*W^2 + theta[3]*(W<=0) + alpha*asinh(m-E) - gamma*asinh((O-E)^2) )
  u = as.matrix( theta[1]*W + theta[2]*W^2 - theta[3]*(W<=0) + alpha*asinh(m-E) - gamma*asinh((O-E)^2) )
  return(u)
}
