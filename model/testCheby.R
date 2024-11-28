#=========================================================================#
# Test Chebyshev Approximation
#
# JP Dube, September 24, 2024
#=========================================================================#

#---------+---------+---------+---------+---------+---------+
# PRELIMINARIES
#---------+---------+---------+---------+---------+---------+
# Set working directories
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

# Open utility functions
source(paste(code_dir,"/Chebyshev.R",sep=""))
source(paste(code_dir,"/Chebyshevgrid.R",sep=""))
source(paste(code_dir,"/waterutility.R",sep=""))


#---------+---------+---------+---------+---------+---------+
# Use utility function as test case (fixed Debt Owed and Expenditure i.e., 1dims)
#---------+---------+---------+---------+---------+---------+
# create state grid
maxW = 1000
maxE = 500
maxO = 1000
bound = matrix(0,2,1)
bound[2,] = cbind(maxW)

# Construct grid for Chebyshev approximation
G = 50;                                   # Number Chebyshev nodes
K = 45;                                   # Order of Chebyshev approximation
outCheby = Chebyshev(G,K,bound)

#parameters
theta = c(0.01,-.00001,-10)               # water consumption utility
alpha = 0.01                              # marginal utility of income
gamma = 0.05                              # marginal utility of civic duty (paying bill on time)
Theta = as.matrix(c(theta,alpha,gamma))
m = maxE
testgrid = cbind(runif(1000)*bound[2,1]) # points at which to test approximation
testgrid[sample(1:1000,10),1]=0
testgrid = as.matrix(sort(testgrid))
XgridCheby = Chebyshevgrid(testgrid,K,bound)

# train approx
u0 = waterutility(outCheby$grid[,1],50,500,m,Theta)
Lam = outCheby$Proj%*%u0

# Compute values at test points
u1 = waterutility(testgrid[,1],50,500,m,Theta)

# Approx. values at test points with Chebyshev Polynomials
u1hat = XgridCheby%*%Lam
uhat = outCheby$poly%*%Lam

print(summary(abs((u1-u1hat))))
print(summary(abs((u1-u1hat)/u1*100)))

plot(testgrid,u1,type="l")
lines(u1hat,col="red")

plot(outCheby$grid,u0,type="l")
plot(outCheby$grid,uhat,type="l")
lines(uhat,col="red")


#---------+---------+---------+---------+---------+---------+
# Use utility function as test case (fixed Debt Owed i.e., 2dims)
#---------+---------+---------+---------+---------+---------+
# create state grid
maxW = 1000
maxE = 500
maxO = 1000
bound = matrix(0,2,2)
bound[2,] = cbind(maxW,maxE)

# Construct grid for Chebyshev approximation
G = 50;                                   # Number Chebyshev nodes
K = 45;                                   # Order of Chebyshev approximation
outCheby = Chebyshev(G,K,bound)

#parameters
theta = c(0.01,-.00001,-10)               # water consumption utility
alpha = 0.01                              # marginal utility of income
gamma = 0.05                              # marginal utility of civic duty (paying bill on time)
Theta = as.matrix(c(theta,alpha,gamma))
m = maxE
testgrid = cbind(runif(1000)*bound[2,1],runif(1000)*bound[2,2]) # points at which to test approximation
testgrid[sample(1:1000,10),1]=0
testgrid[sample(1:1000,100),2]= m
XgridCheby = Chebyshevgrid(testgrid,K,bound)

# train approx
u0 = waterutility(outCheby$grid[,1],outCheby$grid[,2],500,m,Theta)
Lam = outCheby$Proj%*%u0

# Compute values at test points
u1 = waterutility(testgrid[,1],testgrid[,2],500,m,Theta)

# Approx. values at test points with Chebyshev Polynomials
u1hat = XgridCheby%*%Lam

print(summary(abs((u1-u1hat))))
print(summary(abs((u1-u1hat)/u1*100)))

plot(u1)
lines(u1hat,col="red")


#---------+---------+---------+---------+---------+---------+
# Use utility function as test case (3 dims)
#---------+---------+---------+---------+---------+---------+
# create state grid
maxW = 1000
maxE = 500
maxO = 1000
bound = matrix(0,2,3)
bound[2,] = cbind(maxW,maxMminusE,maxOminusE)

# Construct grid for Chebyshev approximation
G = 30;                                   # Number Chebyshev nodes
K = 20;                                   # Order of Chebyshev approximation
outCheby = Chebyshev(G,K,bound)

#parameters
theta = c(0.01,-.00001,-10)               # water consumption utility
alpha = 0.01                              # marginal utility of income
gamma = 0.05                              # marginal utility of civic duty (paying bill on time)
Theta = as.matrix(c(theta,alpha,gamma))
m = maxE
testgrid = cbind(runif(1000)*bound[2,1],runif(1000)*bound[2,2],runif(1000)*bound[2,3]) # points at which to test approximation
#testgrid[sample(1:1000,n=100),1]=0
#testgrid[sample(1:1000,n=100),2]=m
XgridCheby = Chebyshevgrid(testgrid,K,bound)

# train approx
u0 = waterutility(outCheby$grid[,1],outCheby$grid[,2],outCheby$grid[,3],m,Theta)
Lam = outCheby$Proj%*%u0

# Compute values at test points
u1 = waterutility(testgrid[,1],testgrid[,2],testgrid[,3],m,Theta)

# Approx. values at test points with Chebyshev Polynomials
u1hat = XgridCheby%*%Lam

print(summary(abs((u1-u1hat))))
print(summary(abs((u1-u1hat)/u1*100)))

plot(u1)
lines(u1hat,col="red")




#---------+---------+---------+---------+---------+---------+
# Use more complicated 2-dim function as test case
#---------+---------+---------+---------+---------+---------+
# create state grid
maxD = 400
maxB = 100
bound = matrix(0,2,2)
bound[2,] = cbind(maxD,maxB)

# Construct grid for Chebyshev approximation
G = 100;                                        # Number Chebyshev nodes
K = 15;                                         # Order of Chebyshev approximation

testgrid = cbind(as.matrix(seq(from=1,by=.403,to=400)),as.matrix(seq(from=1,by=.1,to=100)))
XgridCheby = Chebyshevgrid(testgrid,K,bound)

# train approx
u0 = as.matrix( cos(outCheby$grid[,1])*sin(outCheby$grid[,2]) + sqrt(outCheby$grid[,1]) + sqrt(outCheby$grid[,2]) )
Lam = outCheby$Proj%*%u0

# Compute actual values
u1 = as.matrix( cos(testgrid[,1])*sin(testgrid[,2]) + sqrt(testgrid[,1]) + sqrt(testgrid[,2]) )

# Approx. values at test points with Chebyshev Polynomials
u1hat = XgridCheby%*%Lam

print(summary((u1-u1hat)/u1*100))


#---------+---------+---------+---------+---------+---------+
# Use more complicated function as test case
#---------+---------+---------+---------+---------+---------+
K = 30
outCheby = Chebyshev(G,K,bound)
testgrid = cbind(as.matrix(seq(from=1,by=.403,to=400)),as.matrix(seq(from=1,by=.1,to=100)))
XgridCheby = Chebyshevgrid(testgrid,K,bound)

# train approx
u0 = as.matrix( sqrt(abs(outCheby$grid[,1])*abs(outCheby$grid[,2])) - .001*outCheby$grid[,1] + 50*outCheby$grid[,2]^2*(outCheby$grid[,2]<=10) )
Lam = outCheby$Proj%*%u0

# Compute actual values
u1 = as.matrix( sqrt(abs(testgrid[,1])*abs(testgrid[,2])) - .001*testgrid[,1] + 50*testgrid[,2]^2*(testgrid[,2]<=10) )

# Approx. values at test points with Chebyshev Polynomials
u1hat = XgridCheby%*%Lam

print(summary((u1-u1hat)/u1))

plot(u1)
lines(u1hat,col="red")
