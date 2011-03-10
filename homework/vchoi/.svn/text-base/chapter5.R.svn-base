#VChoi chapter 5

#####################################################################
#1a.
#####################################################################
logistic = function(init, r) {
  iter = 500
  x = numeric(iter)
  x[1] = init
  for(ii in 2:iter) {
    x[ii] = r*x[ii-1]*(1-x[ii-1])
  }
  return(x)
}

#####################################################################
#1d.
#This is a functor which takes as an argument a function which solves
#a logistic function numerically, and uses that to generate a plot
#####################################################################
plotLogistic = function(f, r, toplot = TRUE) {
  x = f(0.1, r)
  x = x[201:300]
  y = c(1:100)
  plot(y, x, xlab='iteration', ylab='x', type='l')
}

######################################################################
#1b. and 1c.
#This is the combination "wrapper" function and bifurcation plotting
#function.  It takes as an argument a function (logistic) which solves
#a logistic function with one parameter numerically and returns a 
#bifurcation plot for parameter values ranging from 0-4 with step sizes
#of 0.1.
######################################################################
bifurcation = function(f) {
  stepsize = 0.01
  rMin = 0
  rMax = 4
  steps = (rMax-rMin+1)/stepsize
  plot(c(rMin,rMax), c(0, 1), xlab='r', ylab='equilibrium', type='n')
  a = rMin
  for (ii in 1:steps) {
    x = f(0.1, a)
    x = x[201:500]
    for(jj in 1:300) {
      points(a, x[jj])
    }
    a = a+stepsize
  }
}

###################################################################
#2b-d
#returns a vector length niter-1 with the growth rates at each iteration
#if toplot==TRUE, then it will also generate a plot of growth rate at
#  each iteration
####################################################################
leslie = function(A, x1, y1, niter, toplot=TRUE) {
  P = matrix(data=c(x1, y1), nrow=2, ncol=1)
  param = matrix(data = c(0.5, 2, 0.1, A), nrow=2, ncol=2, byrow=TRUE)
  #x stores total population size at each iteration
  x = numeric(niter)
  for(ii in 1:niter) {
    P=param%*%P
    x[ii] = P[1, 1]+P[2, 1]
  }
  #y stores population growth rate at each iteration
  y = numeric(niter-1)
  for(ii in 2:niter) {
    y[ii] = x[ii]/x[ii-1]
  }
  if(toplot==TRUE){
    z = c(1:length(y))
    plot(z, y, xlab='iteration', ylab='growth rate', type = 'l')
  }
  return(y)
}

#########################################################
#3b.
#This function varies A from Amin to Amax with step size
#Astep.  For each A, the vector containing the initial 
#population sizes (x1, y1), is propogated using the 
#Usher matrix param for niter iterations.  The population
#size at iteration niter is plotted against A
##########################################################
usher = function(x1, y1) {
  Amin = 0
  Amax = 1
  Astep = 0.001
  niter = 50
  steps = (Amax-Amin)/Astep
  A = Amin
  x = numeric(steps)
  z = x
  for(ii in 1:steps) {
    P = matrix(data=c(x1, y1), nrow=2, ncol=1)
    param = matrix(data = c(0.5, 2, 0.1, A), nrow=2, ncol=2, byrow=TRUE)
    for(jj in 1:niter) {
      P=param%*%P
    }
    x[ii] = P[1, 1]+P[2, 1]
    z[ii] = A
    A = A + Astep
  }
  plot(z, x, xlab='A', ylab='total population', type = 'l')
}
