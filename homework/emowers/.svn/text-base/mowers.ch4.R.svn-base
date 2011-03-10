# 
# name: Erin Mowers
# assignment: Chapter 4 Exercise
# date: 11/13/09
# filename: Mowers_Ch4_Exercise
#############################################################################







#/--#########################################################################
# name: Erin Mowers
# assignment: Chapter 4 Exercise
# date: 11/13/09
# question: 1
# subquestion: c
# other files: See written copy for 1a,1b, and explanations
##########################################################################/--

Epidemic <- function(I,t,B,g) {
  return((B * (1 - I) * I) - (g * I));
}

forwardEuler <- function(Epidemic, B, g, step, x0) {
  max_iter <- 365 / step
  xvals <- tvals <- numeric(length=(max_iter + 1));
  xvals[1] <- x0;
  tvals[1] <- 0;
  for (ii in 1:max_iter) {
    xvals[ii+1] <- xvals[ii] +
      step * Epidemic(xvals[ii], tvals[ii], B, g);
    tvals[ii+1] <- tvals[ii] + step
  }
  return(lines(tvals, xvals, lty=1, type='l'));
}

#The below equation plots multiple forward Euler approximations with differet
#delta_t's on the same graph so that you can compare what time step is appropriate
#and displays behavior predicted theoretically.
multipleEuler <- function(forwardEuler,B,g,x0,stepMin,stepMax,numLines) {
  plot(c(0,365),c(0,1), type='n',
    main='Mowers 1c: Epidemic!!!',
    xlab='time (days)', ylab='number of infected individuals')
  dstep <- (stepMax - stepMin) / numLines
  step_array <- numeric(length=(numLines+1))
  step_array[1] <- stepMin
  for (iii in 1:(numLines)) {
    step_array[iii+1] <- step_array[iii] + dstep
  }
  for (iiii in 1:numLines+1) {
    step <- step_array[iiii]
    forwardEuler(Epidemic,B,g,step,x0)
  }
  return('See graph')
}


#To solve problem:
multipleEuler(forwardEuler,0.3,0.1,0.01,0.1,10,20) 
multipleEuler(forwardEuler,0.3,0.1,0.01,0.1,10,100)
plot(c(0,365),c(0,1), type='n',
    main='Mowers 1c: Epidemic!!!',
    xlab='time (days)', ylab='number of infected individuals')
forwardEuler_plot(Epidemic,0.3,0.1,10,0.01)


#Please see attached handwritten sheets for explanation

 


#/--#########################################################################
# name: Erin Mowers
# assignment: Chapter 4 Exercise
# date: 11/13/09
# question: 1
# subquestion: d
# other files: Relies on code listed in 1c above to implement the following.
#              Also see handwritten sheets for explanation.
##########################################################################/--

multipleEuler <- function(forwardEuler,B,g,x0,stepMin,stepMax,numLines) {
  plot(c(0,365),c(0,0.05), type='n',
    main='Mowers 1c: Epidemic!!!',
    xlab='time (days)', ylab='number of infected individuals')
  dstep <- (stepMax - stepMin) / numLines
  step_array <- numeric(length=(numLines+1))
  step_array[1] <- stepMin
  for (iii in 1:(numLines)) {
    step_array[iii+1] <- step_array[iii] + dstep
  }
  for (iiii in 1:numLines+1) {
    step <- step_array[iiii]
    forwardEuler(Epidemic,B,g,step,x0)
  }
  return('See graph')
}

#To answer problem:
multipleEuler(forwardEuler,0.1,0.2,0.01,0.1,25,10)
plot(c(0,365),c(0,0.05), type='n',
    main='Mowers 1c: Epidemic!!!',
    xlab='time (days)', ylab='number of infected individuals')
forwardEuler(Epidemic,0.1,0.2,20,0.01)

#See attached handwritten pages for discussion.




#/--#########################################################################
# name: Erin Mowers
# assignment: Chapter 4 Exercise
# date: 11/13/09
# question: 2
# subquestion: d
# other files: See handwritten sheets for explanations
##########################################################################/--

Signaling <- function(R,t,S,R0,k,c) {
  return((c * R0) - (R * (c + (S * k))));
}

forwardEuler <- function(Signaling, S, R0, k, c, step) {
  max_iter <- 1000000 / step
  xvals <- tvals <- numeric(length=(max_iter + 1));
  xvals[1] <- R0;
  tvals[1] <- 0;
  for (ii in 1:max_iter) {
    xvals[ii+1] <- xvals[ii] +
      step * Signaling(xvals[ii], tvals[ii], S, R0, k, c);
    tvals[ii+1] <- tvals[ii] + step
  }
  plot(tvals, xvals, lty=1, type='l',
    main='Mowers 2d: Forward Euler Signaling',
    xlab='time (seconds)', ylab='Receptor concentration [M]')
  return(cat('The estimation is:',xvals[max_iter]))
}

forwardEuler_plot <- function(Signaling, S, R0, k, c, step) {
  max_iter <- 1000000 / step
  xvals <- tvals <- numeric(length=(max_iter + 1));
  xvals[1] <- R0;
  tvals[1] <- 0;
  for (ii in 1:max_iter) {
    xvals[ii+1] <- xvals[ii] +
      step * Signaling(xvals[ii], tvals[ii], S, R0, k, c);
    tvals[ii+1] <- tvals[ii] + step
  }
  return(lines(tvals, xvals, lty=1, type='l'))
}

#To evaluate the model:
forwardEuler(Signaling,10^-6,10^-4,10^-3,10^-5,1000)


#/--#########################################################################
# name: Erin Mowers
# assignment: Chapter 4 Exercise
# date: 11/13/09
# question: 2
# subquestion: e
# other files: See handwritten sheets for explanations.
##########################################################################/--

#Differential equation
#k is k+, and c is k-
#R0 is the initial value of receptor
SignalingEst <- function(R,t,S,R0,k,c) {
  return((c * R0) - (R * (c + (S * k))));
}


#Analytical solution 
SignalingAct <- function(t,S,R0,k,c) {
  q <- c + (S * k)
  m <- c / q
  return((R0 * m) +
    (R0 * (1 - m) * exp((-t) * q)))
}




#Forward Euler

forwardEulerError <- function(SignalingAct, SignalingEst, S, R0, k, c, step) {
  max_iter <- 1000000 / step
  xvals <- tvals <- numeric(length=(max_iter + 1));
  xvals[1] <- R0;
  tvals[1] <- 0;
  for (ii in 1:max_iter) {
    xvals[ii+1] <- xvals[ii] +
      step * SignalingEst(xvals[ii], tvals[ii], S, R0, k, c);
    tvals[ii+1] <- tvals[ii] + step
  }
  trueSol <- SignalingAct(t=tvals, S, R0, k, c);
  error <- sum(abs(trueSol - xvals))/(1000000/step)
  return(error)
}


#Error for multiple time steps
#e is forwardEulerError
#x is SignalingAct (analytic solution)
#y is Signaling (dR/dt)
#S is initial substrate = 10^-6
#R0 is total receptor = 10^-2
#k is forward rate = 10^-3
#c is backwards rate = 10^-5
#tsteps is a vector of different time steps

forwardErrorTotal <- function(e, x, y, S, R0, k, c, tsteps) {
  errorTotal <- numeric(length=(tsteps))
  for(ii in 1:length(tsteps)) {
    errorTotal[ii] <- e(x, y, S, R0, k, c, tsteps[ii])
  }
  plot(tsteps, errorTotal, log='xy', lty=1, type='l',
    main='Mowers 2e: Forward Euler Order',
    xlab='log(time step)', ylab='log(total error)')
  slope <- mean(diff(log(errorTotal))/diff(log(tsteps)))
  return(cat('Please see attached graph.
    The slope and order of the forward Euler is:', slope));
}


#Here's the input to solve probelm:

S <- 10^-6
R0 <- 10^-2
k <- 10^-3
c <- 10^-5
tsteps <- c(10,20,50,100,200,500,1000,2000,5000,10000,20000,50000,100000)
forwardErrorTotal(forwardEulerError, SignalingAct, SignalingEst, S, R0, k, c, tsteps)





#/--#########################################################################
# name: Erin Mowers
# assignment: Chapter 4 Exercise
# date: 11/13/09
# question: 2
# subquestion: f
# other files: 
##########################################################################/--

Signaling <- function(R,t,S,R0,k,c) {
  return((c * R0) - (R * (c + (S * k))));
}

backwardEuler <- function(Signaling, S, R0, k, c, step) {
  max_iter <- 1000000 / step
  xvals <- tvals <- numeric(length=(max_iter + 1));
  xvals[1] <- R0;
  tvals[1] <- 0;
  for (ii in 1:max_iter) {
    xvals[ii+1] <- (xvals[ii] + (step * c * R0)) / 
      (1 + (step * (c + (S * k))));
    tvals[ii+1] <- tvals[ii] +step;
  }
  plot(tvals, xvals, lty=1, type='l',
    main='Mowers 2f: Backwards Euler Signaling',
    xlab='time (seconds)', ylab='Receptor concentration [M]')
  return(cat('The estimation is:',xvals[max_iter]))
}  
    





#/--#########################################################################
# name: Erin Mowers
# assignment: Chapter 4 Exercise
# date: 11/13/09
# question: 2
# subquestion: g
# other files: 
##########################################################################/--

#Differential equation
#k is k+, and c is k-
#R0 is the initial value of receptor
SignalingEst <- function(R,t,S,R0,k,c) {
  return((c * R0) - (R * (c + (S * k))));
}


#Analytical solution 
SignalingAct <- function(t,S,R0,k,c) {
  q <- c + (S * k)
  m <- c / q
  return((R0 * m) +
    (R0 * (1 - m) * exp((-t) * q)))
}




#Backward Euler

backwardEulerError <- function(SignalingAct, SignalingEst, S, R0, k, c, step) {
  max_iter <- 1000000 / step
  xvals <- tvals <- numeric(length=(max_iter + 1));
  xvals[1] <- R0;
  tvals[1] <- 0;
  for (ii in 1:max_iter) {
    xvals[ii+1] <- (xvals[ii] + (step * c * R0)) / 
      (1 + (step * (c + (S * k))));
    tvals[ii+1] <- tvals[ii] +step;
  }
  trueSol <- SignalingAct(t=tvals, S, R0, k, c);
  error <- sum(abs(trueSol - xvals))/(1000000/step)
  return(error)
}


#Error for multiple time steps
#e is backwardEulerError
#x is SignalingAct (analytic solution)
#y is Signaling (dR/dt)
#S is initial substrate = 10^-6
#R0 is total receptor = 10^-2
#k is forward rate = 10^-3
#c is backwards rate = 10^-5
#tsteps is a vector of different time steps

backwardErrorTotal <- function(e, x, y, S, R0, k, c, tsteps) {
  errorTotal <- numeric(length=(tsteps))
  for(ii in 1:length(tsteps)) {
    errorTotal[ii] <- e(x, y, S, R0, k, c, tsteps[ii])
  }
  plot(tsteps, errorTotal, log='xy', lty=1, type='l',
    main='Mowers 2g: Backward Euler Order',
    xlab='log(time step)', ylab='log(total error)')
  slope <- mean(diff(log(errorTotal))/diff(log(tsteps)))
  return(cat('Please see attached graph.
    The slope and order of the forward Euler is:', slope));
}


#Here's the input to solve probelm:

S <- 10^-6
R0 <- 10^-2
k <- 10^-3
c <- 10^-5
tsteps <- c(10,20,50,100,200,500,1000,2000,5000,10000,20000,50000,100000)
backwardErrorTotal(backwardEulerError, SignalingAct, SignalingEst, S, R0, k, c, tsteps)
