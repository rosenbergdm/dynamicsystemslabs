#!/usr/bin/env r
# encoding: utf-8
# 
# name: Katie Sirianni
# assignment: 5
# date: 11/14/09
# filename: REAL Sirianni Homework 5 Submission
#############################################################################

#/--#########################################################################
# name: Katie Sirianni
# assignment: 5
# date: 11/14/09
# question: 1
# subquestion: c
##########################################################################/--

N <- 1          # --> I and S are fractions of pop
B <- 0.3        # per day NOT per timestep
gamma <- 0.1    # per day NOT per timestep

(B*N-gamma) / B   # this is the predicted equilibrium point for number of infected individuals, since gamma is less than B*N = 0.3

x0 <- .01         # initial fraction infected size
t0 <- 1         # time
maxIter <- 364  # number of iterations
step<- 1        # length of time step

plot(-1, -1, 
	main= "SIS Model for Gamma = 0.1, Beta = 0.3", 
	sub= "Sirianni Hmwk 5.1c", 
	ylab= "Estimate of I",
	xlab= "Time(Days)", 
	xlim= c(0,60), 
	ylim= c(0, 0.8), 
	type= "l")  ## This is done to initialize the plot.

sisFunction<- function(x,t) {
	return( B * (N - x) * x - gamma * x)
}               # This defines the ODE, in this case it is the SIS model of infections

## in the forward euler method, we use the estimate from the previous time plus the timestep times the change predicted at the previous time to calculate the new estimate:

forwardEuler<- function(sisFunction, maxIter, step, x0, t0) {
	xvals <- numeric(length = (maxIter + 1));
	tvals <- numeric(length = (maxIter + 1));
	xvals[1] <- x0;
	tvals[1] <- t0;
	for( ii in 1: maxIter){
		xvals[ii + 1] <- xvals[ii] + 
		    step * sisFunction(xvals[ii], tvals[ii]);
		tvals[ii + 1] <- tvals[ii] + step
	}
	lines(tvals, xvals, col= (step))
	timePoint<- "Time Step ="
	finalTimePoint <- paste(timePoint, step)
	text(x= 0, y= step / 20 + 0.25 , labels = finalTimePoint, adj = c(0,1), cex = 0.6, col = (step))
   return(data.frame(t= tvals, x= xvals));
}

## this loop just generates the predictions using multiple time steps on the same plot (which was initialized earlier)

for(step in 1 : 10) {
	forwardEuler(sisFunction, maxIter, step, x0, t0)
}
lines(forwardEuler(sisFunction, maxIter= 3650, step= 0.1, x0, t0), col = "slateblue")
     timePoint<- "Time Step ="
     finalTimePoint <- paste(timePoint, 0.1)
     text(x= 0, y= 0.1 / 20 + 0.25, labels = finalTimePoint, adj = c(0,1), cex = 0.6, col = "slateblue")
	
## The curve appears to stabilize at about time= 4-5, although all of the time steps less than 10 level off at the appropriate equilibirium within one year. I say that it stabilizes at t=4 or 5 because there are few, if any, oscillations around the equilibrium before it settles there.   

#/--#########################################################################
# name: Katie Sirianni
# assignment: 5
# date: 11/14/09
# question: 1
# subquestion: d
##########################################################################/--


N <- 1          # --> I and S are fractions of pop
B <- 0.1        # per day NOT per timestep
gamma <- 0.2    # per day NOT per timestep

# I=0 is the predicted stable equilibrium in this case, since B*N = 0.1 is less than gamma = 0.2

x0 <- 0.01         # initial fraction infected size
t0 <- 1         # time
maxIter <- 364  # number of iterations
step<- 1        # length of time step

plot(-1, -1, 
	main= "SIS Model for Gamma= 0.2, Beta= 0.1", 
	sub= "Sirianni Hmwk 5.1d", 
	ylab= "Estimate of I",
	xlab= "Time(Days)", 
	xlim= c(0,75), 
	ylim= c(-0.001, 0.010), 
	type= "l")  ## This is done to initialize the plot.


for(step in 1:10) {
	forwardEuler(sisFunction, maxIter, step, x0, t0)
	
	timePoint<- "Time Step ="
	finalTimePoint <- paste(timePoint, step)
	text(x= 50, y= step / 1100 + 0.001  , labels = finalTimePoint, adj = c(0,1), cex = 0.6, col = (step))
}

lines(forwardEuler(sisFunction, maxIter= 3650, step= 0.1, x0, t0), col = "slateblue")
     timePoint<- "Time Step ="
     finalTimePoint <- paste(timePoint, 0.1)
     text(x= 50, y= 0.1 / 1100 + 0.001, labels = finalTimePoint, adj = c(0,1), cex = 0.6, col = "slateblue")

## at t= 10 the number of infected individuals goes below 0, which is not biologically feasible.  Other than that, they all go to 0, although the larger step sizes get there more quickly than predicted by the model.

#/--#########################################################################
# name: Katie Sirianni
# assignment: 5
# date: 11/14/09
# question: 2
# subquestion: d
##########################################################################/--

s <- 10**-6       # S is the conc. of the signaling molecule
plusK <- 10**-3   # the rate at which the complex between S and R is formed
minusK <- 10**-5  # the rate at which the complex between S and R degenerates
r0 <- 10**-4      # the initial value of R
t0 <- 0
maxIter <- 2000    # number of iterations
step <- 1         # length of the time step
N<-10**-4

(minusK * N) / (plusK * s + minusK) # This is the equilibrium
	
concFunction<- function(r, t) {
	return(-plusK * s * r + minusK * (N-r))
}

forwardEuler<- function(concFunction, maxIter, step, r0, t0) {
	rvals <- numeric(length = (maxIter + 1));
	tvals <- numeric(length = (maxIter + 1));
	rvals[1] <- r0;
	tvals[1] <- t0;
	for( ii in 1: maxIter){
		rvals[ii + 1] <- rvals[ii] + 
		    step * concFunction(rvals[ii], tvals[ii]);
		tvals[ii + 1] <- tvals[ii] + step
	}

   return(data.frame(t= tvals, r= rvals));
}

concPlot<- forwardEuler(concFunction, maxIter= 10, step=150000, r0, t0)
plot(concPlot$t, concPlot$r, 
   type= "l",
   main= "Concentration of R (using Forward Euler)", 
	sub= "Sirianni Hmwk 5.2d", 
	ylab= "Estimate of R",
	xlab= "Time(seconds)",)
for(step in 1:10 * 15000 ) {
   concPlot<- forwardEuler(concFunction, maxIter, step, r0, t0)
   lines(concPlot$t, concPlot$r, col= (step/1000))
	timePoint<- "Time Step ="
	finalTimePoint <- paste(timePoint, step)
	text(x= 1000000, y= step/15000000070000 + 0.00009999, 
	  labels = finalTimePoint, 
	  adj = c(0,1), cex = 0.6, 
	  col = (step / 1000))
}

timePoint<- "Time Step ="
finalTimePoint<- paste(timePoint, step)


#/--#########################################################################
# name: Katie Sirianni
# assignment: 5
# date: 11/14/09
# question: 2
# subquestion: e
##########################################################################/--

s <- 10**-6       # S is the conc. of the signaling molecule
plusK <- 10**-3   # the rate at which the complex between S and R is formed
minusK <- 10**-5  # the rate at which the complex between S and R degenerates
r0 <- 10**-4      # the initial value of R
t0 <- 0
maxIter <- 2000    # number of iterations
step <- 1         # length of the time step
N<-10**-4

concFunction<- function(r, t) {
	return(-plusK * s * r + minusK * (N-r))
}

forwardEuler<- function(concFunction, tRange, step, r0) {
	tVals <- seq(from=tRange[1], to=tRange[2], by=step);
	rVals <- tVals;
	rVals[1] <- r0;
	for( ii in 1: (length(tVals) - 1)){
		rVals[ii + 1] <- rVals[ii] + step * concFunction(rVals[ii], tvals[ii]);
	}
   return(data.frame(t= tVals, r= rVals));
}

t<- 600000
realValue<- (minusK*N)/ (plusK*s + minusK) + 
	    (r0- (minusK*N)/ (plusK*s + minusK))* exp(-(plusK*s + 
	    minusK)*t)
	    
timeSteps<- c(5,10, 20, 50, 100, 200, 500, 1000, 2000, 5000, 10000)
errors<- numeric(length= length(timeSteps))

for(step in 1:length(timeSteps)) {
	eulerPrediction<- forwardEuler(concFunction, c(0, 1e6), timeSteps[step], r0)
	predictionErrors<- abs(eulerPrediction$r[length(eulerPrediction$r)] - realValue) /
	                   length(eulerPrediction$r)
	errors[step]<- predictionErrors
}

plot(timeSteps, errors, 
   type<- "l", 
   log=c('xy'), 
   main= "Total Error for Different Time Steps", 
   sub= "Sirianni Homework 5.2e",
   xlab= "Time Steps (Seconds)", 
   ylab= "Total Error", 
   col= "darkseagreen4"
   )  
points(timeSteps, errors, pch= 23, col= "plum", bg= "lightgoldenrod")
slope<- mean(diff(log(errors)) / diff(log(timeSteps)))
slopeTitle<- "Slope ="
slopeLegend<- paste(slopeTitle, slope)                   
text(20, 5e-15, slopeLegend, col= "chocolate4", cex = 0.8)
#since the slope is 1 (or close to it, I guess), it is a first order method of prediction

tableErrors<- cbind(timeSteps, errors)

#/--#########################################################################
# name: Katie Sirianni
# assignment: 5
# date: 11/14/09
# question: 2
# subquestion: f
##########################################################################/--

s <- 10**-6       # S is the conc. of the signaling molecule
plusK <- 10**-3   # the rate at which the complex between S and R is formed
minusK <- 10**-5  # the rate at which the complex between S and R degenerates
r0 <- 10**-4      # the initial value of R
t0 <- 0
maxIter <- 100000    # number of iterations
step <- 15        # length of the time step
N<-10**-4

(minusK * N) / (plusK * s + minusK) # This is the equilibrium

backwardEuler<- function(maxIter, step, r0, t0) {
	rvals<- numeric(length = (maxIter + 1))
	tvals<- numeric(length = (maxIter + 1))
	rvals[1]<- r0
	tvals[1]<- t0
	for(ii in 1:maxIter){
	    tvals[ii + 1] <- tvals[ii] + step
	    rvals[ii + 1] <- (rvals[ii] + step* minusK * N) / (1 + step * (minusK + plusK * s))
	}
	return(data.frame(t=tvals, r=rvals)) 
}

beEst<- backwardEuler(maxIter= 10000 , step= 150 , r0, t0)
plot(beEst$t, beEst$r, 
   type= "l",
   main= "Numerical Solution using Backward Euler", 
   xlab= "Time(seconds)", 
   ylab= "Concentration of I (M)")

#/--#########################################################################
# name: Katie Sirianni
# assignment: 5
# date: 11/14/09
# question: 2
# subquestion: g
##########################################################################/--

s <- 10**-6       # S is the conc. of the signaling molecule
plusK <- 10**-3   # the rate at which the complex between S and R is formed
minusK <- 10**-5  # the rate at which the complex between S and R degenerates
r0 <- 10**-4      # the initial value of R
t0 <- 0
maxIter <- 200000    # number of iterations
N<-10**-4

backwardEuler<- function(maxIter, step, r0, t0) {
	rvals<- numeric(length = (maxIter + 1))
	tvals<- numeric(length = (maxIter + 1))
	rvals[1]<- r0
	tvals[1]<- t0
	for(ii in 1:maxIter){
	    tvals[ii + 1] <- tvals[ii] + step
	    rvals[ii + 1] <- (rvals[ii] + step* minusK * N) / (1 + step * (minusK + plusK * s))
	}
	return(data.frame(t=tvals, r=rvals)) 
}

t<- 600000
realValue<- (minusK*N)/ (plusK*s + minusK) + 
	    (r0- (minusK*N)/ (plusK*s + minusK))* exp(-(plusK*s + 
	    minusK)*t)
	    
timeSteps<- c(10, 20, 50, 100, 200, 500, 1000, 2000, 5000, 10000)
errors<- numeric(length= length(timeSteps))

for(step in 1:length(timeSteps)) {
	eulerPrediction<- backwardEuler(maxIter, timeSteps[step], r0, t0)
	predictionErrors<- abs(eulerPrediction$r[length(eulerPrediction$r)] - realValue) /
	                   (maxIter / timeSteps[step])
	errors[step]<- predictionErrors
}

plot(timeSteps, errors, 
   type<- "l", 
   log=c('xy'), 
   main= "Total Error for Different Time Steps", 
   sub= "Sirianni Homework 5.2g",
   xlab= "Time Steps (Seconds)", 
   ylab= "Total Error", 
   col= "darkseagreen4"
   )  
points(timeSteps, errors, pch= 23, col= "plum", bg= "lightgoldenrod")
slope<- mean(diff(log(errors)) / diff(log(timeSteps)))
slopeTitle<- "Slope ="
slopeLegend<- paste(slopeTitle, slope)                   
text(250, 5e-15, slopeLegend, col= "chocolate4", cex = 0.8)
#since the slope is ~ 1, it is a first order method of prediction


tableErrorsBE<- cbind(timeSteps, errors)
