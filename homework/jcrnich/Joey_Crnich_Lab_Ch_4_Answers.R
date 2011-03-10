#!/usr/bin/env r
# encoding: utf-8
# 
# name: Crnich, Joseph
# assignment: 4
# date: 11/13/2009
# filename: Joey_Crnich_Lab_Ch_4_Answers.R
#############################################################################


#/--#########################################################################
# name: Crnich, Joseph
# assignment: 4
# date: 11/13/2009
# question: 1
# subquestion: a
# other files: Additional Papers
##########################################################################/--

## See additional papers with handwritten analysis.

#/--#########################################################################
# name: Crnich, Joseph
# assignment: 4
# date: 11/13/2009
# question: 1
# subquestion: b
# other files: Additional Papers
##########################################################################/--

## See additional papers with handwritten analysis.

#/--#########################################################################
# name: Crnich, Joseph
# assignment: 4
# date: 11/13/2009
# question: 1
# subquestion: c
# other files: 
##########################################################################/--

## Defining our SIS model as a function:

N <- 1
beta <- 0.3
gamma <- 0.1

sisFun <-function(I, t) {
	return(beta * I * (N - I) - gamma * I)
}

## Defining our forward Euler method using the previous SIS model:

fwdEuler <- function(sisFun, max_iter, step, I0, t0) {
	Ivals <- tvals <- numeric(length=(max_iter + 1))
	Ivals[1] <- I0
	tvals[1] <- t0
	for(ii in 1:max_iter) {
		Ivals[ii + 1] <- Ivals[ii] + step * sisFun(Ivals[ii], tvals[ii])
		tvals[ii + 1] <- tvals[ii] + step
	}
	return(data.frame(t=tvals, I=Ivals))
}


## Then, choosing a length of time to watch the model's behaviour over, as
## well as a time step:

maxT <- 365
deltaT <- c(0.1, 1:10)

## The following code initializes a plot, then the for loop displays each
## line in a different color on the resulting graph (The first time step has
## a separate if loop within the for loop because it is on a different scale
## than the other time steps).

plot(-1, 1, xlim=c(0, maxT), ylim=c(0, (2/3) * 1.25), type="l", main="SIS Model: beta=0.3, gamma=0.1", sub="Joey Crnich - 4.1c", xlab="t (days)", ylab="dI/dt", xaxs="i", yaxs="i")
for(ii in deltaT) {
	nSteps <- maxT / ii
	fEuler1 <- fwdEuler(sisFun, nSteps, ii, 0.01, 0)
	if(ii == 0.01) {
		lines(fEuler1$t, fEuler1$I, col=ii*10)
		labels <- paste("Time Step =", ii)
		text(maxT/2, fEuler1$I[length(fEuler1$I)] * 0.1 + ii * .05, labels,
	    adj=c(0,0), col=ii*10, cex=0.7)
	} else {
		lines(fEuler1$t, fEuler1$I, col=ii+1)
		labels <- paste("Time Step =", ii)
		text(maxT/2, fEuler1$I[length(fEuler1$I)] * 0.1 + ii * 0.05, labels,
	    adj=c(0,0), col=ii+1, cex=0.7)
	    }
}


## Here, code replots the graph on a more appropriate scale, showing the
## differences between each of the timesteps as they reach the equilibrium.

plot(-1, 1, xlim=c(0, maxT/4), ylim=c(0, (2/3) * 1.25), type="l", main="SIS Model: beta=0.3, gamma=0.1", sub="Joey Crnich - 4.1c", xlab="t (days)", ylab="dI/dt", xaxs="i", yaxs="i")
for(ii in deltaT) {
	nSteps <- maxT / ii
	fEuler1 <- fwdEuler(sisFun, nSteps, ii, 0.01, 0)
	if(ii == 0.01) {
		lines(fEuler1$t, fEuler1$I, col=ii*10)
		labels <- paste("Time Step =", ii)
		text(maxT/8, fEuler1$I[length(fEuler1$I)] * 0.1 + ii * .05, labels,
	    adj=c(0,0), col=ii*10, cex=0.7)
	} else {
		lines(fEuler1$t, fEuler1$I, col=ii+1)
		labels <- paste("Time Step =", ii)
		text(maxT/8, fEuler1$I[length(fEuler1$I)] * 0.1 + ii * 0.05, labels,
	    adj=c(0,0), col=ii+1, cex=0.7)
	    }
}


#/--#########################################################################
# name: Crnich, Joseph
# assignment: 4
# date: 11/13/2009
# question: 1
# subquestion: d
# other files: 
##########################################################################/--

## Redefining gamma and beta according to the problem:

beta <- 0.1
gamma <- 0.2

## Then replotting the graph using the same time steps and total time as
## before:

plot(-1, 1, xlim=c(0, maxT), ylim=c(0, (0.01) * 1.25), type="l", main="SIS Model: beta=0.1, gamma=0.2", sub="Joey Crnich - 4.1d", xlab="t (days)", ylab="dI/dt", xaxs="i", yaxs="i")
for(ii in deltaT) {
	nSteps <- maxT / ii
	fEuler1 <- fwdEuler(sisFun, nSteps, ii, 0.01, 0)
	if(ii == 0.01) {
		lines(fEuler1$t, fEuler1$I, col=ii*10)
		labels <- paste("Time Step =", ii)
		text(maxT/2, ii * .005 + 0.001, labels,
	    adj=c(0,0), col=ii*10, cex=0.7)
	} else {
		lines(fEuler1$t, fEuler1$I, col=ii+1)
		labels <- paste("Time Step =", ii)
		text(maxT/2, ii * 0.001 + 0.001, labels,
	    adj=c(0,0), col=ii+1, cex=0.7)
	    }
}


## Finally, replotting this graph as well, to better show the differences:

plot(-1, 1, xlim=c(0, maxT/6), ylim=c(-0.001, 0.0125), type="l", main="SIS Model: beta=0.1, gamma=0.2", sub="Joey Crnich - 4.1d", xlab="t (days)", ylab="dI/dt", xaxs="i", yaxs="i")
abline(0, 0)
for(ii in deltaT) {
	nSteps <- maxT / ii
	fEuler1 <- fwdEuler(sisFun, nSteps, ii, 0.01, 0)
	if(ii == 0.01) {
		lines(fEuler1$t, fEuler1$I, col=ii*10)
		labels <- paste("Time Step =", ii)
		text(maxT/12, ii * .005 + 0.001, labels,
	    adj=c(0,0), col=ii*10, cex=0.7)
	} else {
		lines(fEuler1$t, fEuler1$I, col=ii+1)
		labels <- paste("Time Step =", ii)
		text(maxT/12, ii * 0.001 + 0.001, labels,
	    adj=c(0,0), col=ii+1, cex=0.7)
	    }
}


#/--#########################################################################
# name: Crnich, Joseph
# assignment: 4
# date: 11/13/2009
# question: 2
# subquestion: a
# other files: Additional Papers
##########################################################################/--

## See additional papers with handwritten analysis.

#/--#########################################################################
# name: Crnich, Joseph
# assignment: 4
# date: 11/13/2009
# question: 2
# subquestion: b
# other files: Additional Papers
##########################################################################/--

## See additional papers with handwritten analysis.

#/--#########################################################################
# name: Crnich, Joseph
# assignment: 4
# date: 11/13/2009
# question: 2
# subquestion: c
# other files: Additional Papers
##########################################################################/--

## See additional papers with handwritten analysis.

#/--#########################################################################
# name: Crnich, Joseph
# assignment: 4
# date: 11/13/2009
# question: 2
# subquestion: d
# other files: 
##########################################################################/--

## Defining our constants...:

S <- 10^-6
kPlus <- 10^-3
kMinus <- 10^-5
N <- 10^-4

## ...the function...:

sigMolFun <-function(R, t) {
	return((N - R) * kMinus - R * S * kPlus)
}

## ...and the forward Euler for this model:

fwdEulerB <- function(sigMolFun, max_iter, step, R0, t0) {
	Rvals <- tvals <- numeric(length=(max_iter + 1))
	Rvals[1] <- R0
	tvals[1] <- t0
	for(ii in 1:max_iter) {
		Rvals[ii + 1] <- Rvals[ii] + step * sigMolFun(Rvals[ii], tvals[ii])
		tvals[ii + 1] <- tvals[ii] + step
	}
	return(data.frame(t=tvals, R=Rvals))
}


## Here we define the maximum time, the step size, and the number of iterations for our first run:

maxT <- 604800
ii <- 1
nSteps <- maxT / ii

## Then we define a data frame for the first timestep and plot it:

fEuler1 <- fwdEulerB(sigMolFun, nSteps, ii, 10^-4, 0)
plot(fEuler1$t, fEuler1$R, xlim=c(0, maxT), ylim=c(9.998*10^-5, (1.000025*10^-4)), type="l", main="Signaling Molecule Model", sub="Joey Crnich - 4.2d", xlab="t (seconds)", ylab="dR/dt", xaxs="i", yaxs="i")
text(maxT/2, (9.99903*10^-5 - ii*0.0001*10^-5), "Time Step = 1 sec", adj=c(0,0), col=ii, cex=0.7)

## Finally, a for loop plots other time steps to show the decreasing error
## with decreasing time step:

for(ii in (2:10)*18000) {
	nSteps <- maxT / ii
	fEuler1 <- fwdEulerB(sigMolFun, nSteps, ii, 10^-4, 0)
	lines(fEuler1$t, fEuler1$R, col=ii/18000)
	labels <- paste("Time Step =", ii/3600, "hrs")
	text(maxT/2, (9.99903*10^-5 - (ii/18000)*0.0001*10^-5), labels,
	    adj=c(0,0), col=ii/18000, cex=0.7)
}


#/--#########################################################################
# name: Crnich, Joseph
# assignment: 4
# date: 11/13/2009
# question: 2
# subquestion: e
# other files: 
##########################################################################/--

S <- 10^-6
kPlus <- 10^-3
kMinus <- 10^-5
N <- 10^-4

## Here we define a function for the integral of our model:

sigMolInt <- function(t) {
	A <- (N - ((N * kMinus) / (kMinus + S * kPlus)));
	(((N * kMinus) / (kMinus + S * kPlus)) + 
	A * exp(-t * (kMinus + S * kPlus)))
	}

## Choosing a maximum time and calculating the real equilibrium:

maxT <- 10000
actVal <- ((N * kMinus) / (kMinus + S * kPlus))

## Creating a vector of time steps to put into a for loop, then a vector for
## our resulting errors:

tStepVec <- sort(c(1, 2, 5) * c(10^(0:3), 10^(0:3), 10^(0:3)))
avErrVec <- numeric(length(tStepVec))

## This for loop creates data frames for every time step, then finds the difference between its final R value and the real equilibrium. Then it divides that difference by the total number of iterations and stores that value in the error vector created above:

for(ii in 1:length(tStepVec)) {
    nSteps <- maxT / tStepVec[ii]
    fEuler1 <- fwdEulerB(sigMolFun, nSteps, tStepVec[ii], 10^-4, 0)
    error <- abs(fEuler1$R[length(fEuler1)] - actVal)
    totErr <- error/nSteps
    avErrVec[ii] <- totErr
}

## Next, we define the slope as the average of the ratio between the change
## in rise and run (using the diff function to find the rises/runs). The
## paste command allows for the display of the slope on the graph.

slope <- mean(diff(log(tStepVec)) / diff(log(avErrVec)))
graphSlope <- paste("Slope = ", slope)

## Finally, we plot the log of the time step vector vs. the log of the error
## vector, and display the slope.

plot(tStepVec, avErrVec, type="o", main="Signaling Molecule Model: log(Error) vs. log(Time Step)", sub="Joey Crnich - 4.2e", col.sub="darkgrey", xlab="log(time step (s)))", ylab="log(Average Total Error)", log=c("xy"))
text(tStepVec[7], avErrVec[4], graphSlope, adj=c(0,1))


#/--#########################################################################
# name: Crnich, Joseph
# assignment: 4
# date: 11/13/2009
# question: 2
# subquestion: f
# other files: 
##########################################################################/--

S <- 10^-6
kPlus <- 10^-3
kMinus <- 10^-5
N <- 10^-4

## After solving for the slope at a future time step (i+1; see additional papers), we define a function for it:

ftrslp <- function(R, t, step)
    return((R + step * N * kMinus) / (1 + step * (kMinus + kPlus * S)))

## Next we create another function for the Euler method, this time using the
## backwards version:

bkwdEuler <- function(ftrslp, max_iter, step, R0, t0) {
	Rvals <- tvals <- numeric(length=(max_iter + 1))
	Rvals[1] <- R0
	tvals[1] <- t0
	for(ii in 1:max_iter) {
		Rvals[ii + 1] <- ftrslp(Rvals[ii], tvals[ii], step)
		tvals[ii + 1] <- tvals[ii] + step
	}
	return(data.frame(t=tvals, R=Rvals))
}

## Again, defining the maximum time, and the time step; then, the number of
## iterations:

maxT <- 604800
deltaT <- 1
nSteps <- maxT / deltaT;

## Finally, running the backward Euler function with the preceding parameters
## and plotting it:

bEuler <- bkwdEuler(ftrslp, nSteps, deltaT, 10^-4, 0);

plot(bEuler$t, bEuler$R, xlim=c(0, maxT), type="l", main="Backward Euler", sub="Joey Crnich - 4.1f", xlab="t", ylab="R", xaxs="i", yaxs="i")


#/--#########################################################################
# name: Crnich, Joseph
# assignment: 4
# date: 11/13/2009
# question: 2
# subquestion: g
# other files: 
##########################################################################/--

## Here, we choose a maximum time and calculate the real equilibrium again:

maxT <- 1000
actVal <- ((N * kMinus) / (kMinus + S * kPlus))

## Then create a time step vector to run through the following for loop, as
## well as a vector for our error values:

tStepVec <- sort(c(1, 2, 5) * c(10^(0:3), 10^(0:3), 10^(0:3)))
avErrVec <- numeric(length(tStepVec))

## Next, a for loop creates data frames for each time step, finds the
## difference between their final values and the actual value, and calculates
## the total error before entering it into the previously made vector.

for(ii in 1:length(tStepVec)) {
    nSteps <- maxT / tStepVec[ii]
    bEuler <- bkwdEuler(ftrslp, nSteps, tStepVec[ii], 10^-4, 0)
    error <- abs(bEuler$R[length(bEuler)] - actVal)
    totErr <- error/nSteps
    avErrVec[ii] <- totErr
}

## As done earlier, the slope is calculated from the rise and run:

slope <- mean(diff(log(tStepVec)) / diff(log(avErrVec)))
graphSlope <- paste("Slope = ", slope)

## And finally, a log-log plot of the time steps and errors, with the slope
## of the log-line displayed:

plot(tStepVec, avErrVec, type="o", main="Signaling Molecule Model: log(Error) vs. log(Time Step)", sub="Joey Crnich - 4.2e", col.sub="darkgrey", xlab="log(time step (s)))", ylab="log(Average Total Error)", log=c("xy"))
text(tStepVec[7], avErrVec[4], graphSlope, adj=c(0,1))


