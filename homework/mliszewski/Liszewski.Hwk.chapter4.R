#!/usr/bin/env r
# encoding: utf-8
# 
# name: Megan Liszewski
# assignment: Chapter 4
# date: 11/20/09
# filename: Liszewski_Chapter4.r
#############################################################################

#/--#########################################################################
# name: Megan Liszewski
# assignment: Chapter 4
# date: 11/20/09
# question: 1
# subquestion: a&b
# other files: see handwritten work
##########################################################################/--

#/--#########################################################################
# name: Megan Liszewski
# assignment: Chapter 4
# date: 11/20/09
# question: 1
# subquestion: c
# other files: Liszewski_Chap4_1c_plot.pdf
##########################################################################/--


f <- function(I,t) {
	return (b*(N-I)*I-g*I);
}
forwardEuler <- function(f, max_iter, step, I0, t0) {
	Ivals <- tvals <- numeric(length=(max_iter + 1));
	Ivals[1] <- I0;
	tvals[1] <- t0;
	for (ii in 1:max_iter) {
		Ivals[ii+1] <- Ivals[ii] + step*f(Ivals[ii],tvals[ii]);
		tvals[ii+1] <- tvals[ii] + step
	}
	return(data.frame(t=tvals, I=Ivals))
	
}
N<-1
b<-0.3
g<-0.1

step_size<-1
nSteps <- 365/step_size
feuler1 <- forwardEuler(f, nSteps, step_size, 2, 0);
t<-feuler1[1]
t<-t[,1]
I<-feuler1[2]
I<-I[,1]
plot(t,I,type='l',main='Chapter 4 Homework #1c: Forward Euler SIS Epidemiology Model',ylab= '# Infected Individuals',xlab='time (days)',sub='Megan Liszewski')

#When varying the time step, for step_size < 2.5, the system approaches 0.667. When the step_size = 2.5, the system approaches 0. With a time-step above that, the simulation no longer gives back numbers. For step_size < 1.65, there is a decline in the population that eventually reaches ~0.667. For 1.65 < step_size < 2.5, there is a dip in the population which then comes back up to approach 0.667.


#/--#########################################################################
# name: Megan Liszewski
# assignment: Chapter 4
# date: 11/20/09
# question: 1
# subquestion: d
# other files: Liszewski_Chap4_1d_plot.pdf
##########################################################################/--

quartz()
f <- function(I,t) {
	return (b*(N-I)*I-g*I);
}
forwardEuler <- function(f, max_iter, step, I0, t0) {
	Ivals <- tvals <- numeric(length=(max_iter + 1));
	Ivals[1] <- I0;
	tvals[1] <- t0;
	for (ii in 1:max_iter) {
		Ivals[ii+1] <- Ivals[ii] + step*f(Ivals[ii],tvals[ii]);
		tvals[ii+1] <- tvals[ii] + step
	}
	return(data.frame(t=tvals, I=Ivals))
	
}
N<-1
b<-0.1
g<-0.2

step_size<-0.1
nSteps <- 365/step_size
feuler1 <- forwardEuler(f, nSteps, step_size, 2, 0);
t<-feuler1[1]
t<-t[,1]
I<-feuler1[2]
I<-I[,1]
plot(t,I,type='l',main='Chapter 4 Homework #1d: Forward Euler SIS Epidemiology Model',ylab= '# Infected Individuals',xlab='time (days)',sub='Megan Liszewski')

#When varying the time step, for step_size < 5, the system approaches 0. When the step_size = 5, the system goes negative and then stays there. With a time-step above that, the simulation no longer gives back numbers. For step_size < 3.33, there is a decline in the population that eventually reaches ~0. For 3.33 < step_size < 5, there is a dip in the population which then comes back up to approach 0. However, this dip goes below zero which is not possible because there cannot be a negative population size.

#/--#########################################################################
# name: Megan Liszewski
# assignment: Chapter 4
# date: 11/20/09
# question: 2
# subquestion: d
# other files: Liszewski_Chap4_2d_plot.pdf
##########################################################################/--

quartz()

f <- function(R,t) {
	return ( -(k_pos*S + k_neg)*R + k_neg*N);
}
forwardEuler <- function(f, max_iter, step, R0, t0) {
	Rvals <- tvals <- numeric(length=(max_iter + 1));
	Rvals[1] <- R0;
	tvals[1] <- t0;
	for (ii in 1:max_iter) {
		Rvals[ii+1] <- Rvals[ii] + step*f(Rvals[ii],tvals[ii]);
		tvals[ii+1] <- tvals[ii] + step
	}
	return(data.frame(t=tvals, R=Rvals))
	
}
S<-10^(-6)
k_pos <-10^(-3)
k_neg<-10^(-5)
R0<-10^(-2)
N<-10^(-2)

step_size<-500
nSteps <- 10^(6)/step_size
feuler1 <- forwardEuler(f, nSteps, step_size, R0, 0);
t<-feuler1[1]
t<-t[,1]
R<-feuler1[2]
R<-R[,1]
plot(t,R,type='l',main='Chapter 4 Homework #2d: Forward Euler Chemical Reaction Model',ylab= 'Concentration reactant R (M)',xlab='time (seconds)',sub='Megan Liszewski')

#/--#########################################################################
# name: Megan Liszewski
# assignment: Chapter 4
# date: 11/20/09
# question: 2
# subquestion: e
# other files: Liszewski_Chap4_2e_plot.pdf
##########################################################################/--

quartz()

f <- function(R,t) {
	return ( -(k_pos*S + k_neg)*R + k_neg*N);
}
forwardEuler <- function(f, max_iter, step, R0, t0) {
	Rvals <- tvals <- numeric(length=(max_iter + 1));
	Rvals[1] <- R0;
	tvals[1] <- t0;
	for (ii in 1:max_iter) {
		Rvals[ii+1] <- Rvals[ii] + step*f(Rvals[ii],tvals[ii]);
		tvals[ii+1] <- tvals[ii] + step
	}
	return(data.frame(t=tvals, R=Rvals))
	
}
S<-10^(-6)
k_pos <-10^(-3)
k_neg<-10^(-5)
R0<-10^(-2)
N<-10^(-2)

step_size<-c(2,5,10,20,50,100,200,500,1000,2000,5000)
error<-c()

for (jj in 1:length(step_size)){
nSteps <- 10^(6)/step_size[jj]
feuler1 <- forwardEuler(f, nSteps, step_size[jj], R0, 0);
t<-feuler1[1]
t<-t[,1]
R<-feuler1[2]
R<-R[,1]

R_ana<-(N*k_neg/(k_pos*S+k_neg)+N*(1-k_neg/(k_pos*S+k_neg))*exp(-(k_pos*S+k_neg)*t));
error[jj]<-mean(abs(R-R_ana))}

plot(step_size,error,log='xy',type='b',xlab='Time step (seconds)',ylab='Total error (M)',main='Chapter 4 Homework #2e: Error of Forward Euler', sub='Megan Liszewski')

slope=mean(diff(error)/diff(step_size))
cat('The slope of the Forward Euler error curve is',slope,'M/second.
')


#/--#########################################################################
# name: Megan Liszewski
# assignment: Chapter 4
# date: 11/20/09
# question: 2
# subquestion: f
# other files: Liszewski_Chap4_2f_plot.pdf
##########################################################################/--

#To model by backwards Euler, solve:
# R[i] + step*f(R[i+1]) = R[i+1]
# R[i] + step*(k_neg*N - (k_pos*S + k_neg)*R[i+1]) = R[i+1]
# R[i] + step*k_neg*N = R[i+1]*(1+ step*(k_pos*S + k_neg))

#Therefore:
# R[i+1] = (R[i] + step*k_neg*N) / (1+step*(k_pos*S + k_neg))


quartz()

f <- function(R,t) {
	return ( -(k_pos*S + k_neg)*R + k_neg*N);
}
backwardEuler <- function(f, max_iter, step, R0, t0) {
	Rvals <- tvals <- numeric(length=(max_iter + 1));
	Rvals[1] <- R0;
	tvals[1] <- t0;
	for (ii in 1:max_iter) {
		Rvals[ii+1] <- (Rvals[ii]+step*k_neg*N)/(1+step*(k_pos*S+k_neg))
		
		tvals[ii+1] <- tvals[ii] + step
	}
	return(data.frame(t=tvals, R=Rvals))
	
}
S<-10^(-6)
k_pos <-10^(-3)
k_neg<-10^(-5)
R0<-10^(-2)
N<-10^(-2)

step_size<-100
nSteps <- 10^(6)/step_size
beuler1 <- backwardEuler(f, nSteps, step_size, R0, 0);
t<-beuler1[1]
t<-t[,1]
R<-beuler1[2]
R<-R[,1]
plot(t,R,type='l',main='Chapter 4 Homework #2f: Backward Euler Chemical Reaction Model',ylab= 'Concentration reactant R (M)',xlab='time (seconds)',sub='Megan Liszewski')


#/--#########################################################################
# name: Megan Liszewski
# assignment: Chapter 4
# date: 11/20/09
# question: 2
# subquestion: g
# other files: Liszewski_Chap4_2g_plot.pdf
##########################################################################/--

quartz()

f <- function(R,t) {
	return ( -(k_pos*S + k_neg)*R + k_neg*N);
}
backwardEuler <- function(f, max_iter, step, R0, t0) {
	Rvals <- tvals <- numeric(length=(max_iter + 1));
	Rvals[1] <- R0;
	tvals[1] <- t0;
	for (ii in 1:max_iter) {
		Rvals[ii+1] <- (Rvals[ii] + step*k_neg*N)/(1+step*(k_pos*S+k_neg))
		
		tvals[ii+1] <- tvals[ii] + step
	}
	return(data.frame(t=tvals, R=Rvals))
	
}
S<-10^(-6)
k_pos <-10^(-3)
k_neg<-10^(-5)
R0<-10^(-2)
N<-10^(-2)

step_size<-c(2,5,10,20,50,100,200,500,1000,2000,5000)
error<-c()

for (jj in 1:length(step_size)){
nSteps <- 10^(6)/step_size[jj]
feuler1 <- backwardEuler(f, nSteps, step_size[jj], R0, 0);
t<-feuler1[1]
t<-t[,1]
R<-feuler1[2]
R<-R[,1]

R_ana<-(N*k_neg/(k_pos*S+k_neg)+N*(1-k_neg/(k_pos*S+k_neg))*exp(-(k_pos*S+k_neg)*t));
error[jj]<-mean(abs(R-R_ana))
}

plot(step_size,error,log='xy',type='b',xlab='Time step (seconds)',ylab='Total error (M)',main='Chapter 4 Homework #2g: Error of Backward Euler', sub='Megan Liszewski')

slope=mean(diff(error)/diff(step_size))
cat('The slope of the Backward Euler error curve is',slope,'M/second.
')
