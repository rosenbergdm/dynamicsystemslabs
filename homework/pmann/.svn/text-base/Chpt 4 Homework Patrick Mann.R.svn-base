#!/usr/bin/env r
# encoding: utf-8
# 
# name: Patrick Mann
# assignment: 5
# date: 11-18-09
# filename: Chapter 4 Exercises
#############################################################################

#/--#########################################################################
# name: Patrick Mann
# assignment: 5
# date: 11-18-09
# question: 1
# subquestion: c
# other files: NAMES_OF_OTHER_RELATED_FILES
##########################################################################/--

f_I <- function(I, N, t, beta, gamma) {
  return ( ( (beta * N - I) * I )- (gamma * I) )
}

forwardEuler <- function(f, max_iter, step, I0, t0) {
  Ivals <- tvals <- numeric(length=(max_iter + 1));
  Ivals[1] <- I0;
  tvals[1] <- t0
  for (ii in 1:max_iter) {
  	Ivals[ii + 1] <- Ivals[ii] + step *
  	  f(Ivals[ii], N, tvals[ii], beta, gamma);
  	tvals[ii + 1] <- tvals[ii] + step
  }
  return(plot(tvals, Ivals, type='l', 
    main='Infection Epidemiology Model'))
}

N <- 1
beta <- 0.3
gamma <- 0.1
forwardEuler(f_I, 365, 0.1, 0.01, 0)
dev.new()
forwardEuler(f_I, 365, 10, 0.01, 0)
dev.new()
forwardEuler(f_I, 365, 5.5, 0.01, 0)
dev.new()
forwardEuler(f_I, 365, 5.6, 0.01, 0)
dev.new()
forwardEuler(f_I, 365, 9.9, 0.01, 0)
dev.new()
forwardEuler(f_I, 365, 10, 0.01, 0)

##The graph with a step equal to 0.1 appears to be a smooth curve that resembles an 's' and starts to flatten out at f(I) = 0.20 as the number of time steps increase.  The graph with a step equal to 10 oscillates with a diminishing amplitude that hovers around f(I) = 0.20.  It looks like the oscillations begin appearing between step sizes 5.5 and 5.6.  Between step sizes 9.9 and 10, the oscillatons stop diminishing to a consistant flat-line value and instead apparently continue oscillating into infinity.

#/--#########################################################################
# name: Patrick Mann
# assignment: 5
# date: 11-18-09
# question: 1
# subquestion: d
# other files: NAMES_OF_OTHER_RELATED_FILES
##########################################################################/--

f_I <- function(I, N, t, beta, gamma) {
  return ( ( (beta * N - I) * I )- (gamma * I) )
}

forwardEuler <- function(f, max_iter, step, I0, t0) {
  Ivals <- tvals <- numeric(length=(max_iter + 1));
  Ivals[1] <- I0;
  tvals[1] <- t0
  for (ii in 1:max_iter) {
  	Ivals[ii + 1] <- Ivals[ii] + step *
  	  f(Ivals[ii], N, tvals[ii], beta, gamma);
  	tvals[ii + 1] <- tvals[ii] + step
  }
  return(plot(tvals, Ivals, type='l', 
    main='Infection Epidemiology Model'))
}

N <- 1
beta <- 0.1
gamma <- 0.2
forwardEuler(f_I, 365, 0.1, 0.01, 0)
dev.new()
forwardEuler(f_I, 365, 10, 0.01, 0)
dev.new()
forwardEuler(f_I, 365, 9.1, 0.01, 0)
dev.new()
forwardEuler(f_I, 365, 9.2, 0.01, 0)

##The graph doesn't visably osillate until sometime between a time step size of 9.1 and 9.2.  At a time step size somewhere between sizes 19.9 and 20, the oscillations stop diminishing in amplitude until the graph is a flat line and instead appear to keep oscillating into infinity.

#/--#########################################################################
# name: Patrick Mann
# assignment: 5
# date: 11-18-09
# question: 2
# subquestion: d
# other files: NAMES_OF_OTHER_RELATED_FILES
##########################################################################/--

f_R <- function(R, S, k_1, k_2, R_0, t) {
  return ( R_0*k_2 - R*(k_1*S + k_2) )
}
forwardEuler <- function(f, max_iter, step, R_0, t0) {
  Rvals <- tvals <- numeric(length=(max_iter + 1));
  Rvals[1] <- R_0;
  tvals[1] <- t0
  for (ii in 1:max_iter) {
  	Rvals[ii + 1] <- Rvals[ii] + step *
  	  f(Rvals[ii], S, k_1, k_2, R_0, tvals[ii]);
  	tvals[ii + 1] <- tvals[ii] + step
  }
  return(plot(tvals, Rvals, type='l', 
    main='Receptor Concentration Model'))
}

S <- 10^(-6)
k_1 <- 10^(-3)
k_2 <- 10^(-5)
R_0 <- 10^(-2)

forwardEuler(f_R, 1000, 0.001, R_0, 0)

#/--#########################################################################
# name: Patrick Mann
# assignment: 5
# date: 11-18-09
# question: 2
# subquestion: e
# other files: NAMES_OF_OTHER_RELATED_FILES
##########################################################################/--

euler_number <- 2.7182818284

S <- 10^(-6)
k_1 <- 10^(-3)
k_2 <- 10^(-5)
R_0 <- 10^(-2)
step <- c(10, 100, 500, 1000, 2000, 5000, 1e4, 1e5, 1e6, 1e7)
tRange <- c(0, 1e7)

f_R_Solved <- function(S, k_1, k_2, R_0, t) {
  return( ( (R_0*k_2) / (k_1*S + k_2) )*
    (1 - euler_number^( (-k_1*S - k_2)*t) ) +
    R_0*(euler_number^( (-k_1*S - k_2)*t) )
  )
}

f_R <- function(R, S, k_1, k_2, R_0, t) {
  return ( R_0*k_2 - R*(k_1*S + k_2) )
}

forwardEuler <- function(f, tRange, step, R_0, t0) {
  Rvals <- tvals <- seq(tRange[1], tRange[2], by=step);
  Rvals[1] <- R_0;
  tvals[1] <- t0
  for (ii in 1:length(tvals)) {
  	Rvals[ii + 1] <- Rvals[ii] + step *
  	  f(Rvals[ii], S, k_1, k_2, R_0, tvals[ii]);
  	tvals[ii + 1] <- tvals[ii] + step
  }
  return(list(r=Rvals, t=tvals));
}


errors <- step

for ( ii in 1:length(step) ) {
  fEst <- forwardEuler(f_R, tRange, step[ii], R_0, 0);
  actual <- f_R_Solved(S, k_1, k_2, R_0, t=fEst$t);
  meanError <- mean(abs(fEst$r - actual));
  errors[ii] <- meanError;
}

slope <- mean(diff(log(errors)) / diff(log(step)))
slope

plot(step, errors, log='xy', type='b',
  main='Average Error Between Solution and Forward Euler',
  xlab='Time Step Size', ylab='Average Error'
)

#/--#########################################################################
# name: Patrick Mann
# assignment: 5
# date: 11-18-09
# question: 2
# subquestion: f
# other files: NAMES_OF_OTHER_RELATED_FILES
##########################################################################/--

f_R_Bkw <- function(R, S, k_1, k_2, R_0, t, step) {
  return( (R_0*k_2*step)/(1 + (k_1*S + k_2)*step) )
}

backwardEuler <- function(f, max_iter, step, R_0, t0) {
  Rvals <- tvals <- numeric(length=(max_iter + 1));
  Rvals[1] <- R_0;
  tvals[1] <- t0
  for (ii in 1:max_iter) {
  	Rvals[ii + 1] <- Rvals[ii]/(1 + (k_1*S + k_2)*step) +
  	  f(Rvals[ii], S, k_1, k_2, R_0, tvals[ii], step);
  	tvals[ii + 1] <- tvals[ii] + step
  }
  return(plot(tvals, Rvals, type='l', 
    main='Receptor Concentration Model'))
}

S <- 10^(-6)
k_1 <- 10^(-3)
k_2 <- 10^(-5)
R_0 <- 10^(-2)

backwardEuler(f_R_Bkw, 1000, 0.001, R_0, 0)

#/--#########################################################################
# name: Patrick Mann
# assignment: 5
# date: 11-18-09
# question: 2
# subquestion: g
# other files: NAMES_OF_OTHER_RELATED_FILES
##########################################################################/--

euler_number <- 2.7182818284

S <- 10^(-6)
k_1 <- 10^(-3)
k_2 <- 10^(-5)
R_0 <- 10^(-2)
step <- c(10, 100, 500, 1000, 2000, 5000, 1e4, 1e5, 1e6, 1e7)
tRange <- c(0, 1e7)

f_R_Solved <- function(S, k_1, k_2, R_0, t) {
  return( ( (R_0*k_2) / (k_1*S + k_2) )*
    (1 - euler_number^( (-k_1*S - k_2)*t) ) +
    R_0*(euler_number^( (-k_1*S - k_2)*t) )
  )
}

f_R_Bkw <- function(R, S, k_1, k_2, R_0, t, step) {
  return( (R_0*k_2*step)/(1 + (k_1*S + k_2)*step) )
}

backwardEuler <- function(f, max_iter, step, R_0, t0) {
  Rvals <- tvals <- seq(tRange[1], tRange[2], by=step);
  Rvals[1] <- R_0;
  tvals[1] <- t0
  for (ii in 1:length(tvals)) {
  	Rvals[ii + 1] <- Rvals[ii]/(1 + (k_1*S + k_2)*step) +
  	  f(Rvals[ii], S, k_1, k_2, R_0, tvals[ii], step);
  	tvals[ii + 1] <- tvals[ii] + step
  }
  return(r=Rvals, t=tvals)
}

errors <- step

for ( ii in 1:length(step) ) {
  bEst <- backwardEuler(f_R_Bkw, tRange, step[ii], R_0, 0);
  actual <- f_R_Solved(S, k_1, k_2, R_0, t=bEst$t);
  meanError <- mean(abs(bEst$r - actual));
  errors[ii] <- meanError;
}

slope <- mean(diff(log(errors)) / diff(log(step)))
slope


plot(step, errors, log='xy', type='b',
  main='Average Error Between Solution and Backward Euler',
  xlab='Time Step Size', ylab='Average Error'
)
