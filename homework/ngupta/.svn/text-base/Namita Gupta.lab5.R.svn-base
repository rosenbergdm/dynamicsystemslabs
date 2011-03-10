#!/usr/bin/env r
# encoding: utf-8
# 
# name: Namita Gupta
# assignment: 5
# date: November 13 2009
# filename: Namita Gupta.lab5.R
#
#############################################################################

#/--#########################################################################
# name: Namita Gupta
# assignment: 5
# date: November 13 2009
# question: 1
# subquestion: c
# other files: NAMES_OF_OTHER_RELATED_FILES
##########################################################################/--

forwardEuler <- function(f, max_iter, step, x0, t0) {
  xvals <- tvals <- numeric(length=(max_iter+1));
  xvals[1] <- x0;
  tvals[1] <- t0;
  for (j in 1:max_iter) {
    xvals[j+1] <- xvals[j] + step *
                             do.call(f,list(x=xvals[j], t=tvals[j]));
    tvals[j+1] <- tvals[j] + step;
  }
  return(data.frame(t=tvals, x=xvals));
}

inf1 <- function(x,t) {
  return(.3*(1-x)*x-.1*x);
}

#/--#########################################################################
# name: Namita Gupta
# assignment: 5
# date: November 13 2009
# question: 1
# subquestion: c
# other files: NAMES_OF_OTHER_RELATED_FILES
##########################################################################/--

inf2 <- function(x,t) {
  return(.1*(1-x)*x-.2*x);
}

#/--#########################################################################
# name: Namita Gupta
# assignment: 5
# date: November 13 2009
# question: 2
# subquestion: d
# other files: NAMES_OF_OTHER_RELATED_FILES
##########################################################################/--

#R' = -RS(k_+) + (k_-)*(N-R)
complex <- function(x,t) {
  return(-.001*x*.000001+.00001*(.0001-x));
}

R <- function(t) {
  return(1/10001 + (1/100-1/10001)*exp(-10001*t/10^9));
}

#forwardApprox <- forwardEuler(complex, 100, 10^-3, 10^-2, 0)
#forwardApprox$x[length(forwardApprox$x)]

#/--#########################################################################
# name: Namita Gupta
# assignment: 5
# date: November 13 2009
# question: 2
# subquestion: e
# other files: NAMES_OF_OTHER_RELATED_FILES
##########################################################################/--

forwarderror <- function(max) {
  x_vector <- y_vector <- numeric(max+4);
  for(j in -3:max) {
    x_vector[j+4] <- log(10^j);
    approx <- forwardEuler(complex, 100, 10^j, 10^-2, 0);
    y_vector[j+4] <- log(abs(do.call(R,list(t=10^(j+2)))-
                            approx$x[length(approx$x)])/10^(j+2));
  }
  plot(x_vector, y_vector, xlab='log(time-step)', ylab='log(total-error)',
       main='Forward Euler log-log Error')
}

#forwarderror(3)

#/--#########################################################################
# name: Namita Gupta
# assignment: 5
# date: November 13 2009
# question: 2
# subquestion: f
# other files: NAMES_OF_OTHER_RELATED_FILES
##########################################################################/--

backwardEuler <- function(f, max_iter, step, x0, t0) {
  xvals <- tvals <- numeric(length=(max_iter+1));
  xvals[1] <- x0;
  tvals[1] <- t0;
  for (j in 2:max_iter) {
    tvals[j] <- tvals[j-1] + step;
    xvals[j] <- (step + 10^9*xvals[j-1])/(10^9 + 10001*step);
  }
  return(data.frame(t=tvals, x=xvals));
}

#backwardApprox <- backwardEuler(complex, 100, 10^3, 10^-2, 0)
#backwardApprox$x[length(backwardApprox$x)]


#/--#########################################################################
# name: Namita Gupta
# assignment: 5
# date: November 13 2009
# question: 2
# subquestion: g
# other files: NAMES_OF_OTHER_RELATED_FILES
##########################################################################/--

backwarderror <- function(max) {
  x_vector <- y_vector <- numeric(max+4);
  for(j in -3:max) {
    x_vector[j+4] <- log(10^j);
    approx <- backwardEuler(complex, 100, 10^j, 10^-2, 0);
    y_vector[j+4] <- log(abs(do.call(R,list(t=10^(j+2)))-
                            approx$x[length(approx$x)])/10^(j+2));
  }
  plot(x_vector, y_vector, xlab='log(time-step)', ylab='log(total-error)',
       main='Backward Euler log-log Error')
}


