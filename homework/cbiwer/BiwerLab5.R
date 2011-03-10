#!/usr/bin/env r
# encoding: utf-8
# 
# name: Craig Biwer
# assignment: 5
# date: 11/13/2009
# filename: BiwerLab5.R
#############################################################################







#/--#########################################################################
# name: Craig Biwer
# assignment: 5
# date: 11/13/2009
# question: 1
# subquestion: a
# other files: 
##########################################################################/--

#Fixed points occur at I = 0 (no infection, so no spread) and at
#I = N - gamma/beta. This second point is relevant for N > gamma/beta, and
#gives a value at which the S/I populations will be constant. The values
#of the parameters obviously effect the ratio of S/I individuals.



#/--#########################################################################
# name: Craig Biwer
# assignment: 5
# date: 11/13/2009
# question: 1
# subquestion: b
# other files: 
##########################################################################/--

#For I = 0, the point is stable for beta*N - gamma < 0. 
#For I = N - gamma/beta, the point is stable for gamma - beta*N < 0.
#This shows that only one of the two points is ever stable, which makes
#sense as there is no third point in the middle. This shows that given a 
#relationship between N, gamma, and beta, we can determine which point
#will be stable.



#/--#########################################################################
# name: Craig Biwer
# assignment: 5
# date: 11/13/2009
# question: 1
# subquestion: c
# other files: 
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

case1 <- function(x, t) {
    return(0.3*(1 - x)*x - 0.1*x)
}

#forwardEuler(case1, 3650, 0.1, 0.01, 0)
#forwardEuler(case1, 365, 1, 0.01, 0)
#forwardEuler(case1, 73, 5, 0.01, 0)
#forwardEuler(case1, 37, 10, 0.01, 0)
#For the first three cases, the behavior was consistent with what we
#predicted above: the population settled at 2/3, or 1 - 0.1/0.3. For 
#the fourth case, with time steps 10, the solution gradually approaches
#what we'd expect: after 5000 time steps, the value is 0.66332, accurate
#to two decimal places. This shows that while extremely slow, it still
#approaches our predicted value. In its approach, though, it constantly
#bounces above and below the target value.



#/--#########################################################################
# name: Craig Biwer
# assignment: 5
# date: 11/13/2009
# question: 1
# subquestion: d
# other files: 
##########################################################################/--

case2 <- function(x, t) {
    return(0.1*(1 - x)*x - 0.2*x)
}

#forwardEuler(case2, 3650, 0.1, 0.01, 0)
#forwardEuler(case2, 365, 1, 0.01, 0)
#forwardEuler(case2, 73, 5, 0.01, 0)
#forwardEuler(case2, 37, 10, 0.01, 0)
#forwardEuler(case2, 18, 20, 0.01, 0)
#forwardEuler(case2, 9, 40, 0.01, 0)
#For the first four cases, the values returned were effectively 0 (10^-18),
#showing our predictions once again came true (0 as the fixed point). Only
#when I set the time step to 20 days did the model break down, returning
#negative values. Note that at this time step the values alternated
#between positive and negative, but remained very close to 0. Setting the
#time step to 40 causes the negative values to grow exponentially.



#/--#########################################################################
# name: Craig Biwer
# assignment: 5
# date: 11/13/2009
# question: 2
# subquestion: a
# other files: 
##########################################################################/--

#R' = kminus * (R0 - R) - kplus * R * S
#Note: R0 is the initial concentration of R



#/--#########################################################################
# name: Craig Biwer
# assignment: 5
# date: 11/13/2009
# question: 2
# subquestion: b
# other files: 
##########################################################################/--

#There is only one fixed point, and it occurs for
#R = R0 / ( (kplus/kminus) * S + 1)
#This point is always stable, since the derivative of the right hand side
#of our ODE is -kminus - kplus * S. These numbers are always positive, 
#meaning the evaluated value is always negative, meaning the fixed point
#is always stable.



#/--#########################################################################
# name: Craig Biwer
# assignment: 5
# date: 11/13/2009
# question: 2
# subquestion: c
# other files: 
##########################################################################/--

#R[t] = A * exp((-kminus - kplus * S) * t)
#       +
#       (kminus * R0)/(kminus + kplus * S)
#where A is some constant.
#Note that the fixed point is just the constant being added:
#this makes sense in that for large t, the first time goes
#to 0, leaving only the added constant.



#/--#########################################################################
# name: Craig Biwer
# assignment: 5
# date: 11/13/2009
# question: 2
# subquestion: d
# other files: 
##########################################################################/--

case3 <- function(x, t) {
    return(
            1/10001 + (1/100-1/10001)*exp(-10001*x/10^9)
          )
}

#forwardEuler(case3, 1000, 0.0001, 0.01, 0)



#/--#########################################################################
# name: Craig Biwer
# assignment: 5
# date: 11/13/2009
# question: 2
# subquestion: e
# other files: 
##########################################################################/--

case3base <- function(x, t) {
    return(10^-5 * (10^-4 - x) - 10^-3 * x * 10^-6)
}

case3v2 <- function(x) {
    return(
            1/10001 + (1/100-1/10001)*exp(-10001*x/10^9)
          )
}

forwarderror <- function(max) {
    x_vector <- y_vector <- numeric(max+4);
    for(j in -3:max) {
      x_vector[j+4] <- log(10^j);
      approx <- forwardEuler(case3base, 100, 10^j, 10^-2, 0);
      y_vector[j+4] <- log(abs(do.call(case3v2,list(x=10^(j+2)))-
                            approx$x[length(approx$x)])/10^(j+2));
    }
    #print(x_vector);
    #print(y_vector);
    plot(x_vector, y_vector, xlab='log(time-step)', ylab='log(total-error)',
         main='Forward Euler log-log Error', sub='Craig Biwer #2e')
}

#forwarderror(3)
#This plot has a slope of 1, which means the order of our
#method is also 1


#/--#########################################################################
# name: Craig Biwer
# assignment: 5
# date: 11/S13/2009
# question: 2
# subquestion: f
# other files: 
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

#backwardEuler(case3, 3650, 0.1, 0.01, 0)
#backwardEuler(case3, 365, 1, 0.01, 0)
#backwardEuler(case3, 73, 5, 0.01, 0)
#backwardEuler(case3, 37, 10, 0.01, 0)
#backwardEuler(case3, 18, 20, 0.01, 0)
#backwardEuler(case3, 9, 40, 0.01, 0)


#/--#########################################################################
# name: Craig Biwer
# assignment: 5
# date: 11/13/2009
# question: 2
# subquestion: g
# other files: 
##########################################################################/--

backwarderror <- function(max) {
    x_vector <- y_vector <- numeric(max+4);
    for(j in -3:max) {
        x_vector[j+4] <- log(10^j);
        approx <- backwardEuler(case3base, 100, 10^j, 10^-2, 0);
        y_vector[j+4] <- log(abs(do.call(case3v2,list(x=10^(j+2)))-
                            approx$x[length(approx$x)])/10^(j+2));
    }
    #print(x_vector);
    #print(y_vector);
    plot(x_vector, y_vector, xlab='log(time-step)', ylab='log(total-error)',
         main='Backward Euler log-log Error', sub='Craig Biwer 2g')
}

#backwarderror(3)
#This plot also has a slope of -1, which means the order of our
#method is also -1