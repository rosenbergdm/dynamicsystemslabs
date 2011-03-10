#!/usr/bin/env r
# encoding: utf-8
# 
# name: Brendan Colson
# assignment: Chapter 3
# date: DATE_OF_SUBMISSION
# filename: NAME_OF_FILE
#############################################################################





#/--#########################################################################
# name: Brendan Colson
# assignment: Chapter 3
# date: DATE_OF_SUBMISSION
# question: 1
# subquestion: A
# other files:
##########################################################################/--

#See separate paper


#/--#########################################################################
# name: Brendan Colson
# assignment: Chapter 3
# date: DATE_OF_SUBMISSION
# question: 1
# subquestion: B
# other files:
##########################################################################/--

#Graph of f(N)
#The population decreases from N = 0 until N = 20 (which is the parameter U)
#The population increases from N = 20 until N = 200 (par. K)
#The population decreases past N = 200 

F_func <- function(N, r=10, U = 20, K = 200) {
  return(r * N * ((N / U) - 1) * (1 - (N / K)))
}
curve(F_func,
  -30, 210,     
  main = 'f(N)',
  xlab = 'N',
  ylab = 'f(N)',
  sub = 'Brendan Colson Question_1B Exercise_3',
  pos = 0
)


#Graph of g(N)
#The population increases until N = 200 (parameter K)

G_func <- function(N, r=10, U = 20, K = 200) {
  return(r * N ^ 2 * (1 - (N / K)))
}
curve(G_func,
  -30, 210,     
  main = 'g(N)',
  xlab = 'N',
  ylab = 'g(N)',
  sub = 'Brendan Colson Question_1B Exercise_3',
  pos = 0
)


#/--#########################################################################
# name: Brendan Colson
# assignment: Chapter 3
# date: DATE_OF_SUBMISSION
# question: 1
# subquestion: C
# other files:
##########################################################################/--

#   The difference in dynamics relies on the difference on the number of 
# parameters. f(N) has two parameters that effect the fixed points, U and K.  
# The lower parameter is the unstable point where the population does not 
# have enough individuals to survive and past which the population grows.
# The upper parameter represents carrying capacity, which is stable, with the
# population growing up until that point and decreasing back to it if 
# surpassed.
#   g(N) has one parameter, the carrying capacity.  If there is even 1 in the 
# population, the population will grow until carrying capacity, and if 
# surpassed will recede to carrying capacity. 


#/--#########################################################################
# name: Brendan Colson
# assignment: Chapter 3
# date: DATE_OF_SUBMISSION
# question: 2
# subquestion: A
# other files:
##########################################################################/--

# See other paper


#/--#########################################################################
# name: Brendan Colson
# assignment: Chapter 3
# date: DATE_OF_SUBMISSION
# question: 2
# subquestion: B
# other files:
##########################################################################/--

# See other paper 


#/--#########################################################################
# name: Brendan Colson
# assignment: Chapter 3
# date: 
# question: 2
# subquestion: c 
# other files:
##########################################################################/-

f <- function(x, t, r=1) {
  return(r * x  - sin(2 * t / pi))
}

## Parameters

tRange <- c(0, 15)
dt <- .5
xRange <- c(-25, 25)
dx <- 2
h <- 0.75
k <- 1.25
tSteps <- diff(range(tRange)) / dt
xSteps <- diff(range(xRange)) / dx

## Plot

plot(c(-2, 10), c(-15, 15), type='n',
  main = 'bacteria growth',
  xlab = 't',
  ylab = 'N',
  yaxs = 'i')

## Loop

for (ii in 0: (tSteps-1)) {
  for(jj in 0: (xSteps-1)) {
    t <- ii * dt - 2
    x <- jj * dx -15
    m <- f(x, t) ;
    u <- t + (h * dt)
    v <- x + (k * h * dt) * m
    arrows(t, x, u, v, length = 0.1)
}
}

#Fixed points are periodic sin function N(t) = (1/r)sin(2t/pi).  In this 
#case, N(t) = sin(2t/pi), and thus will be situated between N = 1 and -1. 
#This portion of the direction field points away from the fixed points, and 
#thus they are all unstable.

#/--#########################################################################
# name: Brendan Colson
# assignment: Chapter 3
# date: DATE_OF_SUBMISSION
# question: 2
# subquestion: D 
# other files:
##########################################################################/--

#See other page for calculations

solution_2a <- function(t, N, C = 0) {
  return(((pi ^ 2 * sin(2 * t / pi) - 2 * pi * cos(2 * t / pi)) / (-pi ^ 2 
   + 4)) + C)
}

solution_2b <- function(t, N, C = 1) {
  return(((pi ^ 2 * sin(2 * t / pi) - 2 * pi * cos(2 * t / pi)) / (-pi ^ 2 
   + 4)) + C)
}

solution_2c <- function(t, N, C = 2) {
  return(((pi ^ 2 * sin(2 * t / pi) - 2 * pi * cos(2 * t / pi)) / (-pi ^ 2 
   + 4)) + C)
}

#choose one

curve(solution_2a, -1, 10, type='l')
abline(h=0, col='black', lwd=1)

curve(solution_2b, -1, 10, type='l')
abline(h=0, col='black', lwd=1)

curve(solution_2c, -1, 10, type='l')
abline(h=0, col='black', lwd=1)

## As the solution curves remain periodic, it is seen that there is no stable
## fixed point for N.


#/--#########################################################################
# name: Brendan Colson
# assignment: Chapter 3
# date: DATE_OF_SUBMISSION
# question: 3
# subquestion: A 
# other files:
##########################################################################/--

#See Paper


#/--#########################################################################
# name: Brendan Colson
# assignment: Chapter 3
# date: DATE_OF_SUBMISSION
# question: 3
# subquestion: B 
# other files:
##########################################################################/--


f <- function(x, t, N = 1, A = 58.1, K = 100, P = -69.6) {
  return(- N * (x - A) - K * (x - P))
}
tRange <- c(-2, 50)
dt <- 5
xRange <- c(-100, 10)
dx <- 10
h <- 0.05
k <- 0.01
tSteps <- diff(range(tRange)) / dt
VSteps <- diff(range(xRange)) / dx
plot(c(-2, 50), c(-100, 10), type='n',
  main = 'membrane potential',
  xlab = 't',
  ylab = 'V',
  yaxs = 'i')
for (ii in 0: (tSteps-1)) {
  for(jj in 0: (VSteps-1)) {
    t <- ii * dt - 2
    x <- jj * dx - 100
    m <- f(x, t) ;
    u <- t + (h * dt)
    v <- x + (k * h * dt) * m
    arrows(t, x, u, v, length = 0.07)
}
}

#The following represents the fixed point

abline(h = - 68.3, col = 'blue', lwd = 1)

##   This does agree with the fixed point analysis.  The analysis predicted a  
## single fixed point for the situation.  This fixed point can be represented
## by plugging in the constant values to the fixed point equation, yielding
## V = -68.3 mV.  This is where the arrows in the slope field converge to.


#/--#########################################################################
# name: Brendan Colson
# assignment: Chapter 3
# date: DATE_OF_SUBMISSION
# question: 3
# subquestion: C 
# other files:
##########################################################################/--

# See paper for analytic solution

## This model predicts that the value x will be -76.0, with no time variable
##  I believe the model is incorrect.
## The analytic solution did not need to be found, because we knew that the
## stable fixed point was V = -68.3 mV, and thus the long term behavior tends
## toward this fixed point.

#/--#########################################################################
# name: Brendan Colson
# assignment: Chapter 3
# date: DATE_OF_SUBMISSION
# question: 4
# subquestion: 
# other files:
##########################################################################/--

#  define function F(x: REAL, t: REAL)
#    draw curve(F(x,t))
#      draw arrows [(x1, y1, x2, y2)]
#        repeat 'draw [(x1, y1, x2, y2)]' from each x where F(x) = 0 
#  



#/--#########################################################################
# name: Brendan Colson
# assignment: Chapter 3
# date: DATE_OF_SUBMISSION
# question: 4
# subquestion: B
# other files:
##########################################################################/--

flow_fun <- function(x) {
  return((x / 3) * (1 - (x / 5)))
}

curve(flow_fun, from=-5, to=10, col='blue', main='4B Flow Lines',
xlab='t', fg=gray(0.6),
ylab= 'f(x)', bty='n');

arrows(c(0, 0, 10), c(0.1, -0.1, -0.1), c(5, -5, 5), c(0.1, -0.1, -0.1), 
col='green', lwd=2, length=0.05)
    
abline(h=0, col='black', lwd=1);
abline(v=0, col='black', lwd=1);


