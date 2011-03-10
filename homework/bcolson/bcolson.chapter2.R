#!/usr/bin/env r
# encoding: utf-8
# 
# name: BRENDAN COLSON
# assignment: 3
# date: 10/22/09
# filename: EXERCISE 3
# subquestion:
# other files: 
##########################################################################/--






#/--#########################################################################
# name: BRENDAN COLSON
# assignment: 3
# date: 10/22/09
# question: 1
# subquestion: A
# other files: 
##########################################################################/--

nFun <- function(x) { 
  return(41 * x - (10 * x ^2))
}
max_iter <- 100;
y_vector <- x_vector <- numeric(
  max_iter * 2);
y_vector[1] <- 0;
x_vector[1] <- 0.5;
for(ii in 1:100) {
  y_vector[2 * ii] <- nFun(
    x_vector[2 * ii - 1]);
  y_vector[2 * ii + 1] <-
    y_vector[2 * ii];
  x_vector[2 * ii] <-
    x_vector[2 * ii - 1];
  x_vector[2 * ii + 1] <-
    y_vector[2 * ii + 1];
}
plot( -10:100 / 10,
	nFun(-10:100 /10),
	xlab='N',
	ylab='newN',
	main='Population a',
	type='l',
	xlim=c(-2, 20),
	ylim=c(0, 50),
	xaxs='i',
	yaxs='i');
lines(-2:20, -2:20, col='red');
lines(x_vector, y_vector, col='blue');

#There is an equilibrium at N = 4, but it is unstable


#/--#########################################################################
# name: BRENDAN COLSON
# assignment: 3
# date: 10/22/09
# question: 1
# subquestion: B
# other files: 
##########################################################################/--

nFun <- function(x) { 
  return(41 * x + (2 * x ^2))
}
max_iter <- 100;
y_vector <- x_vector <- numeric(
  max_iter * 2);
y_vector[1] <- 0;
x_vector[1] <- 0.5;
for(ii in 1:100) {
  y_vector[2 * ii] <- nFun(
    x_vector[2 * ii - 1]);
  y_vector[2 * ii + 1] <-
    y_vector[2 * ii];
  x_vector[2 * ii] <-
    x_vector[2 * ii - 1];
  x_vector[2 * ii + 1] <-
    y_vector[2 * ii + 1];
}
plot( -10:1000 / 10,
	nFun(-10:1000 /10),
	xlab='N',
	ylab='newN',
	main='Population b',
	type='l',
	xlim=c(0, 100),
	ylim=c(0, 5000),
	xaxs='i',
	yaxs='i');
lines(-30:30, -30:30, col='red');
lines(x_vector, y_vector, col='blue');

#There is an equilibrium at N = -20, but it is unstable, and impossible for a 
#population

#/--#########################################################################
# name: BRENDAN COLSON
# assignment: 3
# date: 10/22/09
# question: 2
# subquestion: A
# other files: 
##########################################################################/--

nFun <- function(x) { 
  return(x * (5 - 4 * x))
}
max_iter <- 50;
y_vector <- x_vector <- numeric(
  max_iter * 2);
y_vector[1] <- 0;
x_vector[1] <- 0.1;
for(ii in 1:50) {
  y_vector[2 * ii] <- nFun(
    x_vector[2 * ii - 1]);
  y_vector[2 * ii + 1] <-
    y_vector[2 * ii];
  x_vector[2 * ii] <-
    x_vector[2 * ii - 1];
  x_vector[2 * ii + 1] <-
    y_vector[2 * ii + 1];
}
plot( -10:100 / 10,
	nFun(-10:100 /10),
	xlab='N',
	ylab='newN',
	main='2a',
	type='l',
	xlim=c(-0.5, 1.5),
	ylim=c(0, 2),
	xaxs='i',
	yaxs='i');
lines(-2:20, -2:20, col='red');
lines(x_vector, y_vector, col='blue');

#Fixed Point:
# x=0 and 
# (r-1)/k
# 5-1/4 = 1
# x=1
# x=0 is an unstable fixed point, as f'(0) > 0
# x=1 is stable, as f'(1) < 0
 
 

#/--#########################################################################
# name: BRENDAN COLSON
# assignment: 3
# date: 10/22/09
# question: 2
# subquestion: B
# other files: 
##########################################################################/--

nFun <- function(x) { 
  return(2 * x * (1 - (3 * x / 2)))
}
max_iter <- 1000;
y_vector <- x_vector <- numeric(
  max_iter * 2);
y_vector[1] <- 0;
x_vector[1] <- 0.1;
for(ii in 1:1000) {
  y_vector[2 * ii] <- nFun(
    x_vector[2 * ii - 1]);
  y_vector[2 * ii + 1] <-
    y_vector[2 * ii];
  x_vector[2 * ii] <-
    x_vector[2 * ii - 1];
  x_vector[2 * ii + 1] <-
    y_vector[2 * ii + 1];
}
plot( -10:1000 / 10,
	nFun(-10:1000 /10),
	xlab='N',
	ylab='newN',
	main='2b',
	type='l',
	xlim=c(-.2, 1),
	ylim=c(0, 0.5),
	xaxs='i',
	yaxs='i');
lines(-2:100, -2:100, col='red');
lines(x_vector, y_vector, col='blue');

#Fixed Points:
# x = 0 and x = (1/3)
# x = 0 is unstable, as f'(0) > 0
# x = (1/3) is f'(1/3) = 0, more info is necessary
#/--#########################################################################
# name: BRENDAN COLSON
# assignment: 3
# date: 10/22/09
# question: 2
# subquestion: C
# other files: 
##########################################################################/--

nFun <- function(x) { 
  return((x / 2) - (x^2 / 5))
}
max_iter <- 1000;
y_vector <- x_vector <- numeric(
  max_iter * 2);
y_vector[1] <- 0;
x_vector[1] <- 0.5;
for(ii in 1:1000) {
  y_vector[2 * ii] <- nFun(
    x_vector[2 * ii - 1]);
  y_vector[2 * ii + 1] <-
    y_vector[2 * ii];
  x_vector[2 * ii] <-
    x_vector[2 * ii - 1];
  x_vector[2 * ii + 1] <-
    y_vector[2 * ii + 1];
}
plot( -10:1000 / 10,
	nFun(-10:1000 /10),
	xlab='N',
	ylab='newN',
	main='2b',
	type='l',
	xlim=c(-0.5, 3),
	ylim=c(0, 0.5),
	xaxs='i',
	yaxs='i');
lines(-2:100, -2:100, col='red');
lines(x_vector, y_vector, col='blue');

#Fixed Points:
#  x = 0 and x = (- 5/2)
#    f'(0) > 0, unstable
#    f'(-5/2) < 0, stable

#/--#########################################################################
# name: BRENDAN COLSON
# assignment: 3
# date: 10/22/09
# question: 3
# subquestion:  
# other files: 
##########################################################################/--

nFun <- function(x) { 
  return(x * ((5/2) - 7 * x))
}
max_iter <- 1000;
y_vector <- x_vector <- numeric(
  max_iter * 2);
y_vector[1] <- 0;
x_vector[1] <- 0.1;
cobweb <- function(vector) {
for(ii in 1:1000) {
  y_vector[2 * ii] <- nFun(
    x_vector[2 * ii - 1]);
  y_vector[2 * ii + 1] <-
    y_vector[2 * ii];
  x_vector[2 * ii] <-
    x_vector[2 * ii - 1];
  x_vector[2 * ii + 1] <-
    y_vector[2 * ii + 1];
}
plot( -10:1000 / 10,
	nFun(-10:1000 /10),
	xlab='N',
	ylab='newN',
	main='3',
	type='l',
	xlim=c(-0.1, 0.5),
	ylim=c(0, 0.5),
	xaxs='i',
	yaxs='i');
lines(-2:100, -2:100, col='red');
lines(x_vector, y_vector, col='blue')
}
cobweb(nFun)

#/--#########################################################################
# name: BRENDAN COLSON
# assignment: 3
# date: 10/22/09
# question: 4
# subquestion: A 
# other files: 
##########################################################################/--

#f(x) = -k[G(t)]
#This is a homogenous and autonomous ODE 

#/--#########################################################################
# name: BRENDAN COLSON
# assignment: 3
# date: 10/22/09
# question: 4
# subquestion: B 
# other files: 
##########################################################################/--

#The solution is found by integrating both sides of the equation
#  int((dx/dt)*dt) = int(rxdt)
#Divide by x
#  int(dx/x) = int(rdt)
#  log|x| = rt + C
#  x = Ce^(rt)
#Since the initial condition is Go, and k = r
#  x = Go*e^(-kt)

#/--#########################################################################
# name: BRENDAN COLSON
# assignment: 3
# date: 10/22/09
# question: 4
# subquestion: C
# other files: 
##########################################################################/--

nFun <- function(x) { 
  return((100 * exp((-0.01) * x)))
}
max_iter <- 1000;
y_vector <- x_vector <- numeric(
  max_iter * 2);
y_vector[1] <- 0;
x_vector[1] <- 0.01;
cobweb <- function(vector) {
for(ii in 1:1000) {
  y_vector[2 * ii] <- nFun(
    x_vector[2 * ii - 1]);
  y_vector[2 * ii + 1] <-
    y_vector[2 * ii];
  x_vector[2 * ii] <-
    x_vector[2 * ii - 1];
  x_vector[2 * ii + 1] <-
    y_vector[2 * ii + 1];
}
plot( -10:10000 / 10,
	nFun(-10:10000 /10),
	xlab='t',
	ylab='new[G]',
	main='Glucose Concentration [G]',
	type='l',
	xlim=c(0, 500),
	ylim=c(0, 125),
	xaxs='i',
	yaxs='i');
lines(-2:105, -2:105, col='red');
lines(x_vector, y_vector, col='blue')
}
cobweb(nFun)

#/--#########################################################################
# name: BRENDAN COLSON
# assignment: 3
# date: 10/22/09
# question: 4
# subquestion: D
# other files: 
##########################################################################/--


#There is a fixed point at [G(t)] = 0 and [G(t)] = (r - 1) / k = 100 mg/dl   
#This is a stable fixed point.


#/--#########################################################################
# name: BRENDAN COLSON
# assignment: 3
# date: 10/22/09
# question: 5
# subquestion: A 
# other files: 
##########################################################################/--


#f(x) = a(t) - k[G(t)]
#This is an autonomous and inhomogeneous equation 


#/--#########################################################################
# name: BRENDAN COLSON
# assignment: 3
# date: 10/22/09
# question: 5
# subquestion: B 
# other files: 
##########################################################################/--


#The solution is found by the general expression
# x(t) = e ^ int(a(t)dt) * int(b(t) * e ^ -int(a(t)dt) * dt + C * e ^ int(a(t)dt)
#So 
#  x(t) = e ^ ((1/2)* k * t ^ 2) * int(a(t) * e ^ -((1/2)* k * t ^ 2))dt)dt +         G(0) * e^((1/2) * k * t ^ 2)   



#/--#########################################################################
# name: BRENDAN COLSON
# assignment: 3
# date: 10/22/09
# question: 5
# subquestion: C 
# other files: 
##########################################################################/--

#The solution I acquired in the last problem is likely incorrect, and will not be
#able to be plotted as written...

nFun <- function(x) { 
  return( )
}
max_iter <- 1000;
y_vector <- x_vector <- numeric(
  max_iter * 2);
y_vector[1] <- 0;
x_vector[1] <- 0.01;
cobweb <- function(vector) {
for(ii in 1:1000) {
  y_vector[2 * ii] <- nFun(
    x_vector[2 * ii - 1]);
  y_vector[2 * ii + 1] <-
    y_vector[2 * ii];
  x_vector[2 * ii] <-
    x_vector[2 * ii - 1];
  x_vector[2 * ii + 1] <-
    y_vector[2 * ii + 1];
}
plot( -10:10000 / 10,
	nFun(-10:10000 /10),
	xlab='t',
	ylab='new[G]',	
	main='Glucose Concentration [G]',
	type='l',
	xlim=c(0, 500),
	ylim=c(0, 125),
	xaxs='i',
	yaxs='i');
lines(-2:105, -2:105, col='red');
lines(x_vector, y_vector, col='blue')
}
cobweb(nFun)

# x(t) = e ^ ((1/200) * t ^ 2) * 4 * int(e ^ -(((1/200) * t ^ 2))dt) + 
# 100 * e^((1/200) * t ^ 2)   


#/--#########################################################################
# name: BRENDAN COLSON
# assignment: 3
# date: 10/22/09
# question: 5
# subquestion: D
# other files: 
##########################################################################/--


#Equilibrium concentrations at G(t) = 0 and G(t) = (r - 1) / k = 300 mg/dl
