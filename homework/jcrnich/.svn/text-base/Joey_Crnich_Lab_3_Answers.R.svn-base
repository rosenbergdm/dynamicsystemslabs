#!/usr/bin/env r
# encoding: utf-8
# 
# name: Crnich, Joseph
# assignment: 3
# date: 10/23/2009
# filename: Joey_Crnich_Lab_3_Answers.R
#############################################################################


#/--#########################################################################
# name: Crnich, Joseph
# assignment: 3
# date: 10/23/2009
# question: 1
# subquestion: a
# other files: 
##########################################################################/--

## N(t+1) = N(t) • (41 - 10N(t))
## N* = N* • (41 - 10N*)		=> N* = 0
## 1 = 41 - 10N*
## 10N* = 40
## N* = 4, 	N* = 0
## Fixed points at 0 and 4.

## Function of the form: F(x) = rx - kx^2, where r = 41, and k = 10
## Thus, at F(0), function is stable if r < 1; r !< 1, therefore F(0) is
## unstable. At F(4), function is stable if 1 < r < 3; r > 3, therefore, F(4)
## is also unstable.

#/--#########################################################################
# name: Crnich, Joseph
# assignment: 3
# date: 10/23/2009
# question: 1
# subquestion: b
# other files: 
##########################################################################/--

## N(t+1) = N(t) • (41 + 2N(t))
## N* = N* • (41 + 2N*)		=> N* = 0
## 1 = 41 + 2N*
## 2N* = -40
## N* = -20,	N* = 0
## Fixed points at 0 and -20.

## Function again of the form: F(x) = rx - kx^2, where r = 41, and k = -2
## Thus, at F(0), function is stable if r < 1; r !< 1, therefore F(0) is
## unstable. At F(-20), function is stable if 1 < r < 3; r > 3, therefore,
## F(-20) is also unstable.

#/--#########################################################################
# name: Crnich, Joseph
# assignment: 3
# date: 10/23/2009
# question: 2
# subquestion: a
# other files: 
##########################################################################/--

## f(x) = x(5 - 4x)

aFun <- function(x) {
	return(x * (5 - 4 * x));
}
max_iter <- 50
y_vector <- x_vector <- numeric(max_iter * 2)
y_vector[1] <- 0
x_vector[1] <- 0.2
for(ii in 1:50)	{
	y_vector[2 * ii] <- aFun(x_vector[2 * ii - 1]);
	y_vector[2 * ii + 1] <- y_vector[2 * ii];
	x_vector[2 * ii] <- x_vector[2 * ii - 1];
	x_vector[2 * ii + 1] <- y_vector[2 * ii + 1];
}
plot(-100:500 / 100, aFun(-100:500 / 100), xlab='x(t)', ylab='x(t+1)',
    main='Cobweb Plot', type='l',
    xlim=c(0, 1.6), ylim=c(0,1.6),
    xaxs='i', yaxs='i')
abline(0, 1, col='red')
lines(x_vector, y_vector, col='blue')
points(1, 1)
points(0, 0)

## Two fixed points, 0 and 1, but neither are stable, as the blue line goes
## off towards negative infinity.

#/--#########################################################################
# name: Crnich, Joseph
# assignment: 3
# date: 10/23/2009
# question: 2
# subquestion: b
# other files: 
##########################################################################/--

## f(x) = 2x(1 -3x/2)

bFun <- function(x) {
	return(2 * x * (1 - 3 * x / 2));
}
max_iter <- 50
y_vector <- x_vector <- numeric(max_iter * 2)
y_vector[1] <- 0
x_vector[1] <- 0.15
for(ii in 1:50)	{
	y_vector[2 * ii] <- bFun(x_vector[2 * ii - 1]);
	y_vector[2 * ii + 1] <- y_vector[2 * ii];
	x_vector[2 * ii] <- x_vector[2 * ii - 1];
	x_vector[2 * ii + 1] <- y_vector[2 * ii + 1];
}
plot(-100:500 / 100, bFun(-100:500 / 100), xlab='x(t)', ylab='x(t+1)',
    main='Cobweb Plot', type='l',
    xlim=c(0, 0.6), ylim=c(0,0.5),
    xaxs='i', yaxs='i')
abline(0, 1, col='red')
lines(x_vector, y_vector, col='blue')
points(0, 0)
points(x_vector[50], y_vector[50])

## Fixed points are 0 and 1/3. As the blue line approaches 1/3, it is stable.

#/--#########################################################################
# name: Crnich, Joseph
# assignment: 3
# date: 10/23/2009
# question: 2
# subquestion: c
# other files: 
##########################################################################/--

## f(x) = x/2 - (x^2)/5

cFun <- function(x) {
	return(x / 2 - x^2 / 5);
}
max_iter <- 50
y_vector <- x_vector <- numeric(max_iter * 2)
y_vector[1] <- 0
x_vector[1] <- 2
for(ii in 1:50)	{
	y_vector[2 * ii] <- cFun(x_vector[2 * ii - 1]);
	y_vector[2 * ii + 1] <- y_vector[2 * ii];
	x_vector[2 * ii] <- x_vector[2 * ii - 1];
	x_vector[2 * ii + 1] <- y_vector[2 * ii + 1];
}
plot(-100:500 / 100, cFun(-100:500 / 100), xlab='x(t)', ylab='x(t+1)',
    main='Cobweb Plot', type='l',
    xlim=c(0, 3), ylim=c(0, .5),
    xaxs='i', yaxs='i')
abline(0, 1, col='red')
lines(x_vector, y_vector, col='blue')
points(0, 0)

## Zero is the only non-negative point on the line y=x. As the blue line
## approaches it, zero is also stable.

#/--#########################################################################
# name: Crnich, Joseph
# assignment: 3
# date: 10/23/2009
# question: 2
# subquestion: d
# other files: 
##########################################################################/--

## f(x) = x(5/2 - 7x)

dFun <- function(x) {
	return(x * (5 / 2 - 7 * x));
}
max_iter <- 50
y_vector <- x_vector <- numeric(max_iter * 2)
y_vector[1] <- 0
x_vector[1] <- 0.15
for(ii in 1:50)	{
	y_vector[2 * ii] <- dFun(x_vector[2 * ii - 1]);
	y_vector[2 * ii + 1] <- y_vector[2 * ii];
	x_vector[2 * ii] <- x_vector[2 * ii - 1];
	x_vector[2 * ii + 1] <- y_vector[2 * ii + 1];
}
plot(-100:500 / 100, dFun(-100:500 / 100), xlab='x(t)', ylab='x(t+1)',
    main='Cobweb Plot', type='l',
    xlim=c(0, 0.4), ylim=c(0,0.3),
    xaxs='i', yaxs='i')
abline(0, 1, col='red')
lines(x_vector, y_vector, col='blue')
points(x_vector[50], y_vector[50])

## Fixed points at 0 and 3/14. Blue line approaches 3/14, thus it's stable.

#/--#########################################################################
# name: Crnich, Joseph
# assignment: 3
# date: 10/23/2009
# question: 3
# subquestion: 
# other files: 
##########################################################################/--

## Define some function:

exampleFun <- function(x) {
	return(x * (2 - 3 * x / 2));
}

## Then define the functor plotCob; y and x vectors are designed to alternate between the 'previous' values of one of two functions, the argument function, f, and the line y=x.

plotCob <- function(f) {
    max_iter <- 50
    y_vector <- x_vector <- numeric(max_iter * 2)
    y_vector[1] <- 0
    x_vector[1] <- 0.15
    for(ii in 1:50)	{
	    y_vector[2 * ii] <- sapply(x_vector[2 * ii - 1], f);
	    y_vector[2 * ii + 1] <- y_vector[2 * ii];
	    x_vector[2 * ii] <- x_vector[2 * ii - 1];
	    x_vector[2 * ii + 1] <- y_vector[2 * ii + 1];
    }
    
    ## This is then plotted in blue, along with y=x in red, and the argument
    ## function, f, in black.
    
    plot(-100:200 / 100, sapply(c(-100:200 / 100), f),
        xlab='x(t)', ylab='x(t+1)',
        main='Cobweb Plot', type='l',
        xlim=c(0, 2), ylim=c(0, 2),
        xaxs='i', yaxs='i')
    abline(0, 1, col='red')
    lines(x_vector, y_vector, col='blue')
    points(x_vector[50], y_vector[50])
}
plotCob(exampleFun)

#/--#########################################################################
# name: Crnich, Joseph
# assignment: 3
# date: 10/23/2009
# question: 4
# subquestion: a
# other files: 
##########################################################################/--

## G'(t) = -k * G(t)
## This function is an autonomous, linear ODE.

#/--#########################################################################
# name: Crnich, Joseph
# assignment: 3
# date: 10/23/2009
# question: 4
# subquestion: b
# other files: 
##########################################################################/--

## G'(t) = -k * G(t)
  ## dG/dt = -k * G
  ## dG/G = -k dt
  ## ∫dG/G = ∫-k dt
  ## ln |G| = -kt + C
  ## G = e^(-kt + C)
  ## G = e^C * e^(-kt)
    ## G(0) = e^C

## G = G(0) * e^(-kt)

#/--#########################################################################
# name: Crnich, Joseph
# assignment: 3
# date: 10/23/2009
# question: 4
# subquestion: c
# other files: 
##########################################################################/--

gluFun <- function(t) {
	return(100 * exp(-0.01 * t));
}

plot(0:100000 / 100, sapply(c(0:100000 / 100), gluFun),
    xlab='time (min)', ylab='G(t) (mg/dl)',
    main='Glucose Concentration v.1a', type='l',
    xlim=c(0, 800), ylim=c(0, 110),
    xaxs='i', yaxs='i')

#/--#########################################################################
# name: Crnich, Joseph
# assignment: 3
# date: 10/23/2009
# question: 4
# subquestion: d
# other files: 
##########################################################################/--

## In this model, the equilibrium concentration is 0 mg/dl. The point is
## stable, as the graph approaches it consistently. Even if the initial
## concentration is drastically increased, the graph eventually returns to 
## zero. The example below has a G(0) = 1000 mg/dl:

glu2Fun <- function(t) {
	return(1000 * exp(-0.01 * t));
}

plot(0:100000 / 100, sapply(c(0:100000 / 100), glu2Fun),
    xlab='time (min)', ylab='G(t) (mg/dl)',
    main='Glucose Concentration v.1b', type='l',
    xlim=c(0, 800), ylim=c(0, 1100),
    xaxs='i', yaxs='i')

#/--#########################################################################
# name: Crnich, Joseph
# assignment: 3
# date: 10/23/2009
# question: 5
# subquestion: a
# other files: 
##########################################################################/--

## G'(t) = -k * G(t) + a
## This function is also an autonomous, linear ODE.

#/--#########################################################################
# name: Crnich, Joseph
# assignment: 3
# date: 10/23/2009
# question: 5
# subquestion: b
# other files: 
##########################################################################/--

## G'(t) = -k * G(t) + a
  ## dG/dt = -k * G + a
  ## dG/dt + k * G = a

## We can find a solution by using an integration factor: a(t) = -k, and
## b(t) = a. Therefore, our analytic solution is as follows, first
## multiplying by e^(-∫a(t)dt):

  ## dG/dt * e^(kt) + k * G * e^(kt) = a * e^(kt)

## Then, by the product rule:

  ## d/dt * (G * e^(kt)) = a * e^(kt)

## Integrate:

  ## G * e^(kt) = a/k * e^(kt) + C
  ## G = a/k * e^(kt) * e^(-kt) + C * e^(-kt)
  ## G = a/k + C * e^(-kt)
    ## G(0) = a/k + C
    ## C = G(0) - a/k

## G = a/k + (G(0) - a/k) * e^-kt


#/--#########################################################################
# name: Crnich, Joseph
# assignment: 3
# date: 10/23/2009
# question: 5
# subquestion: c
# other files: 
##########################################################################/--

glu3Fun <- function(t) {
	return(4 / 0.01 + (100 - 4 / 0.01) * exp(-0.01 * t));
}

plot(0:100000 / 100, sapply(c(0:100000 / 100), glu3Fun),
    xlab='time (min)', ylab='G(t) (mg/dl)',
    main='Glucose Concentration v.2a', type='l',
    xlim=c(0, 1000), ylim=c(0, 500),
    xaxs='i', yaxs='i')

#/--#########################################################################
# name: Crnich, Joseph
# assignment: 3
# date: 10/23/2009
# question: 5
# subquestion: d
# other files: 
##########################################################################/--

## The equilibrium is concentration is 400 mg/dl. It's also stable. For
## example, if we start the concentration higher than 400, it decreases
## gradually until it reaches the equilibrium again. Here, G(0) = 500 mg/dl:

glu4Fun <- function(t) {
	return(4 / 0.01 + (500 - 4 / 0.01) * exp(-0.01 * t));
}

plot(0:100000 / 100, sapply(c(0:100000 / 100), glu4Fun),
    xlab='time (min)', ylab='G(t) (mg/dl)',
    main='Glucose Concentration v.2b', type='l',
    xlim=c(0, 1000), ylim=c(0, 500),
    xaxs='i', yaxs='i')
