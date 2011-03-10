#!/usr/bin/env r
# encoding: utf-8
# 
# name: Katie Sirianni
# assignment: 2
# date: 10/15/09
# filename: Sirianni Homework Chp 1
#############################################################################

inputEx<- "" #insert equation to be differentiated

derivFunction<- function(inputEx) {
   parsePolynomial(inputEx)
   result<- parsePolynomial(inputEx)
newExponents<- result[['exponents']]
newCoefficients<- result[['coefficients']]
coefficients<- newCoefficients*newExponents
exponents<-newExponents-1
firstDerivative<-deparsePolynomial(coefficients,exponents)
return(firstDerivative)
}

#/--#########################################################################
# name: Katie Sirianni
# assignment: 2
# date: 10/15/09
# question: 2
# subquestion: a
##########################################################################/--

# Logistic plot of f(x)= x(2-x/2)

logModel <- function(y) {
    y1 <- y*(2-y/2)
    return (y1);
}
init_value <- 1;
max_iter <- 20;
x_array <- numeric(length=max_iter);
x_array[1] <- 1;
for ( ii in 2:max_iter) {
   new_value <- logModel( x_array[ii-1]) ;
   x_array[ii] <- new_value
}
plot(1:20, x_array, 
   main= "My First Logistic Plot",
   xlab= "Time",
   ylab= "Population Size",
   type= "l"
)

#/--#########################################################################
# name: KAtie Sirianni
# assignment: 2
# date: 10/15/09
# question: 2
# subquestion: b
##########################################################################/--

fixedPoints<- numeric()
fixedPoints[1]<- 0
timePlusone<- sapply(x_array, logModel) 
plot(x_array, timePlusone,       # this plots Nt vs N(t+1)
   main="Fixed Point Graph", 
   xlab= "f(xsubt)", 
   ylab= "f(xsub(t+1))", 
   xlim=c(0,5), 
   ylim= c(0,5), 
   type= "l")
abline(0,1, col= "red")    
for(i in 1:max_iter) {
   if(x_array[i]==timePlusone[i]) {
     fixedPoints<-c(fixedPoints, x_array[i])
}
   if(x_array[i]==timePlusone[i]) {
     points(x_array[i], timePlusone[i], col= "blue")
}
}
fixedPoints

#  Calculating fixed points analytically:
#  f(x) = x(2-x/2)
#  logistic Equation: Nsub(t+1)= Nt(r-kNt)

#  so in this equation, r=2 and k=.5
#  fixed points at N*=0 and at 
#  N*= (r-1)/k =(2-1)/.5=1/.5=2

#  so the fixed points are at 0 and 2

#  Which matches what was calculated, yay!

#/--#########################################################################
# name: Katie Sirianni
# assignment: 2
# date: 10/15/09
# question: 3
# subquestion: a
##########################################################################/--

#  analytically: 
#  2x(1-x) == x(2-2x) 
#  so r=2 and k=2
#  so N*= 0 and N*= (2-1)/2 = 1/2

logModel <- function(y) {
    y1 <- (2*y)*(1-y)
    return (y1);
}
init_value <- .2;
max_iter <- 20;
x_array <- numeric(length=max_iter);
x_array[1] <- init_value;
for ( ii in 2:max_iter) {
   new_value <- logModel( x_array[ii-1]) ;
   x_array[ii] <- new_value
}
plot(1:max_iter, x_array, 
   main= "3a",
   xlab= "Time",
   ylab= "Population Size")

fixedPoints<- numeric()
fixedPoints[1]<- 0
timePlusone<- sapply(x_array, logModel)
plot(x_array, timePlusone, 
   main="Fixed Point Graph", 
   xlab= "f(xsubt)", 
   ylab= "f(xsub(t+1))", 
   xlim=c(0,5), ylim= c(0,5), 
   type= "l")
abline(0,1, col= "red")
for(i in 1:max_iter) {
   if(x_array[i]==timePlusone[i]) {
      fixedPoints<-c(fixedPoints, x_array[i])
}
   if(x_array[i]==timePlusone[i]) {
      points(x_array[i], timePlusone[i], col= "blue")
}
}
fixedPoints

#/--#########################################################################
# name: Katie Sirianni	
# assignment: 2
# date: 10/15/09
# question: 3
# subquestion: b
# other files: NAMES_OF_OTHER_RELATED_FILES
##########################################################################/--

#  f(x)= 4x(1-x)
#  analytically:
#  4x(1-x) == x(4-4x)
#  so r= 4 and k=4
#  then N* = 0 and N* = (r-1)/k = (4-1)/4 = 3/4

logModel <- function(y) {        # This doesn't really work, but
    y1 <- (4*y)*(1-y)           # I guess we didn't have to do it
    return (y1);                # anyways. 
}
init_value <- .25;
max_iter <- 20;
x_array <- numeric(length=max_iter);
x_array[1] <- init_value;
for ( ii in 2:max_iter) {
   new_value <- logModel( x_array[ii-1]) ;
   x_array[ii] <- new_value
}

fixedPoints<- numeric()
fixedPoints[1]<- 0
timePlusone<- sapply(x_array, logModel)
plot(x_array, timePlusone, main="Fixed Point Graph", xlab= "f(xsubt)", ylab= "f(xsub(t+1))", xlim=c(0,2), ylim= c(0,2), type="l")
abline(0,1, col= "red")
for(i in 1:max_iter) {
   if(x_array[i]==timePlusone[i]) {
      fixedPoints<-c(fixedPoints, x_array[i])
}
   if(x_array[i]==timePlusone[i]) {
      points(x_array[i], timePlusone[i], col= "blue")
}
}
fixedPoints
