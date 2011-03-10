#!/usr/bin/env r
# encoding: utf-8
# 
# name: BRENDAN COLSON
# assignment: 2
# date: 10/22/09
# filename: EXCERCISE 1
# subquestion:
# other files: 
##########################################################################/--






#/--#########################################################################
# name: BRENDAN COLSON
# assignment: 2
# date: 10/22/09
# question: 1
# subquestion: 
# other files: 
##########################################################################/--

#poly is the polynomial
# result is assigned as the parsed polynomial
# newExp reduces the exponent by one, then is reassigned to remove negative 
#  exponents
# newCoe takes original coefficients and multiplies by original exponents
#  then is reassigned to have same length as extant exponents
#deparse constructs the new polynomial

poly <- "2x^3 + x^2 + 3x + 4"
result <- parsePolynomial(poly)
    newExp <- result[['exponents']] - 1
    newExp <- newExp[newExp != -1]
    newCoe <- result[['coefficients']] * result[['exponents']]
    newCoe <- newCoe[1:length(newExp)]
deparsePolynomial(newCoe, newExp)




#/--#########################################################################
# name: BRENDAN COLSON
# assignment: 2
# date: 10/22/09
# question: 2
# subquestion: A
# other files: 
##########################################################################/--

diffEqImap <- function(x_at_t) {
  x_at_t1 <- x_at_t * (2 - (x_at_t / 2))
  return(x_at_t1)}
init_value <- 1
max_iter <- 10;
x_array <- numeric(length=max_iter);
x_array[1] <- init_value
  for(ii in  2:max_iter) {
    new_value <- diffEqImap(x_array[ii-1])
    x_array[ii] <- new_value
}
plot(1:max_iter, x_array, type='l',
     main='2a', xlab='t',
    ylab='x(t)')


#/--#########################################################################
# name: BRENDAN COLSON
# assignment: 2
# date: 10/22/09
# question: 2
# subquestion: B
# other files: 
##########################################################################/--

#try with do.call.  Does this make it a functor?
diffEqImap <- function(x_at_t) {
  x_at_t1 <- x_at_t * (2 - (x_at_t / 2))
  return(x_at_t1)}
init_value <- 1
max_iter <- 10;
x_array <- numeric(length=max_iter);
x_array[1] <- init_value
  for(ii in  2:max_iter) {
    x_array[ii] <- do.call(diffEqImap, list(x_array[ii-1]))
}
plot(1:max_iter, x_array, type='l',
     main='2a', xlab='t',
    ylab='x(t)')

#/--#########################################################################
# name: BRENDAN COLSON
# assignment: 2
# date: 10/22/09
# question: 3
# subquestion: A
# other files: 
##########################################################################/--

diffEqImap <- function(x_at_t) {
  x_at_t1 <- 2* x_at_t * (1 - x_at_t)
  return(x_at_t1)}
init_value <- .1
max_iter <- 10;
x_array <- numeric(length=max_iter);
x_array[1] <- init_value
  for(ii in  2:max_iter) {
    x_array[ii] <- do.call(diffEqImap, list(x_array[ii-1]))
}
plot(1:max_iter, x_array, type='l',
     main='3a', xlab='t',
    ylab='x(t)')


#/--#########################################################################
# name: BRENDAN COLSON
# assignment: 2
# date: 10/22/09
# question: 3
# subquestion: B
# other files: 
##########################################################################/--

diffEqImap <- function(x_at_t) {
  x_at_t1 <- 4 * x_at_t * (1 - x_at_t)
  return(x_at_t1)}
init_value <- 1
max_iter <- 10;
x_array <- numeric(length=max_iter);
x_array[1] <- init_value
  for(ii in  2:max_iter) {
    x_array[ii] <- do.call(diffEqImap, list(x_array[ii-1]))
}
plot(1:max_iter, x_array, type='l',
     main='3b', xlab='t',
    ylab='x(t)')

