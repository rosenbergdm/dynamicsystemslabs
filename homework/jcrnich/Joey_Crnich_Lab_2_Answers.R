#!/usr/bin/env r
# encoding: utf-8
# 
# name: Crnich, Joseph
# assignment: 2
# date: 10/18/2006
# filename: Joey_Crnich_Lab_2_Answers.R
#############################################################################


#/--#########################################################################
# name: Crnich, Joseph
# assignment: 2
# date: 10/18/2006
# question: 1
# subquestion: 
# other files: 
##########################################################################/--

inputEx <- "_"									# Input polynomial for '_'
result <- parsePolynomial(inputEx)					# Parsing the polynomial for it's exp./coef.
newCoef <- result[['exponents']] * result[['coefficients']]	# Establishing coef. of derivative as product of polynomial's coef. and exp.
newExp <- result[['exponents']] - 1					# Reducing all exp. by one (as polynomial derivatives do)
deparsePolynomial(newCoef, newExp)				# Deparsing the new coef. and exp. back into a polynomial

#/--#########################################################################
# name: Crnich, Joseph
# assignment: 2
# date: 10/18/2006
# question: 2
# subquestion: a/b
# other files: 
##########################################################################/--

logModFun <- function(x_at_t) {				# These commands define the equation f(x)
      x_at_t1 <- x_at_t * (2 - x_at_t/2);
      return(x_at_t1);
}
init_value <- 1;						# These commands define a vector of x values (integers -> discrete time)
max_iter <- 10;							# Changing 'max_iter' and 'init_value' will allow display of larger/smaller
x_array <- numeric(length=max_iter);	# sections of the graph and left boundary of graph, respectively.
x_array[1] <- init_value;
for (ii in 2:max_iter) {				# This for command redefines the previous vector as values for the graph [x(t)].
      new_value <- logModFun(x_array[ii-1]);
      x_array[ii] <- new_value
}
plot(1:10, x_array, type='l', main='Logistic Growth Model', xlab='t', ylab='x(t)')	# Plotting x as a function of t

nt <- 1							# These three definitions allow for the following command chunk to find the fixed point.
nt_plus_1 <- logModFun(nt)
nt_plus_2 <- logModFun(nt_plus_1)
count <- 1										# This count is used to graphically find the fixed point below.
while ((nt_plus_1 != nt) | (nt_plus_1 != nt_plus_2)) {	# This while command runs through each x(t) until
      nt_plus_1 <- logModFun(nt)				# it's the same as x(t-1), bumping the counter up one each time.
      nt_plus_2 <- logModFun(nt_plus_1)
      count <- count + 1
      nt <- nt_plus_2
}
cat('The fixed point is', nt, '.')
points(count, nt)							# Here, we graph the point.

#/--#########################################################################
# name: Crnich, Joseph
# assignment: 2
# date: 10/18/2006
# question: 3
# subquestion: a
# other files: 
##########################################################################/--

#	x = 2x(1 - x)
#	1/2 = 1 - x
#	x = 1/2

logModFun <- function(x_at_t) {
      x_at_t1 <- 2 * x_at_t * (1 - x_at_t);
      return(x_at_t1);
}

init_value <- 1/100;					# These commands define a vector of x values (integers -> discrete time)
max_iter <- 20;							# Changing 'max_iter' and 'init_value' will allow display of larger/smaller
x_array <- numeric(length=max_iter);	# sections of the graph and left boundary of graph, respectively.
x_array[1] <- init_value;
for (ii in c((2:20) / 100)) {			# This for command redefines the previous vector as values for the graph [x(t)].
      new_value <- logModFun(x_array[(ii * 100) - 1]);
      x_array[(ii * 100)] <- new_value
}
plot(((1:20) / 100), x_array, type='l',
      main='Logistic Growth Model', xlab='t', ylab='x(t)')	# Plotting x as a function of t…
points(0.11, logModFun(x_array[11]))		# …and it's fixed point.
cat("There's a fixed point at", logModFun(x_array[11]), ".")

#/--#########################################################################
# name: Crnich, Joseph
# assignment: 2
# date: 10/18/2006
# question: 3
# subquestion: b
# other files: 
##########################################################################/--

#	x = 4x(1 - x)
#	1/4 = 1 - x
#	x = 3/4

logModFun <- function(x_at_t) {
      x_at_t1 <- 4 * x_at_t * (1 - x_at_t);
      return(x_at_t1);
}

init_value <- 1/100;					# These commands define a vector of x values (integers -> discrete time)
max_iter <- 20;							# Changing 'max_iter' and 'init_value' will allow display of larger/smaller
x_array <- numeric(length=max_iter);	# sections of the graph and left boundary of graph, respectively.
x_array[1] <- init_value;
for (ii in c((2:20) / 100)) {			# This for command redefines the previous vector as values for the graph [x(t)].
      new_value <- logModFun(x_array[(ii * 100) - 1]);
      x_array[(ii * 100)] <- new_value
}
plot(((1:20) / 100), x_array, type='l',
      main='Logistic Growth Model', xlab='t', ylab='x(t)')	# Plotting x as a function of t…

## …clearly shows that the fixed point at 3/4 is unstable (because
## the computed fixed point is not approached in the iterated graph).


