#!/usr/bin/env r
# encoding: utf-8
# 
# name: Namita Gupta
# assignment: 3
# date: Oct. 23, 2009
# filename: Namita Gupta.lab3.R
#############################################################################

#/--#########################################################################
# name: Namita Gupta
# assignment: 3
# date: Oct. 23, 2009
# question: 2
# subquestion: a
# other files: fun2a_cobweb.eps
##########################################################################/--
fun2a <- function(x) {
	return(x*(5-4*x));
}

#/--#########################################################################
# name: Namita Gupta
# assignment: 3
# date: Oct. 23, 2009
# question: 2
# subquestion: b
# other files: fun2b_cobweb.eps
##########################################################################/--
fun2b <- function(x) {
	return(x*(2-3*x));
}

#/--#########################################################################
# name: Namita Gupta
# assignment: 3
# date: Oct. 23, 2009
# question: 2
# subquestion: c
# other files: fun2c_cobweb.eps, fun2c_cobweb2.eps
##########################################################################/--
fun2c <- function(x) {
	return(x*(1/2 - x/5));
}

#/--#########################################################################
# name: Namita Gupta
# assignment: 3
# date: Oct. 23, 2009
# question: 2
# subquestion: d
# other files: fun2d_cobweb.eps
##########################################################################/--
fun2d <- function(x) {
	return(x*(5/2-7*x));
}

#/--#########################################################################
# name: Namita Gupta
# assignment: 3
# date: Oct. 23, 2009
# question: 3
# subquestion: 
# other files: NAMES_OF_OTHER_RELATED_FILES
##########################################################################/--
cobwebPlot <- function(f) {
	max_iter <- 50;
	y_vector <- x_vector <- numeric(max_iter*2);
	y_vector[1] <- 0;
	x_vector[1] <- .1;
	for(j in 1:50) {
		y_vector[2*j] <- do.call(f,list(x=x_vector[2*j-1]));
		y_vector[2*j+1] <- y_vector[2*j];
		x_vector[2*j] <- x_vector[2*j-1];
		x_vector[2*j+1] <- y_vector[2*j+1];
	}
	curve(f, 0,1, xlab= 'x', ylab= 'f(x)', main='Exercise 3', 
		type= 'l');
	lines(-3:6, -3:6, col='red');
	lines(x_vector, y_vector, col='blue');
}
#/--#########################################################################
# name: Namita Gupta
# assignment: 3
# date: Oct. 23, 2009
# question: 4
# subquestion: c
# other files: graph4c.eps
##########################################################################/--

#curve(100*exp(-.01*x), 0, 800, type = 'l', xlab = 't',
#	ylab = 'G(t)', main = 'Exponential Decay of Glucose')

#/--#########################################################################
# name: Namita Gupta
# assignment: 3
# date: Oct. 23, 2009
# question: 5
# subquestion: c
# other files: graph5c.eps
##########################################################################/--

#curve((-300*exp(-.01*x)+400), 0, 800, type = 'l', xlab = 't',
#	ylab = 'G(t)', main = 'Decay with Glucose Added Too')