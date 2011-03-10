#!/usr/bin/env r
# encoding: utf-8
# 
# name: Patrick Mann
# assignment: 3
# date: 10-22-09
# filename: Chapter 2 Exercises
#############################################################################







#/--#########################################################################
# name: Patrick Mann
# assignment: 3
# date: 10-22-09
# question: 2
# subquestion: a
# other files: NAMES_OF_OTHER_RELATED_FILES
##########################################################################/--

cobImapFun <- function(x) {
	return(3 * x - 0.75 * x ** 2 + 1);
	}
max_iter <- 50
y_vector <- x_vector <- numeric(max_iter * 2);
y_vector[1] <- 0
x_vector[1] <- 0.5
for(ii in 1:50) {
	y_vector[2 * ii] <- cobImapFun(
		x_vector[2 * ii - 1]);
	y_vector[2 * ii + 1] <-
		y_vector[2 * ii];
	x_vector[2 * ii] <-
		x_vector[2 * ii - 1];
	x_vector[2 * ii + 1] <-
		y_vector[2 * ii + 1];
}
plot( -10:50 / 10,
		cobImapFun( -10:50 / 10),
		xlab='x',
		ylab='y',
		main='Question 2a Cobweb, Green = Fixed Point',
		type='l',
		xlim=c(-2, 6),
		ylim=c(-2, 5),
		xaxs='i',
		yaxs='i');
lines(-2:6, -2:6, col='red');
lines(x_vector, y_vector,
		col='blue');
x_fixed <- c(-0.4305, 3.0972)
y_fixed <- c(-0.4305, 3.0972)
points(x_fixed, y_fixed, pch=19, col="green", cex=1)

#Fixed Points are both Unstable

#/--#########################################################################
# name: Patrick Mann
# assignment: 3
# date: 10-22-09
# question: 2
# subquestion: b
# other files: NAMES_OF_OTHER_RELATED_FILES
##########################################################################/--

cobImapFun <- function(x) {
	return(100 * x - 2 * x ** 2);
	}
max_iter <- 500
y_vector <- x_vector <- numeric(max_iter * 2);
y_vector[1] <- 0
x_vector[1] <- 0.3
for(ii in 1:500) {
	y_vector[2 * ii] <- cobImapFun(
		x_vector[2 * ii - 1]);
	y_vector[2 * ii + 1] <-
		y_vector[2 * ii];
	x_vector[2 * ii] <-
		x_vector[2 * ii - 1];
	x_vector[2 * ii + 1] <-
		y_vector[2 * ii + 1];
}
plot( -10:500 / 10,
		cobImapFun( -10:500 / 10),
		xlab='x',
		ylab='y',
		main='Question 2b Cobweb, Green = Fixed Point',
		type='l',
		xlim=c(-5, 50),
		ylim=c(-100, 1300),
		xaxs='i',
		yaxs='i');
lines(-100:55, -100:55, col='red');
lines(x_vector, y_vector,
		col='blue');
x_fixed <- c(0, 49.5)
y_fixed <- c(0, 49.5)
points(x_fixed, y_fixed, pch=19, col="green", cex=1)

#Fixed Points are both Unstable

#/--#########################################################################
# name: Patrick Mann
# assignment: 3
# date: 10-22-09
# question: 2
# subquestion: c
# other files: NAMES_OF_OTHER_RELATED_FILES
##########################################################################/--

cobImapFun <- function(x) {
	return(-100 * x + 0.5 * x ** 2);
	}
max_iter <- 5000
y_vector <- x_vector <- numeric(max_iter * 2);
y_vector[1] <- 0
x_vector[1] <- 201.5
for(ii in 1:5000) {
	y_vector[2 * ii] <- cobImapFun(
		x_vector[2 * ii - 1]);
	y_vector[2 * ii + 1] <-
		y_vector[2 * ii];
	x_vector[2 * ii] <-
		x_vector[2 * ii - 1];
	x_vector[2 * ii + 1] <-
		y_vector[2 * ii + 1];
}
plot( -5000:5000 / 10,
		cobImapFun( -5000:5000 / 10),
		xlab='x',
		ylab='y',
		main='Question 2c Cobweb, Green = Fixed Point',
		type='l',
		xlim=c(-100, 250),
		ylim=c(1000, -5500),
		xaxs='i',
		yaxs='i');
lines(-250:250, -250:250, col='red');
lines(x_vector, y_vector,
		col='blue');
x_fixed <- c(0, 202)
y_fixed <- c(0, 202)
points(x_fixed, y_fixed, pch=19, col="green", cex=1)

#Fixed Points are both Unstable

#/--#########################################################################
# name: Patrick Mann
# assignment: 3
# date: 10-22-09
# question: 3
# subquestion: 
# other files: NAMES_OF_OTHER_RELATED_FILES
##########################################################################/--

cobImapFun <- function(x) {
	return(2 * x - 0.75 * x ** 2 + 1);
	}
Funct <- cobImapFun
mrFunctor <- function(Funct)
{
max_iter <- 50
y_vector <- x_vector <- numeric(max_iter * 2);
y_vector[1] <- 0
x_vector[1] <- 0.5
for(ii in 1:50) {
	y_vector[2 * ii] <- Funct(
		x_vector[2 * ii - 1]);
	y_vector[2 * ii + 1] <-
		y_vector[2 * ii];
	x_vector[2 * ii] <-
		x_vector[2 * ii - 1];
	x_vector[2 * ii + 1] <-
		y_vector[2 * ii + 1];
}
plot( -10:50 / 10,
		Funct( -10:50 / 10),
		xlab='x',
		ylab='y',
		main='Example cobweb',
		type='l',
		xlim=c(0, 4),
		ylim=c(0, 2.5),
		xaxs='i',
		yaxs='i');
lines(-2:6, -2:6, col='red');
lines(x_vector, y_vector,
		col='blue');
}
mrFunctor(Funct)

#/--#########################################################################
# name: Patrick Mann
# assignment: 3
# date: 10-22-09
# question: 4
# subquestion: c
# other files: NAMES_OF_OTHER_RELATED_FILES
##########################################################################/--

euler_number <- 2.7182818284
GlucConc_at_t <- function(t) {
	return(100 * euler_number^(-0.01 * t) )
	}
plot(0:250, GlucConc_at_t(0:250), 
	xlim=c(0, 250), 
	ylim=c(0, 100),
	xlab='Time (min)',
	ylab='Concentration of Glucose (mg/dl)',
	main='Glucose Concentration in Blood',
	type='l'
	)

#/--#########################################################################
# name: Patrick Mann
# assignment: 3
# date: 10-22-09
# question: 5
# subquestion: c
# other files: NAMES_OF_OTHER_RELATED_FILES
##########################################################################/
--

euler_number <- 2.7182818284
GlucConc_at_t <- function(t) {
	return((4/0.01) + (100 -(4/0.01) ) * euler_number^(-0.01 * t) )
	}
plot(0:500, GlucConc_at_t(0:500), 
	xlim=c(0, 500), 
	ylim=c(0, 450),
	xlab='Time (min)',
	ylab='Concentration of Glucose (mg/dl)',
	main='Glucose Concentration in Blood',
	type='l'
	)
