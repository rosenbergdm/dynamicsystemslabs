#!/usr/bin/env r
# encoding: utf-8
# 
# name: MEGAN LISZEWSKI
# assignment: 3
# date: 10/23/09
# filename: Homework3.r
#############################################################################


#/--#########################################################################
# name: MEGAN LISZEWSKI
# assignment: 3
# date: 10/23/09
# question: 1
# subquestion: a&b
# other files: WORD FILE (no R code for this question)
##########################################################################/--

#/--#########################################################################
# name: MEGAN LISZEWSKI
# assignment: 3
# date: 10/23/09
# question: 2
# subquestion: a
# other files: word file
##########################################################################/--
cobImapFun <- function(x) {
	return (x*(5-4*x))
	}
max_iter <-50
y_vector<-x_vector<-numeric(max_iter*2)
y_vector[1]<-0
x_vector[1]<-0.05
for (ii in 1:50){
	y_vector[2*ii]<-cobImapFun(x_vector[2*ii-1])
	y_vector[2*ii+1]<-y_vector[2*ii]
	x_vector[2*ii]<-x_vector[2*ii-1]
	x_vector[2*ii+1]<-y_vector[2*ii+1]
	}

plot(-10:500/100, cobImapFun(-10:500/ 100), xlab='x',
ylab='y', main='2a. Cobweb Plot', type='l', xlim=c(-.1,1.5),
ylim=c(-.1,2),xaxs='i', yaxs='i')

lines(-2:6, -2:6, col='red')
lines(x_vector,y_vector, col='blue')
points(0,0)
points(1,1)

#/--#########################################################################
# name: MEGAN LISZEWSKI
# assignment: 3
# date: 10/23/09
# question: 2
# subquestion: b
# other files: word file
##########################################################################/--
cobImapFun <- function(x) {
	return (x*(2-3*x))
	}
max_iter <-50
y_vector<-x_vector<-numeric(max_iter*2)
y_vector[1]<-0
x_vector[1]<-.1
for (ii in 1:50){
	y_vector[2*ii]<-cobImapFun(x_vector[2*ii-1])
	y_vector[2*ii+1]<-y_vector[2*ii]
	x_vector[2*ii]<-x_vector[2*ii-1]
	x_vector[2*ii+1]<-y_vector[2*ii+1]
	}

plot(-10:500/100, cobImapFun(-10:500 / 100), xlab='x',
ylab='y', main='2b. Cobweb Plot', type='l', xlim=c(0,.7),
ylim=c(0,.4),xaxs='i', yaxs='i')

lines(0:1, 0:1, col='red')
lines(x_vector,y_vector, col='blue')
points(0,0)
points(1/3,1/3)

#/--#########################################################################
# name: MEGAN LISZEWSKI
# assignment: 3
# date: 10/23/09
# question: 2
# subquestion: c
# other files: word file
##########################################################################/--

cobImapFun <- function(x) {
	return (x*(5/2-7*x))
	}
max_iter <-50
y_vector<-x_vector<-numeric(max_iter*2)
y_vector[1]<-0
x_vector[1]<-.01
for (ii in 1:50){
	y_vector[2*ii]<-cobImapFun(x_vector[2*ii-1])
	y_vector[2*ii+1]<-y_vector[2*ii]
	x_vector[2*ii]<-x_vector[2*ii-1]
	x_vector[2*ii+1]<-y_vector[2*ii+1]
	}

plot(-50:500/100, cobImapFun(-50:500 /100), xlab='x',
ylab='y', main='2c. Cobweb Plot', type='l', xlim=c(0,.4),
ylim=c(0,.25),xaxs='i', yaxs='i')

lines(0:1, 0:1, col='red')
lines(x_vector,y_vector, col='blue')
points(0,0)
points(3/14,3/14)

#/--#########################################################################
# name: MEGAN LISZEWSKI
# assignment: 3
# date: 10/23/09
# question: 3
# subquestion: 
# other files: word file
##########################################################################/--


cobImapFun <- function(F_x) {

max_iter <-50;
y_vector<-x_vector<-numeric(
	max_iter*2);
y_vector[1]<-0;
x_vector[1]<-1;
for (ii in 1:50){
	y_vector[2*ii]<-F_x(
		x_vector[2*ii-1]);
	y_vector[2*ii+1]<-
		y_vector[2*ii];
	x_vector[2*ii]<-
		x_vector[2*ii-1];
	x_vector[2*ii+1]<-
		y_vector[2*ii+1]
	}

plot(-10:50/10, F_x(-10:50/ 10), xlab='x',
ylab='y', main='3. Functor Cobweb Plot', type='l', xlim=c(0,4),
ylim=c(0,4),xaxs='i', yaxs='i')

lines(-2:6, -2:6, col='red')
lines(x_vector,y_vector, col='blue')
#return(x_vector)
}

F_x<-function(x){
	return(2*x-0.5*x**2)
	}
	
cobImapFun(F_x)
#/--#########################################################################
# name: MEGAN LISZEWSKI
# assignment: 3
# date: 10/23/09
# question: 4
# subquestion:a&b 
# other files:WORD FILE (no R code)
##########################################################################/--

#/--#########################################################################
# name: MEGAN LISZEWSKI
# assignment: 3
# date: 10/23/09
# question: 4
# subquestion:c
# other files: word file
##########################################################################/--
Go<-100
k=0.01
t<-1:2000
G<-Go*exp(-k*t)
plot(t,G,type='l')

#/--#########################################################################
# name: MEGAN LISZEWSKI
# assignment: 3
# date: 10/23/09
# question: 4
# subquestion:d
# other files: WORD FILE (no R code)
##########################################################################/--

#/--#########################################################################
# name: MEGAN LISZEWSKI
# assignment: 3
# date: 10/23/09
# question: 5
# subquestion:a&b
# other files: WORD FILE (no R code)
##########################################################################/--

#/--#########################################################################
# name: MEGAN LISZEWSKI
# assignment: 3
# date: 10/23/09
# question: 5
# subquestion:c
# other files: word file
##########################################################################/--

Go<100
k=0.01
a=4
t<-1:2000
G<-a/k+(Go-(a/k))*exp(-k*t)
plot(t,G,type='l')

#/--#########################################################################
# name: MEGAN LISZEWSKI
# assignment: 3
# date: 10/23/09
# question: 5
# subquestion:d
# other files: WORD FILE (no R code)
##########################################################################/--