#!/usr/bin/env r
# encoding: utf-8
# 
# name: Namita Gupta
# assignment: 4
# date:  2009
# filename: Namita Gupta.lab4.R
#***NOTE:  Worked on this homework assignment with Craig Biwer.***
#############################################################################

#/--#########################################################################
# name: Namita Gupta
# assignment: 4
# date: 2009
# question: 1
# subquestion: b
# other files: NAMES_OF_OTHER_RELATED_FILES
##########################################################################/--

#g(N) = curve((10*x**2*(1-x/200)), 0, 210)
#f(N) = curve((10*x*(x/20-1)*(1-x/200)), 0, 210)

#/--#########################################################################
# name: Namita Gupta
# assignment: 4
# date: 2009
# question: 2
# subquestion: c
# other files: NAMES_OF_OTHER_RELATED_FILES
##########################################################################/--

dirField <- function(f,h,tf,x0,xf) {
	tRange <- c(0, tf)
	dt <- .5
	xRange <- c(x0,xf)
	dx <- .5
	h <- h
	tSteps <- diff(range(tRange)) / dt
	xSteps <- diff(range(xRange)) / dx
	plot(c(0,tf-1), c(x0,xf-1), type='n')
	for(i in 0:(tSteps-1)) {
		for(j in 0:(xSteps-1)) {
			t <- i*dt;
			x <- x0+j*dx;
			m <- do.call(f,list(x=x, t=t));
			u <- t + (h*dt);
			v <- x + (h*dt) *m;
			arrows(t,x,u,v,code=2,length=.01)
		}
	}
}

bact <- function(x, t) {
	return(x - sin(2*t/pi));
}
#dirField(bact,.5,10,0,10)

#/--#########################################################################
# name: Namita Gupta
# assignment: 4
# date: 2009
# question: 2
# subquestion: d
# other files: NAMES_OF_OTHER_RELATED_FILES
##########################################################################/--

#curve(0*exp(x) + pi*(2*cos(2*x/pi) + pi*sin(2*x/pi))/(4+(pi**2)), 0,10)
#curve(-.05*exp(x) + pi*(2*cos(2*x/pi) + pi*sin(2*x/pi))/(4+(pi**2)), 0,3)
#curve(.01*exp(x) + pi*(2*cos(2*x/pi) + pi*sin(2*x/pi))/(4+(pi**2)), 0,5)

#/--#########################################################################
# name: Namita Gupta
# assignment: 4
# date: 2009
# question: 3
# subquestion: b
# other files: NAMES_OF_OTHER_RELATED_FILES
##########################################################################/--

volt <- function(x,t) {
	return(-20/3*(x-58.1)-2000/3*(x+69.6));
}

#dirField(volt,.005,10,-70,-65)

#/--#########################################################################
# name: Namita Gupta
# assignment: 4
# date: 2009
# question: 3
# subquestion: c
# other files: NAMES_OF_OTHER_RELATED_FILES
##########################################################################/--

#curve(0*exp(-673.3*x)-68.3,0,10)
#curve(-40*exp(-673.3*x)-68.3,0,.02)
#curve(-75*exp(-673.3*x)-68.3,0,.01)

#/--#########################################################################
# name: Namita Gupta
# assignment: 4
# date: 2009
# question: 4
# subquestion: a
# other files: NAMES_OF_OTHER_RELATED_FILES
##########################################################################/--

#define function F(x: REAL)
#var dx, dy: REAL
#var zeros : REAL array
#var signs : INT array (+/- 1)
#
#signs[0] <- abs(F(zeros[0]-1))/F(zeros[0]-1)
#
#for i <- 0 to length(zeros)-2 do
#begin
#	signs[i+1] <- abs(F((zeros[i]+zeros[i+1])/2))/F((zeros[i]+zeros[i+1])/2)
#end
#
#signs[length(zeros)] <- abs(F(zeros[length(zeros)-1]+1))/F(zeros[length(zeros)-1]+1)
#
#plot F(x)
#
#if signs[0] > 0 do
#begin
#	draw_arrow((zeros[0]-dx,dy) , (zeros[0],dy))
#end
#else do
#begin
#	draw_arrow((zeros[0], -dy) , (zeros[0]-dx, -dy))
#end
#
#for j <- 0 to length(zeros)-2 do
#begin
#	if signs[i+1]>0 do
#	begin
#		draw_arrow((zeros[i], dy) , (zeros[i+1], dy))
#	end
#	else do
#		draw_arrow((zeros[i+1], -dy) , (zeros[i], -dy))
#	end
#end
#
#if signs[length(zeros)] > 0 do
#begin
#	draw_arrow((zeros[length(zeros)-1],dy) , (zeros[length(zeros)-1]+dx,dy))
#end
#else do
#begin
#	draw_arrow((zeros[length(zeros)-1]+dx, -dy) , (zeros[length(zeros)-1], -dy))
#end
#end

#/--#########################################################################
# name: Namita Gupta
# assignment: 4
# date: 2009
# question: 4
# subquestion: b
# other files: NAMES_OF_OTHER_RELATED_FILES
##########################################################################/--

f <- function(x) {
	return(x/3 * (1-x/5));
}

dx <- 1
dy <- .05
zeros <- c(0,5)
signs <- numeric(length(zeros)+1)
signs[1] <- abs(f(zeros[1]-1))/f(zeros[1]-1)
signs[2] <- abs(f((zeros[1]+zeros[2])/2))/f((zeros[1]+zeros[2])/2)

#for (i in 1:length(zeros)-1) {
#	signs[i+1] <- abs(f((zeros[i]+zeros[i+1])/2))/f((zeros[i]+zeros[i+1])/2)
#}

signs[length(zeros)+1] <- abs(f(zeros[length(zeros)]+1))/f(zeros[length(zeros)]+1)
curve(f,-2,7)
if (signs[1] > 0) {
	arrows(zeros[1]-dx,dy , zeros[1],dy)
} else {
	arrows(zeros[1], -dy , zeros[1]-dx, -dy)
}

if (signs[2]>0) {
		arrows(zeros[1], dy , zeros[2], dy)
	} else {
		arrows(zeros[2], -dy , zeros[1], -dy)
	}

#for (j in 1:length(zeros)-1) {
#	if (signs[i+1]>0) {
#		arrow(zeros[i], dy , zeros[i+1], dy)
#	} else {
#		arrows(zeros[i+1], -dy , zeros[i], -dy)
#	}
#}

if (signs[length(zeros)+1] > 0) {
	arrows(zeros[length(zeros)],dy , zeros[length(zeros)]+dx,dy)
} else {
	arrows(zeros[length(zeros)]+dx, -dy , zeros[length(zeros)], -dy)
}


