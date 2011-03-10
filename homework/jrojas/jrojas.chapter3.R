#!/usr/bin/env r
# encoding: utf-8
# 
# name: Jose Rojas
# assignment: 3
# date: November 5 2009
# filename: jrojas3.R
#############################################################################



 # ARGUMENTS model function, plot window limits, step lengths in
 #           the x and t axes, an arrow length paramter h,
 # RETURTNS  A direction field describing the model

dirField <- function (model, dt, dx, tAxis, xAxis, h=dt*(8/10)){
  ## calculate appropriate x and t steps to cover plot window
  xs <- seq(xAxis[1],xAxis[2],dx)
  ts <- seq(tAxis[1],tAxis[2],dt)

  ## initialize plot
  plot(1,xlim=tAxis, ylim=xAxis,type="n")
  
  ## main loop
  for (t in ts){
    for (x in xs){
      m <- model(x,t)
      arrows(t,x,t+h,x+h*m,length=h/10)	
    }
  }
}

 ## Example Call ##
exampleFun <- function(x,t)
	1/90 * x * (90-x)
dirField(exampleFun,0.3,3,c(0,9),c(-20,130))


#/--#########################################################################
# name: Jose Rojas
# assignment: 3
# date: Novermber 5 2009
# question: 1
# subquestion:
# other files:
##########################################################################/--


f <- function(N,t){
  10*N * (N/20 - 1) * (1 - N/200)
}
curve(f,-10,210)
arrows(18,0,2,0,col="red",length=0.05,lwd=2)
arrows(22,0,198,0,col="red",length=0.05,lwd=2)
arrows(208,0,202,0,col="red",length=0.05,lwd=2)

g <- function(N){
  10*N^2 * (1 - N/200)
}
curve(g,-10,210)
arrows(2,0,198,0,col="red",length=0.05,lwd=2)
arrows(208,0,202,0,col="red",length=0.05,lwd=2)


#/--#########################################################################
# name: Jose Rojas
# assignment: 3
# date: Novermber 5 2009
# question: 2
# subquestion: b
# other files:
##########################################################################/--

## fixed points and their stability condistions for r = 0.3
r <- 0.3
fixedPoints <- function (t) r * sin( (4*t) / (2*pi) )
curve(fixedPoints,0,16)

stabilityCondition <- function (t) r - (4/(2*pi)) * cos((4*t) / (2*pi))
lines(0:16,stabilityCondition(0:16))

## fixed points and their stability condistions for r = 1
r <- 1
fixedPoints <- function (t) r * sin( (4*t) / (2*pi) )
curve(fixedPoints,0,16)

stabilityCondition <- function (t) r - (4/(2*pi)) * cos((4*t) / (2*pi))
lines(0:16,stabilityCondition(0:16))

                              
#/--#########################################################################
# name: Jose Rojas
# assignment: 3
# date: Novermber 5 2009
# question: 2
# subquestion: c
# other files:
##########################################################################/--

## Direction field
r <- 1
bacteria <- function (N,t) {r * N - sin( (4*t) / (2*pi) )}
dirField(bacteria,1,.1,c(0,14),c(-3,3))

## Fixed point changes with time
fpRange <- seq(0,14,.1)
lines(fpRange,fixedPoints(fpRange),lwd=2,col="red")
## It's clear that the fixed point is not stable anywhere

                              
#/--#########################################################################
# name: Jose Rojas
# assignment: 3
# date: Novermber 5 2009
# question: 2
# subquestion: d
# other files:
##########################################################################/--

r<-1
BacteriaSol <- function (t){
  (2 * pi * cos((4*t)/(2*pi)) + pi^2 * r * sin((4*t)/(2*pi))) /
   (pi^2 * r^2 +4) +
   exp(r*t) * N0 * ((pi^2 * r^2 +4) / (4 - 2*pi))
}

dirField(bacteria,.6,0.3,c(0,12),c(-3,20))
fpRange <- seq(0,14,0.1)
N0 <- 0.0001
lines(fpRange,BacteriaSol(fpRange),lwd=2,col="red")
N0 <- 0
lines(fpRange,BacteriaSol(fpRange),lwd=2,col="red")
N0 <- -0.0001
lines(fpRange,BacteriaSol(fpRange),lwd=2,col="red")

 # The solutions clearly behave as the direction field predicts that they
 # would. At N0 = 0 the solution is in an unstable equilibrium, but even
 # small perturbations send the solution curves skyrocketing.

#/--#########################################################################
# name: Jose Rojas
# assignment: 3
# date: Novermber 5 2009
# question: 3
# subquestion: b
# other files:
##########################################################################/--

## Constants
C <- 0.15
gNa <- 1
gK <- 100
VNa <- 58.1
VK <- -69.6

voltage <- function (V,t) {
  (1/C) * (-gNa * (V - VNa) - gK * (V - VK))
}

dirField(voltage,.1,.005,c(0,1),c(-69,-67))
## unstable fixed point at -68.34 confirms my analysis

voltageSolution <- function (t) {
  (gNa * VNa + gK * VK) / (gNa + gK) +
  (V0 - (gNa * VNa + gK * VK) / (gNa + gK)) *
  exp(((gNa+gK)/C)*t)
}

fpRange <- seq(0,1,0.01)
V0 <- (gNa * VNa + gK * VK) / (gNa + gK)
lines(fpRange,voltageSolution(fpRange),lwd=2,col="red")
V0 <- (gNa * VNa + gK * VK) / (gNa + gK) + 0.000001
lines(fpRange,voltageSolution(fpRange),lwd=2,col="red")

## I'd plot it in log scale but I can't figure out par
## I'll ask you about it on Thursday


#/--#########################################################################
# name: Jose Rojas
# assignment: 3
# date: Novermber 5 2009
# question: 4
# subquestion: b
# other files:
##########################################################################/--


 # ARGUMENTS function, number of samples to take for plot,
 #           a vector containing minumum and maximum values for x,
 # RETURTNS  A flow line diagram describing the function

flowLines <- function (func, samples, xaxis){
  x <- seq(xaxis[1],xaxis[2],length.out=samples)
  y <- func(x)

  ## Curve
  plot(x,y,xlim=xaxis,type="l",col="blue")
  ## Lines for x and y axes
  segments(xaxis[1],0,xaxis[2],0,
           lwd=2)
  segments(0,max(y),0,min(y),
           lwd=2)

  ## Flow lines
  offset <- diff(xaxis)/100)
  start <- xaxis[1]
  for (i in 1:(length(y)-1)){
    if (y[i] * y[i+1] < 0){
      if (y[i] > y[i+1]) arrows(start, offset, x[i], offset,
                                length=0.05, col="red", lwd=2)
      else arrows(x[i], -offset, start, -offset,
                   length=0.05, col="red", lwd=2)
      start <- x[i+1]
    }
  }
  if (y[length(y)] > 0) arrows(start, offset, xaxis[2], offset,
                               length=0.05, col="red", lwd=2)
  else  arrows(xaxis[2], -offset, start, -offset,
               length=0.05, col="red", lwd=2)
}

f <- function(x)  {(x/3)*(1-x/5)}

flowLines(f,100,c(-1,6))
