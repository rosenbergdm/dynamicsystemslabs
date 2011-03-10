#!/usr/bin/env r
# encoding: utf-8
# 
# name: Patrick Mann
# assignment: 4
# date: 10-30-09
# filename: Chapter 3 Exercises
#############################################################################


#/--#########################################################################
# name: Patrick Mann
# assignment: 4
# date: 10-30-09
# question: 1
# subquestion: b
# other files: NAMES_OF_OTHER_RELATED_FILES
##########################################################################/--

U <- 20
r <- 10
K <- 200
popGrow_1 <- function(N) {
	return(r * N * (N/U - 1) * (1 - N/K) )
}

curve(popGrow_1, main='Model 1',
	  xlab='N', ylab='f(N)',
	  xlim=c(-50, 250),
	  ylim=c(-100, 3000)
)

lines(-100:300,0*(-100:300))

# Population grows for the intervals {N < 0, 20 < N < 200}
# Population decays for the intervals {0 < N < 20, n > 200}

popGrow_2 <- function(N) {
	return(r * N * N * (1 - N/K) )
}

curve(popGrow_2, main='Model 1',
	  xlab='N', ylab='f(N)',
	  xlim=c(-50, 250),
	  ylim=c(-100, 60000)
)

lines(-100:300,0*(-100:300))

# Population grows for the interval {N < 200, N != 0}
# Population decays for the interval {N > 200}

#/--#########################################################################
# name: Patrick Mann
# assignment: 4
# date: 10-30-09
# question: 1
# subquestion: c
# other files: NAMES_OF_OTHER_RELATED_FILES
##########################################################################/--

#_The population rate of change f(N) demonstrates two stable fixed points, 
#_one at N=0 and one at N=200.  The population rate of change g(N) 
#_demonstrates only one stable fixed point at N=200.  This indicates that a 
#_population whose change in size is defined by f(N) can either decay 
#_towards extinction or approach a carrying capacity. On the other hand, 
#_the population whose growth is defined by g(N) will only approach a 
#_carrying capacity.  

#_The population model defined by rate f(N) will have a tendency to decay 
#_to zero at smaller population sizes (N<20), but if the population starts 
#_at a sufficiently large enough initial size (N>20), it will stabilize at 
#_a carrying capacity of 200.  The population model defined by rate g(N) 
#_will always approach a stable carrying capactiy of 200 for any positive 
#_starting population size (except N=20 where the population will neither 
#_grow nor decay).

#/--#########################################################################
# name: Patrick Mann
# assignment: 4
# date: 10-30-09
# question: 2
# subquestion: c
# other files: NAMES_OF_OTHER_RELATED_FILES
##########################################################################/--

n_0 <- 0
f <- exampleFun1 <- function(x, t) {
	return( x - sin(4*t/(2*pi)) )
}

tRange <- c(-10, 10)
dt <- 0.75
xRange <- c(-10, 10)
dx <- 0.75
h <- 0.2
k <- 1
tSteps <- diff(range(tRange)) / dt
xSteps <- diff(range(xRange)) / dx

plot(c(0, 10), c(-10, 10), type='n',
	main='f(x,t)',
	xlab='t', ylab='x',)
for (ii in 0:(tSteps-1)) {
	for (jj in -(xSteps-1):(xSteps-1)){
		t <- ii * dt
		x <- jj * dx
		m <- f(x, t)
		u <- t + (h * dt)
		v <- x + (k * h * dt) * m
		arrows(t, x, u, v, code=2,
			length=0.05)
	}
}

#/--#########################################################################
# name: Patrick Mann
# assignment: 4
# date: 10-30-09
# question: 2
# subquestion: d
# other files: NAMES_OF_OTHER_RELATED_FILES
##########################################################################/--

e <- 2.718281828
n_0 <- 0
bacCult <- function(t) {
	return( ( -1 * (sin(2 * t / pi) + (2/pi) * cos(2 * t / pi) ) /
			(4 / pi^2) 
			) +
			n_0 * e^(t)	- (pi/2) * e^(t)	   
	)
}

plot(c(-10, 10), c(10, -250), type='n',
	main='Bacteria Culture Equation',
	xlab='t', ylab='N(t)'
)

curve(bacCult, type='l', col='green')

n_0 <- 10
x <- c((1:100) * 0.1)
y <- bacCult(x)
points(x, y, type='p')

n_0 <- -50
x <- c((1:100) * 0.2)
y <- bacCult(x)
points(x, y, type='p', col='red')

#_The curves follow the course of a set of arrows in the direction field, 
#_starting from one arrow and moving to the next arrow it points to, and 
#_the next, and so on.

#/--#########################################################################
# name: Patrick Mann
# assignment: 4
# date: 10-30-09
# question: 3
# subquestion: b
# other files: NAMES_OF_OTHER_RELATED_FILES
##########################################################################/--

e <- 2.718281828
n_0 <- 0
f <- exampleFun1 <- function(x, t) {
	return( (-6901.9 - 101 * x) / 0.15
	)
}

tRange <- c(0, 10)
dt <- 1
xRange <- c(60, 90)
dx <- 1
h <- 0.0001
tSteps <- diff(range(tRange)) / dt
xSteps <- diff(range(xRange)) / dx

plot(c(0, 10), c(-60, -80), type='n',
	main='f(x,t)',
	xlab='t', ylab='v')
for (ii in -(tSteps-1):(tSteps-1)) {
	for (jj in -(xSteps-1):(xSteps-1)){
		t <- ii * dt
		x <- -60 + jj * dx
		m <- f(x, t)
		u <- t + (h * dt)
		v <- x + (h * dt) * m
		arrows(t, x, u, v, code=2,
			length=0.05)
	}
}

#/--#########################################################################
# name: Patrick Mann
# assignment: 4
# date: 10-30-09
# question: 3
# subquestion: c
# other files: NAMES_OF_OTHER_RELATED_FILES
##########################################################################/--

e <- 2.718281828
v_0 <- 0
volt <- function(t) {
	return( -68.3356 * (1 + e^(-673.333 * t) ) 
		   + v_0 * e^(-673.333 * t)
		   )
	}
plot(c(0, 10), c(-60, -80), type='n',
	main='Two Voltage Dependent Ion Channel Equation',
	xlab='t', ylab='V(t)'
	)
curve(volt, type='l', col='green')
v_0 <- 10
x <- c((1:100) * 0.1)
y <- volt(x)
points(x, y, type='p')
v_0 <- -50
x <- c((1:100) * 0.2)
y <- volt(x)
points(x, y, type='p', col='red')

#_long term behavior will always equilibrate around the 
#_fixed point, V = -68.3356

#_Analytical solution was not necessary to come to the above conclusion

#/--#########################################################################
# name: Patrick Mann
# assignment: 4
# date: 10-30-09
# question: 4
# subquestion: a
# other files: NAMES_OF_OTHER_RELATED_FILES
##########################################################################/--

#_define function F(x: REAL)
#_var x_min, x_max, F(x)_min, F(x)_max: REAL   // Plot limits
#_var x: REAL			   	       // Temporary Placeholders
#_init plot
	
#_arrows(x_min:1st_F(x)=0) 		      // Depending on Direction of 
#_						 F(x)=+ ==> F(x)=-
#_OR 								
#_arrows(1st_F(x)=0:x_min)

#_arrows(1st_F(x)=0:2nd_F(x)=0) 
#_OR
#_arrows(2nd_F(x)=0:1st_F(x)=0)

#_arrows(2nd_F(x)=0:x_max)
#_OR
#_arrows(x_max:1st_F(x)=0)

#/--#########################################################################
# name: Patrick Mann
# assignment: 4
# date: 10-30-09
# question: 4
# subquestion: b
# other files: NAMES_OF_OTHER_RELATED_FILES
##########################################################################/--

logModel <- function(x) {
	return( (x/3) * (1 - (x/5) ) )
}

curve(logModel, type='l', col='blue', xlim=c(-1,7))

lines(0*(-1:1), -1:1)
lines(-2:7, 0*(-2:7))

arrows(-0.2, -0.01, -0.9, -0.01, col='red', code=2, length=0.05)
arrows(0.2, 0.01, 4.8, 0.01, col='red', code=2, length=0.05)
arrows(6, -0.01, 5.2, -0.01, col='red', code=2, length=0.05)