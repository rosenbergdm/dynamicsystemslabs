#!/usr/bin/env r
# encoding: utf-8
# 
# name: Crnich, Joseph
# assignment: 4
# date: 10/30/2009
# filename: Joey_Crnich_Lab_4_Answers.R
#############################################################################


#/--#########################################################################
# name: Crnich, Joseph
# assignment: 4
# date: 10/30/2009
# question: 1
# subquestion: a
# other files: 
##########################################################################/--

## •N = f(N) = rN(N/U - 1)(1 - N/K)
## 0 = rN(N/U - 1)(1 - N/K);		N = 0
## 0 = (N/U - 1)(1 - N/K)
## 0 = N/U - 1	;	0 = 1 - N/K	
## N = U		;	N = K

## f(N) = ((r/U) * N^2 - rN)(1 - N/K)
## f(N) = (r/U) * N^2 - rN + (r/K) * N^2 - (r/UK) * N^3
## f'(N) = 2r/U * N - r + 2r/K * N - 3r/UK * N^2
	## f'(0) = -r
	## f'(U) = 2r - r + 2rU/K - 3rU/K = r(1 - U/K)
	## f'(K) = 2rK/U - r + 2r - 3rK/U = r(1 - K/U)

## •N = g(N) = rN^2 * (1 - N/K)
## 0 = rN^2 * (1 - N/K);		N = 0
## 0 = 1 - N/K
## N = K

## g(N) = rN^2 * (1 - N/K)
## g(N) = rN^2 - (r/K) * N^3
## g'(N) = 2rN - (3r/K) * N^2
	## g'(0) = 0
	## g'(K) = 2rK - 3rK = -rK

#/--#########################################################################
# name: Crnich, Joseph
# assignment: 4
# date: 10/30/2009
# question: 1
# subquestion: b
# other files: 
##########################################################################/--

## For the function f(N):

r <- 10
K <- 200
U <- 20
nDotF <- function(N) {
	return(r * N * (N / U - 1) * (1 - N / K));
}

max_iter <- 1000
y_vector <- numeric(max_iter)
x_vector <- c(1:max_iter) / 5
for(ii in 1:max_iter)	{
	y_vector[ii] <- nDotF(x_vector[ii])
}

plot(x_vector, y_vector, xlab='N', ylab='f(N)',
    main='Function •N = f(N)', type='l', col='turquoise3',
    xlim=c(0, 210), ylim=c(-100,2600),
    xaxs='i', yaxs='i')
abline(0, 0)
points(0,nDotF(0))
points(U,nDotF(U))
points(K,nDotF(K))

## The population is growing where f(N) is positive - therefore, the
## population is growing when:	20 < N < 200, or U < N < K.



## For the function g(N):

r <- 10
K <- 200
nDotG <- function(N) {
	return(r * N^2 * (1 - N / K));
}

max_iter <- 1000
y_vector <- numeric(max_iter)
x_vector <- c(1:max_iter) / 5
for(ii in 1:max_iter)	{
	y_vector[ii] <- nDotG(x_vector[ii])
}

plot(x_vector, y_vector, xlab='N', ylab='f(N)',
    main='Function •N = g(N)', type='l', col='turquoise3',
    xlim=c(0, 210), ylim=c(-100,60000),
    xaxs='i', yaxs='i')
points(0,nDotG(0))
points(K,nDotG(K))

## The population is growing when:	0 < N < 200, or 0 < N < K.



#/--#########################################################################
# name: Crnich, Joseph
# assignment: 4
# date: 10/30/2009
# question: 1
# subquestion: c
# other files: 
##########################################################################/--

## In the population defined by f(N), at small N (N < U, to be precise) the
## population declines to zero, while the population defined by g(N) always
## has positive growth. In biological terms, f(N) is similar to populations
## with Allee effects - that is, at small population sizes, a positive
## relationship between population size N and rate of growth f(N). One
## example would be species that free spawn; when the population dips below a
## certain number, there are not enough individuals spawning for the gametes
## to mingle in the water and fertilize, thus leading to decrease in
## population size.

#/--#########################################################################
# name: Crnich, Joseph
# assignment: 4
# date: 10/30/2009
# question: 2
# subquestion: a
# other files: 
##########################################################################/--

## If the rate is proportional (r) to the population size (N), then the
## geometric growth model is appropriate, less the bacteria removed (d(t)):
## •N = rN - d(t)
## •N = rN - sin(4t/2π)

#/--#########################################################################
# name: Crnich, Joseph
# assignment: 4
# date: 10/30/2009
# question: 2
# subquestion: b
# other files: 
##########################################################################/--

## •N = f(N) = rN - sin(4t/2π)
	## 0 = rN - sin(4t/2π)
	## rN = sin(4t/2π)
	## N = sin(4t/2π)/r

## f'(N) = r

## Any fixed point over the whole function would be unstable because the
## second derivative is always positive (population always growing, r > 0).
## The only 'fixed point' is at N=0, t=0, but this is impractical for several
## reasons. First, a population can't grow if it doesn't exist (N=0). Second,
## the student researchers cannot remove any bacteria if none exist. Finally,
## as our population is growing over time, t will inherently change and
## cannot stay fixed. Thus, no real, practical fixed points exist.

#/--#########################################################################
# name: Crnich, Joseph
# assignment: 4
# date: 10/30/2009
# question: 2
# subquestion: c
# other files: 
##########################################################################/--

r <- 1
bacFunc <- function(N, t) {
	return(r * N - sin((4 * t) / (2 * pi)))
}

tRange <- c(0, 10)
dt <- 0.5
NRange <- c(0, 50)
dN <- 2.5
h <- 0.8
k <- 0.5

tSteps <- diff(range(tRange)) / dt
NSteps <- diff(range(NRange)) / dN

plot(c(0,9), c(-45,45), type='n',
	xlab='t', ylab='N',
    main='f(N) = 10N - sin(4t/2π)',
    xaxs='i', yaxs='i')

for (ii in -(tSteps - 1):(tSteps - 1)) {
	for (jj in -(NSteps - 1):(NSteps - 1)) {
		t <- ii * dt
		N <- jj * dN
		m <- bacFunc(N, t)
		u <- t + (h * dt)
		v <- N + (k * h * dt) * m
		arrows(t, N, u, v, code=2, length=0.05)
	}
}

## The only fixed point is at N=0, t=0. Any deviation upwards (N>0) causes
## the function to grow very rapidly away from zero. Any deviation forward in
## time (at N=0) creates a wave function (zooming in on the y axis from -.1
## to .1 reveals this). For these reasons, the 'fixed point' is unstable.

#/--#########################################################################
# name: Crnich, Joseph
# assignment: 4
# date: 10/30/2009
# question: 2
# subquestion: d
# other files: 'Handwritten integration of ∫-sin(4t/2π)e^-10t dt'
##########################################################################/--

## •N = f(N) = rN - sin(4t/2π)
## dN/dt = rN - sin(4t/2π)		# Using the integration factor: e^-∫a(t)dt,
	## dN/dt - rN = -sin(4t/2π)					# where a(t) = r ...
	## dN/dt*e^-rt - rN*e^-rt = -sin(4t/2π)*e^-rt
	## d/dt(N(t)*e^-rt) = -sin(4t/2π)*e^-rt
	## N(t)*e^-rt = ∫(-sin(4t/2π)*e^-rt)dt + C      # (cf. additional papers)
	## N(t)*e^-rt = (4π/(16+4r^2 * π^2))(rπsin(4t/2π) + 2cos(4t/2π))(e^-rt)+C
	## N(t) = (4π / (16+4r^2 * π^2))(rπsin(4t/2π) + 2cos(4t/2π)) + Ce^rt

	## N(0) = (4π / (16+4r^2 * π^2))(rπsin(4t/2π) + 2cos(4t/2π)) + Ce^rt
	## N(0) = (8π / (16+4r^2 * π^2)) + C
	## C = N(0) - (8π / (16+4r^2 * π^2))


r <- 10
init_N <- 10
bFunc <- function(t) {
	C <- init_N - (8 * pi / (16 + 4 * r^2 * pi^2));
	return((4 * pi^2 / (16 + (4 * r) * pi^2)) * (sin(4 * t / (2 * pi)) + 
	(4 / (2 * pi)) * cos(4 * t / (2 * pi))) + C * exp(r * t));
}
max_iter <- 10000
y_vector <- numeric(max_iter + 1)
x_vector <- c(0:max_iter) /100
for(ii in 1:max_iter)	{
	y_vector[ii] <- bFunc(x_vector[ii])
}
plot(x_vector, y_vector,
	xlab='t', ylab='N(t)',
    main='Bacterial Growth: N(0) = 10',
    type='l', col='turquoise3',
    xlim=c(0, 2), ylim=c(0,1000),
    xaxs='i', yaxs='i')
abline(0,0)

####################################

init_N <- 1

y_vector <- numeric(max_iter + 1)
x_vector <- c(0:max_iter) / 100
for(ii in 1:max_iter)	{
	y_vector[ii] <- bFunc(x_vector[ii])
}

plot(x_vector, y_vector,
	xlab='t', ylab='N(t)',
    main='Bacterial Growth: N(0) = 1',
    type='l', col='turquoise3',
    xlim=c(0, 2), ylim=c(0, 1000),
    xaxs='i', yaxs='i')
abline(0,0)

####################################

init_N <- 0.1

y_vector <- numeric(max_iter + 1)
x_vector <- c(0:max_iter) / 100
for(ii in 1:max_iter)	{
	y_vector[ii] <- bFunc(x_vector[ii])
}

plot(x_vector, y_vector,
	xlab='t', ylab='N(t)',
    main='Bacterial Growth: N(0) = 0.1',
    type='l', col='turquoise3',
    xlim=c(0, 2), ylim=c(0, 1000),
    xaxs='i', yaxs='i')
abline(0,0)


## These solutions show that at almost any initial condition, the population
## will always grow to infinity (because an exponential is embedded in the
## equation). The only time where this not the case is C < 0, or C = 0, as
## shown below. This behavior is the same as that identified by our direction
## field in that any C != 0 will grow to (+/-) infinity. The only 'fixed
## point' exists at certain periods where [N = sin(4t/2π)/r] when N(0) =
## (8π/(16 + 4r^2 * π^2)). In the case below, this number is 0.006340501.
	## Of course, neither of these cases for C yields biologically applicable	## results, as a population cannot dip below zero.

init_N <- (8 * pi / (16 + 4 * r^2 * pi^2))

y_vector <- numeric(max_iter + 1)
x_vector <- c(0:max_iter) / 100
for(ii in 1:max_iter)	{
	y_vector[ii] <- bFunc(x_vector[ii])
}

plot(x_vector, y_vector,
	xlab='t', ylab='N(t)',
    main='Bacterial Growth: N(0) = (8π/(16 + 4r^2 * π^2))',
    type='l', col='turquoise3',
    xlim=c(0, 25), ylim=c(-0.15, 0.15),
    xaxs='i', yaxs='i')
abline(0,0)

####################################

init_N <- 0

y_vector <- numeric(max_iter + 1)
x_vector <- c(0:max_iter) / 100
for(ii in 1:max_iter)	{
	y_vector[ii] <- bFunc(x_vector[ii])
}

plot(x_vector, y_vector,
	xlab='t', ylab='N(t)',
    main='Bacterial Growth: N(0) = 0',
    type='l', col='turquoise3',
    xlim=c(0, 2), ylim=c(-1000, 0),
    xaxs='i', yaxs='i')
abline(0,0)


#/--#########################################################################
# name: Crnich, Joseph
# assignment: 4
# date: 10/30/2009
# question: 3
# subquestion: a
# other files: 
##########################################################################/--

## C•V = -gNA * (V - VNA) - gK * (V - VK)
	## 0 = -gNA * (V - VNA) - gK * (V - VK)
	## 0 = -gNAV + gNAVNA - gKV + gKVK
	## 0 = gNAVNA + gKVK - V(gNA + gK)
	## V(gNA + gK) = gNAVNA + gKVK
	## V = (gNAVNA + gKVK)/(gNA + gK)

	## V = (1 * 58.1 + 100 * -69.6)/(1 + 100)
	## V = (58.1 - 6960)/(101)
	## V = -6901.9/101
	## V = -68.335644
	
## C•V = -gNA * (V - VNA) - gK * (V - VK)
	## F(V) = (-gNA * (V - VNA) - gK * (V - VK))/C
	## F(V) = (-gNA*V + gNA*VNA - gK*V + gK*VK)/C
	## F(V) = (gNA*VNA + gK*VK - V(gK + gNA))/C
	## F(V) = (gNA*VNA + gK*VK)/C - V(gK + gNA)/C
	## f'(V) = -(gK + gNA)/C

	## f'(V) = -(100 + 1)/0.15
	## f'(V) = -101/0.15
	## f'(V) = -1128.9043

## Because the resulting derivative is both nonhomogeneous and autonomous,
## the fixed point is only dependent upon the constant parameters. With these
## parameters, the fixed point is stable, as the derivative is negative.



#/--#########################################################################
# name: Crnich, Joseph
# assignment: 4
# date: 10/30/2009
# question: 3
# subquestion: b
# other files: 
##########################################################################/--

gNA <- 1
gK <- 100
VNA <- 58.1
VK <- -69.6
C <- 0.15
membFunc <- function(V, t) {
	return((-gNA * (V - VNA) - gK * (V - VK)))
}

tRange <- c(0, 1)
dt <- 0.05
VRange <- c(-110, 0)
dV <- 5
h <- 0.8
k <- 0.02

tSteps <- diff(range(tRange)) / dt
VSteps <- diff(range(VRange)) / dV

plot(c(0,1), c(-100,0), type='n',
	xlab='t', ylab='V',
    main='V(t)',
    xaxs='i', yaxs='i')

for (ii in -(tSteps - 1):(tSteps - 1)) {
	for (jj in -(VSteps - 1):(VSteps - 1)) {
		t <- ii * dt
		V <- jj * dV
		m <- membFunc(V, t)
		u <- t + (h * dt)
		v <- V + (k * h * dt) * m
		arrows(t, V, u, v, code=2, length=0.05)
	}
}

## The directional field shows slopes that guide the value of V towards the
## fixed point at -68.335644, agreeing with the analytical stability above.

#/--#########################################################################
# name: Crnich, Joseph
# assignment: 4
# date: 10/30/2009
# question: 3
# subquestion: c
# other files: 
##########################################################################/--

## C•V = -gNA * (V - VNA) - gK * (V - VK)
	## dV/dt = (gNA*VNA + gK*VK)/C - V(gK + gNA)/C
	## dV/dt - V(-gK - gNA)/C = (gNA*VNA + gK*VK)/C
	## dV/dt*e^(gK + gNA)t/C - V*e^(gK + gNA)t/C * (gK + gNA)/C =
		##		(gNA*VNA + gK*VK)/C * e^(gK + gNA)t/C
	## d/dt(V(t)*e^(gK + gNA)t/C) = (gNA*VNA + gK*VK)/C * e^(gK + gNA)t/C
	## V(t)*e^(gK + gNA)t/C = ∫(gNA*VNA + gK*VK)/C * e^(gK + gNA)t/C
	## V(t)*e^(gK + gNA)t/C = (gNA*VNA + gK*VK)/C * ∫e^(gK + gNA)t/C
	## V(t)e^(gK+gNA)t/C = (gNAVNA+gKVK)/C * (C/(gK+gNA))e^(gK+gNA)t/C + A
	## V(t) = (gNA*VNA+gK*VK)/(gK+gNA) + Ae^-(gK+gNA)t/C

	## V(0) = (gNA*VNA+gK*VK)/(gK+gNA) + A
	## A = V(0) - (gNA*VNA+gK*VK)/(gK+gNA)

gNA <- 1
gK <- 100
VNA <- 58.1
VK <- -69.6
C <- 0.15
init_V <- 1000
mFunc <- function(t) {
	A <- init_V - ((gNA * VNA + gK * VK) / (gK + gNA));
	return((gNA * VNA + gK * VK) / (gK + gNA) + 
		A * exp(-((gK + gNA) * t) / C));
}
max_iter <- 10000
y_vector <- numeric(max_iter + 1)
x_vector <- c(0:max_iter) /10000
for(ii in 1:(max_iter+1))	{
	y_vector[ii] <- mFunc(x_vector[ii])
}

plot(x_vector, y_vector,
	xlab='t', ylab='N(t)',
    main='Membrane Voltage: V(0) = 1000',
    type='l', col='turquoise3',
    xlim=c(0, 0.02), ylim=c(-110, 1000),
    xaxs='i', yaxs='i')
abline(0,0)

####################################

init_V <- -1000

y_vector <- numeric(max_iter + 1)
x_vector <- c(0:max_iter) / 10000
for(ii in 1:(max_iter+1))	{
	y_vector[ii] <- mFunc(x_vector[ii])
}

plot(x_vector, y_vector,
	xlab='t', ylab='N(t)',
    main='Membrane Voltage: V(0) = -1000',
    type='l', col='turquoise3',
    xlim=c(0, 0.02), ylim=c(-1000, 0),
    xaxs='i', yaxs='i')
abline(0,0)

####################################

init_V <- -68.33564

y_vector <- numeric(max_iter + 1)
x_vector <- c(0:max_iter) / 10000
for(ii in 1:(max_iter+1))	{
	y_vector[ii] <- mFunc(x_vector[ii])
}

plot(x_vector, y_vector,
	xlab='t', ylab='N(t)',
    main='Membrane Voltage: V(0) = -68.33564',
    type='l', col='turquoise3',
    xlim=c(0, 0.02), ylim=c(-100, 0),
    xaxs='i', yaxs='i')
abline(0,0)

## In each of these cases (V(0) >, <, and = to the fixed point), the graph
## tends towards the fixed point, mirrorring the direction field perfectly.
## As both the direction field and fixed point analysis from parts a) and b)
## gave this result, the analytical solution wasn't exactly necessary, though
## it does show us exactly how the voltage approaches the fixed point with 
## differing initial voltages over time.

#/--#########################################################################
# name: Crnich, Joseph
# assignment: 4
# date: 10/30/2009
# question: 4
# subquestion: a
# other files: 
##########################################################################/--

## 1. Define a function f(x) that represents the derivative of x(t)
## 
## 2. Choose a domain defined by min_iter and max_iter
## 
## 3. Define vector of x values from min_iter to max_ter, x_vec
## 
## 4. Define numeric vector of y values the length of the difference between
## max_iter and min_iter (plus one if zero is between the two).
## 
## 5. Run a for loop to define each value in y_vec as the output of the
## function f(x) for its corresponding value in x_vec
## 
## 6. Plot the function (with axes would be useful)
## 
## 7. Define a vector of the indices of the fixed points, fix_vec, using the
## which command
## 
## 8. Run one if loop with respect to the first fixed point containing two
## possibilities:
## 			a) if the point just before the fixed point is positive, draw an
## 			arrow from some number lower than the x of the fixed point to the
## 			x at the fixed point (with the same y so the arrow is level)
## 	else	b) if the point just before the fixed point is negative, draw an
## 			arrow from the x of the fixed point to some number lower than the ## 			x of the fixed point
## 
## 8. Run a for loop from the first fixed point to the last containing:
## 		a) another if loop similar to the previous but checking the point
## 		just after the fixed point:
## 				i) if the point just after the fixed point is positive, draw
## 				an arrow from the x of the fixed point to x of the next fixed
## 				point (using the fix_vec index numbers)
## 		else	ii) if the point just after the fixed point is negative, draw
## 				an arrow from x of the next fixed point to x of the current
## 				fixed point
## 
## 9. Because the loop above depends on two fixed points to draw an arrow,
## one last if loop is necessary for the final fixed point:
## 			a) if the point just after the fixed point is positive, draw an
## 			arrow from the x of the fixed point to some number higher than
## 			x of the fixed point
## 	else	b) if the point just after the fixed point is negative, draw an
## 			arrow from some number higher than the x of the fixed point to x ## 			at the fixed point

#/--#########################################################################
# name: Crnich, Joseph
# assignment: 4
# date: 10/30/2009
# question: 4
# subquestion: b
# other files: 
##########################################################################/--

flowFun <- function(x) {
	return((x / 3) * (1 - x / 5) );
}

max_iter <- 7000
min_iter <- -2000
y_vec <- numeric(max_iter - min_iter + 1)
x_vec <- c(min_iter:max_iter) / 1000
for(ii in 1:(max_iter - min_iter + 1))	{
	y_vec[ii] <- flowFun(x_vec[ii])
}

plot(x_vec, y_vec,
	xlab='x', ylab='f(x)',
    main='Flow Lines',
    type='l', col='turquoise3',
    xlim=c(x_vec[1], x_vec[length(x_vec)]),
    xaxs='i', yaxs='i')

abline(0,0)
xax <- numeric(length=1001)
yax <- c(-500:500)
lines(xax, yax)

fix_vec <- which(y_vec == 0)
if (y_vec[fix_vec[1] - 1] > 0) {
	arrows((x_vec[fix_vec[1]] - 2), .1, x_vec[fix_vec[1]], .1,
		col="red", length=.1, lwd=2)
} else if (y_vec[fix_vec[1] - 1] < 0) {
	arrows(x_vec[fix_vec[1]], -.1, (x_vec[fix_vec[1]] - 2), -.1,
		col="red", length=.1, lwd=2)
		}
for (ii in 1:length(fix_vec)) {
	if (y_vec[fix_vec[ii] + 1] > 0) {
		arrows(x_vec[fix_vec[ii]], .1, x_vec[fix_vec[ii+1]], .1,
			col="red", length=.1, lwd=2)
	} else if (y_vec[fix_vec[ii] + 1] < 0) {
		arrows(x_vec[fix_vec[ii+1]], -.1, x_vec[fix_vec[ii]], -.1,
			col="red", length=.1, lwd=2)
		}
	}
if ((y_vec[fix_vec[length(fix_vec)] + 1]) > 0) {
	arrows(x_vec[fix_vec[length(fix_vec)]], .1,
		(x_vec[fix_vec[length(fix_vec)]] + 2), .1,
		col="red", length=.1, lwd=2)
} else if ((y_vec[fix_vec[length(fix_vec)] + 1]) < 0) {
	arrows((x_vec[fix_vec[length(fix_vec)]] + 2), -.1,
		x_vec[fix_vec[length(fix_vec)]], -.1,
		col="red", length=.1, lwd=2)
		}
