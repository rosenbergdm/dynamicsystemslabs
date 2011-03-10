# name: Craig Biwer
# assignment: 4
# date: 10/30/2009
# filename: BiwerLab4.R
#############################################################################







#/--#########################################################################
# name: Craig Biwer
# assignment: Lab 4
# date: 10/30/2009
# question: 1
# subquestion: a
# other files:
##########################################################################/--

#f(N):
#Equilibria: 0 (stable), U (stable for U>K), K (stable for K>U)
#
#g(N):
#Equilibria: 0 (unstable), K (stable)


#/--#########################################################################
# name: Craig Biwer
# assignment: Lab 4
# date: 10/30/2009
# question: 1
# subquestion: b
# other files:
##########################################################################/--

#f(N)
#curve(10*x*(x/20 - 1)*(1-x/200), 0, 220)
#Increasing from 9.737 to 136.929, decreasing otherwise
#
#g(N)
#curve((10*x*x*(1-x/200)), 0, 220)
#Increasing from 0 to 133.33, decreasing otherwise


#/--#########################################################################
# name: Craig Biwer
# assignment: Lab 4
# date: 10/30/2009
# question: 1
# subquestion: c
# other files:
##########################################################################/--

#The principle difference between the two is that for f(N), the function
#originally decreases, going negative before returning positive (negative
#populations are non-physical). g(N), though, starts off increasing.


#/--#########################################################################
# name: Craig Biwer
# assignment: Lab 4
# date: 10/30/2009
# question: 2
# subquestion: a
# other files:
##########################################################################/--

#N' = r*N - Sin(4t / (2pi))


#/--#########################################################################
# name: Craig Biwer
# assignment: Lab 4
# date: 10/30/2009
# question: 2
# subquestion: b
# other files:
##########################################################################/--

#Fixed points depend on t: occur at (1/r)*(Sin(2t/pi)), but they're unstable


#/--#########################################################################
# name: Craig Biwer
# assignment: Lab 4
# date: 10/30/2009
# question: 2
# subquestion: c
# other files:
##########################################################################/--

dirField <- function(f, h, tf, x0, xl){
	tRange <- c(0, tf)
	xRange <- c(x0, xl)
	dt <- .5
	dx <- .5
	h <- h
	tSteps <- diff(range(tRange))/dt
	xSteps <- diff(range(xRange))/dx
	plot(c(0, tf-1), c(x0, xl-1), type = 'n')
	for (i in 0:(tSteps-1)) {
		for(j in 0:(xSteps-1)){
			t <- i*dt
			x <- x0+j*dx
			m <- do.call(f,list(x=x, t=t))
			u <- t+(h*dt);
			v <- x+(h*dt)*m
			arrows(t,x,u,v,code=2,length=.01)
		}
	}
}

bctclt <- function(x, t) {
	return(x - sin(4 * t / (2 * pi)))
}

#dirField(bctclt, .5, 10, -4, 8)
#Fixed points along the sine curve, visible in the arrows. Since there are no
#constant horizontal lines, no stable fixed points exist.



#/--#########################################################################
# name: Craig Biwer
# assignment: Lab 4
# date: 10/30/2009
# question: 2
# subquestion: d
# other files:
##########################################################################/--

#Solution:
#N(t) = N(0)*exp(r*t) + pi/(4+(pi*r)^2) * (2*cos(2*t/pi) + pi*r*sin(2*t/pi))
#
#Plots:
#curve(.2*exp(x) + pi/(4+(pi)^2) * (2*cos(2*x/pi) + pi*sin(2*x/pi)), 0, 4)
#curve(.02*exp(x) + pi/(4+(pi)^2) * (2*cos(2*x/pi) + pi*sin(2*x/pi)), 0, 4)
#curve(2*exp(x) + pi/(4+(pi)^2) * (2*cos(2*x/pi) + pi*sin(2*x/pi)), 0, 4)
#
#Given a starting point, our solution curves follow the arrows in the
#direction field


#/--#########################################################################
# name: Craig Biwer
# assignment: Lab 4
# date: 10/30/2009
# question: 3
# subquestion: a
# other files:
##########################################################################/--

#Fixed point at V = -68.3356, and it's stable



#/--#########################################################################
# name: Craig Biwer
# assignment: Lab 4
# date: 10/30/2009
# question: 3
# subquestion: b
# other files:
##########################################################################/--

mbrnvlt <- function(x, t) {
	return((0-(x - 58.1) - 100 * (x + 69.6)) / .15)
}

#dirField(mbrnvlt, .0004, 20, -74, -64)

#The plot agrees with the fixed point analysis we performed.


#/--#########################################################################
# name: Craig Biwer
# assignment: Lab 4
# date: 10/30/2009
# question: 3
# subquestion: c
# other files:
##########################################################################/--

#Solution:
#V(t) = V(0) * exp(-673.333 * t) - 68.3356
#
#curve(.1 * exp(-673.333 * x) - 68.3356, 0, .02)
#curve(.02 * exp(-673.333 * x) - 68.3356, 0, .008)
#curve(-.02 * exp(-673.333 * x) - 68.3356, 0, .008)


#/--#########################################################################
# name: Craig Biwer
# assignment: Lab 4
# date: 10/30/2009
# question: 4
# subquestion: a
#NOTE: Worked on this with Namita Gupta
# other files:
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
# name: Craig Biwer
# assignment: Lab 4
# date: 10/30/2009
# question: 4
# subquestion: b
#NOTE: Worked on this with Namita Gupta
# other files:
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


if (signs[length(zeros)+1] > 0) {
	arrows(zeros[length(zeros)],dy , zeros[length(zeros)]+dx,dy)
} else {
	arrows(zeros[length(zeros)]+dx, -dy , zeros[length(zeros)], -dy)
}

