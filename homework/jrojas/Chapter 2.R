##################################
#Chapter 2 Lab
#
#Jose Rojas
##################################

################# Exercise 1 ####################

 # (a)
 # At equilibrium N_t+1 = N_t, and:
 #
 # 41*N - 10*N^2 = N
 # 40*N - 10*N^2 = 0
 # N (40 - 10*N) = 0
 # 
 # The fixed points are at:
 # N = 0	N = 4
 # 
 # f'(0) =  41 this point is unstable
 # f'(4) = -39 this point is unstable
 # 
 # (b)
 # 41*N + 2*N^2 = N
 # 40*N + 2*N^2 = 0
 # N (40 + 2*N) = 0
 # 
 # The fixed points are at:
 # N = 0	N = -20
 # 
 # f'(0)   =  41 this point is unstable
 # f'(-20) = -39 this point is unstable

################# Exercises 2 and 3 ####################

 # ARGUMENTS Map, starting conditions x0, plot window,
 #           number of iterations, and ... any arguments to be
 #		 passed to the plot function
 # RETURTNS  The cobweb plot of the map, and the points used to
 #		 make it
cobWeb <- function(Map,x0,iterations,xAxis,yAxis, ...){
	curve(Map, xAxis[1], xAxis[2], ylim=yAxis, ...)
	abline(0,1,col='red',lwd=2)

	MapX <- numeric(2*iterations)
	MapY <- numeric(2*iterations)
	MapX[1]	<- x0
	MapY[1]	<- 0

	for (i in 1:iterations){
		MapY[2*i]	<- Map( MapX[2*i-1] )
		MapY[2*i+1]	<- MapY[2*i]
		MapX[2*i]	<- MapX[2*i-1]
		MapX[2*i+1]	<- MapY[2*i+1]
	}
	lines(MapX,MapY,col='blue',lwd=1.5)
	list(xs=MapX,ys=MapY)
}

 # Simple function to make a particular logistic growth function
 # ARGUMENTS desired model parmeters
 # RETURTNS  a function describing a log model with those parameters
logModelMap <- function (a, b, c) {
	Map <- function(x)
		a + b*x + c*x^2
	return( Map )
}

 # f(x) = 3x - (3/4)x^2 + 1
map <- logModelMap(1,3,-(3/4))
 # There's a fixed point at about 3.1. It's unstable:
cobWeb(map,3.1,200,xAxis=c(-1,5),yAxis=c(0,5))

 # f(x) = 100x - 2x^2
map <- logModelMap(0,100,-2)
 # There is a stable point at 49.5. Tt's clearly unstable:
cobWeb(map,.5,200,xAxis=c(0,100),yAxis=c(-10,1300))
cobWeb(map,.49,200,xAxis=c(0,100),yAxis=c(-10,1300))
cobWeb(map,.51,200,xAxis=c(0,100),yAxis=c(-10,1300))

 # f(x) = -100x + (1/2)x^2
map <- logModelMap(0,-100,.5)
 # There is a fixed point at 202. It's unstable:
cobWeb(map,202,200,xAxis=c(-10,300),yAxis=c(0,400))
cobWeb(map,201,200,xAxis=c(-10,300),yAxis=c(0,400))
cobWeb(map,203,200,xAxis=c(-10,300),yAxis=c(0,400))


################# Exercises 4 and 5 ####################

 # There's a some math on paper associated with these.
 # I'll give it to you next on Monday, or whenever.

glucose <- function (t, rate, intial)
	intial * exp(-1 * rate * t)

plot(glucose(0:600,.01,100),
	main="Glucose Concentration",
	sub="Initial concentration 100mg/dl and Removal rate of 0.01/min",
	xlab="Time (minutes)",
	ylab="Concentration (mg/dl)")


glucose2 <- function (t, removal, addition, intial)
	(addition / removal) +
		(addition / removal) * exp(-1 * removal * t) +
		intial * exp(-1 * removal * t)

plot(glucose2(0:100,.1,4,100),
	main="Glucose Concentration",
	sub="Initial concentration 100mg/dl and Removal rate of 0.01/min",
	xlab="Time (minutes)",
	ylab="Concentration (mg/dl)",
	ylim=c(0,200))

 
