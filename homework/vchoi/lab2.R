# Vivian Choi
# Lab 2

#plots f(x) = x * (5 - 4 * x) given initial condition "init" for "k" iterations
#Sorry, the plots look a little weird because I couldn't figure out how to use
#the plot function to scale things properly.
twoFunctor <- function(init, k, fun){
	x = numeric(k*2)
	x[1] = init
	y = numeric(k*2)
	y[1] = 0
	ii = 2
	while(ii <= 2*k) {
		y[ii] = fun(x[ii-1])
		y[ii+1] = y[ii]
		x[ii] = x[ii-1]
		x[ii+1] = y[ii+1]
		ii = ii+2
	}
	plot((init-k):(init+k), fun((init-k):(init+k)), type = 'l', 
		xlim = c(-10, 10), ylim = c(-10, 10))
	lines(x, y, col = 'red')
	lines((init-k):(init+k), (init-k):(init+k), col='blue')
}

#The fixed points do not appear to be stable.
fun1 <- function(x) {
	return (x * (5 - 4 * x))
}

#Both fixed points appear to be stable.
fun2 <- function(x) {
	return (2 * x * (1 - 3 * x / 2))
}

#The fixed point at x=0 is stable, but the other at x=2.5 is not.
fun3 <- function(x) {
	return(x / 2 - x^2 / 5)
}

#The fixed point at x=0 is stable, but the other is not.
fun4 <- function(x) {
	return(x * (5/2 - 7 * x))
}
