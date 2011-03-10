#!/usr/bin/env r
# encoding: utf-8
# 
# Name: Si Tang
# Assignment: 3
# Date: Oct. 22, 2009
# Filename: HW3-2_SiTang.R
# Question: 2
#
# Task: Generate cobweb plots for three logistic models 
# 
# 
#############################################################################

myfunc2a <- function (x)
{
	y <- 3 * x - 3* x * x / 4 + 1;
	return (y)
}


myfunc2b <- function (x)
{
	y <- 100 * x - 2 * x *x
	return (y)
}


myfunc2c <- function (x)
{
	y <- x * x / 2 - 100 * x
	return (y)
}


par(mfrow=c(3,1))

max_iter=50;
plotcobweb <- function (myfunction, startpoint) 
{
	x_vector <- y_vector <- numeric(max_iter *2)
	y_vector[1] <-0
	x_vector[1] <-startpoint
	for(ii in 1:max_iter)
	{ 
		y_vector[2*ii] <- myfunction(x_vector[2*ii-1])
		y_vector[2*ii + 1] <- y_vector [2*ii]
		x_vector[2*ii] <- x_vector[2*ii-1]
		x_vector[2*ii+1] <- y_vector[2*ii+1]
	}
	
	lines(x_vector, y_vector, col='blue')
}

		

# plot the cobweb for function 2(a)
title1 <- expression ( 3*x - 3*x^2 / 4 + 1)
curve(myfunc2a, from=0, to= 4, ylab='y', ylim=c(0,4), main=title1)
lines(c(0,4), c(0,4), col='red')
plotcobweb(myfunc2a, 0.05)
Legend <- sprintf ("Iteration=%d\nStartPoint=%.3f", max_iter, 0.05)
legend(0,4,Legend,bty='n')

# plot the cobweb for function 2(b)
title2 <- expression ( 100*x-2*x^2)
curve(myfunc2b, from=0, to= 50, ylab='y', ylim=c(0,1250), main=title2)
lines(c(0,1250), c(0,1250), col='red')
plotcobweb(myfunc2b, 0.01)
Legend <- sprintf ("Iteration=%d\nStartPoint=%.3f", max_iter, 0.01)
legend(0,1250,Legend,bty='n')

# plot the cobweb for function 2(c)
title3 <- expression ( -100 *x + x^2/2 )
curve(myfunc2c, from=200, to= 250, ylab='y',  main=title3)
lines(c(0,250), c(0,250), col='red')
plotcobweb(myfunc2c, 205)
Legend <- sprintf ("Iteration=%d\nStartPoint=%.3f", max_iter, 205)
legend(200,5000,Legend,bty='n')


#!/usr/bin/env r
# encoding: utf-8
# 
# Name: Si Tang
# Assignment: 3
# Date: Oct. 22, 2009
# Filename: HW3-2-revised_SiTang.R
# Question: 2 - Revised Models
#
# Task: Generate cobweb plots for three logistic models 
# 
# 
#############################################################################


f1 <- function (x) 
{
	return (x * (5 - 4 * x))
}

f2 <- function (x) 
{
	return(2 * x * (1 - 3 * x / 2) )
}
f3 <- function (x)
{
	return (x / 2 - x*x / 5)
}

f4 <- function (x)
{
	return (x * (5/2 - 7 * x))

}

cobweb <- function ( myfunction, max_iter, startpoint)
{
        y_vector <- x_vector <- numeric (max_iter * 2)
        y_vector[1] <- 0
        x_vector[1] <- startpoint

        for (ii in 1: max_iter) {
                y_vector[2 * ii] <- myfunction( x_vector[2 * ii -1])
                y_vector[2 * ii + 1] <- y_vector[2 * ii]
                x_vector[2 * ii] <- x_vector[2 * ii -1]
                x_vector[2 * ii + 1] <- y_vector[2 * ii + 1]
        }

        x_max <- max(x_vector)
	a <- seq(0, x_max, length=200);
        y_max <- max(myfunction(a))

	Legend <- sprintf ("Iteration Times: %d\nStartPoint=%.3f", max_iter, startpoint)
        plot ( a, myfunction(a), xlab='x', ylab = 'y', type='l', ylim = c(0, y_max))
        lines (c(0,x_max),c(0, x_max), col = 'red')
        lines (x_vector, y_vector, col='blue')

        legend(0, y_max, Legend, bty='n');


}

par(mfrow=c(2,2))
cobweb(f1, 100, 0.01)
cobweb(f2, 100, 0.6)
cobweb(f3, 100, 2)
cobweb(f4, 100, 0.1)



#!/usr/bin/env r
# encoding: utf-8
# 
# Name: Si Tang
# Assignment: 3
# Date: Oct. 22, 2009
# Filename: HW3-3_SiTang.R
# Question: 3
#
# Task: write a functor which takes as an argument a function describing a
# logistic model and generates a cobweb plot
# 
# Functor Name: cobweb
# Arguments: myfunction ( The logistical function to generate cobweb plot), 
# max_iter ( The maximum iteration times), startpoint ( The initial condition
# for iteration )
# 
#############################################################################


cobweb <- function ( myfunction, max_iter, startpoint)
{
        y_vector <- x_vector <- numeric (max_iter * 2)
        y_vector[1] <- 0
        x_vector[1] <- startpoint

        for (ii in 1: max_iter) {
                y_vector[2 * ii] <- myfunction( x_vector[2 * ii -1])
                y_vector[2 * ii + 1] <- y_vector[2 * ii]
                x_vector[2 * ii] <- x_vector[2 * ii -1]
                x_vector[2 * ii + 1] <- y_vector[2 * ii + 1]
        }

        x_max <- max(x_vector)
        a <- seq(0, x_max, length=200);
        y_max <- max(myfunction(a))

        Legend <- sprintf ("Iteration Times: %d\nStartPoint=%.3f", max_iter, startpoint)
        plot ( a, myfunction(a), xlab='x', ylab = 'y', type='l', ylim = c(0, y_max))
        lines (c(0,x_max),c(0, x_max), col = 'red')
        lines (x_vector, y_vector, col='blue')

        legend(0, y_max, Legend, bty='n');


}


#!/usr/bin/env r
# encoding: utf-8
# 
# Name: Si Tang
# Assignment: 3
# Date: Oct. 22, 2009
# Filename: HW3-4_SiTang.R
# Question: 4(c)
#
# Task: Plot the glucose concentration as a graph over a reasonable time interval
# 
#######################################################################
G0 = 100;
k=0.01;

Glucose <- function (t)
{
	return (exp((-1)*k*t) * G0)
}

a <- c(0:180)
b <- Glucose(a)

plot (a, b, type = 'l', xlab='t/min', ylab='[Glucose]/(mg/dl)',col='purple', main='Glucose Concentration over three hours', xlim=c(0,200))

#!/usr/bin/env r
# encoding: utf-8
# 
# Name: Si Tang
# Assignment: 3
# Date: Oct. 22, 2009
# Filename: HW3-5_SiTang.R
# Question: 5(c)
#
# Task: Plot the glucose concentration as a graph over a reasonable time interval
# 
#######################################################################
G0 = 100
k=0.01
a= 4

Glucose <- function (t)
{
	return ( exp( (-1)*k * t) * (G0 - a/k )  + a / k )
}

b <- c(0:360)
c <- Glucose(b)

plot ( b,c, type = 'l', xlab='t/min', ylab='[Glucose]/(mg/dl)',col='purple', main='Glucose Concentration over six hours', xlim=c(0,360))

#!/usr/bin/env r
# encoding: utf-8
# 
# Name: Si Tang
# Assignment: 3
# Date: Oct. 22, 2009
# Filename: HW3-6a_SiTang.R
# Question: 6(a)
#
# Task: Binary Search
# 
#######################################################################


BinSearch <- function (X,a=1,b=length(X),k) 
{
	
if(a==b)
{
	if(X[a]==k) {return (a)}
	else {return (-1)}

}

mid <- as.integer( a + ( (b-a)/2 ) )

if (k < X[mid] )

{
	return ( BinSearch(X, a, mid, k) )
}

else if (k > X[mid] )

{
        return ( BinSearch(X, mid+1, b, k) )
}

else
{
	return (mid)
}

}
