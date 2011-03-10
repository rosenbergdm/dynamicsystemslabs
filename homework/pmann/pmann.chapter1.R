# Patrick Mann
# Chapter 1 Exercises
# pmann.chapter1.R


# 1.

dervPoly <- function(Poly1) {
	polyPts <- parsePolynomial(Poly1);
	polyPts$exponents <- polyPts$exponents[
	polyPts$exponents >= 0];
	polyPts$coefficients <- polyPts$coefficients[
	1:length(polyPts$exponents)]
	polyPtsDerv <- parsePolynomial(Poly1);
	polyPtsDerv$exponents <- polyPts$exponents-1;
	polyPtsDerv$exponents <- polyPtsDerv$exponents[
	polyPtsDerv$exponents >= 0];
	polyPtsDerv$coefficients <- polyPts$coefficients*
	polyPts$exponents;
	polyPtsDerv$coefficients <- c(polyPtsDerv$coefficients[
	1:length(polyPtsDerv$exponents)])
	derivative1 <- deparsePolynomial(polyPtsDerv[[2]],
	polyPtsDerv[[1]])
	return(derivative1)
	}

# 2a.

Ques2 <- function(x) {
	return(2*x-0.5*x^2)}
    x_array <- Ques2(0)
    t0 <- 0
	t1 <- 1
	while (length(x_array) < 10) {
		t0 <- t0 + t1;
		x_array <- c(x_array, Ques2(t0))
		}

plot(0:(length(x_array)-1), x_array, main='f(x) = x ( 2 - (x/2) )',
	xlab='x', ylab='f(x)')
p1 <- c(0,2)
q1 <- c(0,2)
points(p1,q1,pch=19,col="red",cex=1)

#red points are the fixed points for function f(x) (aka Ques2())

# 2b.

diffEqImap <- function(x) {
	return (2*x-0.5*x^2)
	}
init_value <- 1;
max_iter <- 50;
p1 <- diffEqImap

mrFunctor <- function(p1)
{plot(init_value:max_iter, 
	do.call( p1, 
	list(x=init_value:max_iter) ),
	type='l', 
	main='Difference Equation Iterated Plot', 		xlab='t',
	ylab='x(t)');
	t0 <- -0.01
	t1 <- 0.01
	t2 <- 1
	while (t2 > 0) {
		t0 <- t0 + t1;
		t2 <- p1(t0)-t0
		}
	x_Fixed <- c(t0)
	t0 <- t1
	t1 <- 0.01
	t2 <- 1
	while (t2 > 0) {
		t0 <- t0 + t1;
		t2 <- p1(t0)-t0
		}
	x_Fixed <- c(x_Fixed, t0)
	return (x_Fixed) }

mrFunctor(p1)

# 3a.

diffEqImap <- function(x) {
	return (2*x-2*x^2)
	}
init_value <- 1;
max_iter <- 50;
p1 <- diffEqImap

mrFunctor <- function(p1)
{plot(init_value:max_iter, 
	do.call( p1, 
	list(x=init_value:max_iter) ),
	type='l', 
	main='Difference Equation Iterated Plot', 		xlab='t',
	ylab='x(t)');
	t0 <- -0.01
	t1 <- 0.01
	t2 <- 1
	while (t2 > 0) {
		t0 <- t0 + t1;
		t2 <- p1(t0)-t0
		}
	x_Fixed <- c(t0)
	t0 <- t1
	t1 <- 0.01
	t2 <- 1
	while (t2 > 0) {
		t0 <- t0 + t1;
		t2 <- p1(t0)-t0
		}
	x_Fixed <- c(x_Fixed, t0)
	return (x_Fixed) }

mrFunctor(p1)

#Yes, the solution found computationally (x_Fixed) matches analytically calculated fixed points ((0,0) and (0.5,0.5).


# 3b.

diffEqImap <- function(x) {
	return (4*x-4*x^2)
	}
init_value <- 1;
max_iter <- 50;
p1 <- diffEqImap

mrFunctor <- function(p1)
{plot(init_value:max_iter, 
	do.call( p1, 
	list(x=init_value:max_iter) ),
	type='l', 
	main='Difference Equation Iterated Plot', 		xlab='t',
	ylab='x(t)');
	t0 <- -0.01
	t1 <- 0.01
	t2 <- 1
	while (t2 > 0) {
		t0 <- t0 + t1;
		t2 <- p1(t0)-t0
		}
	x_Fixed <- c(t0)
	t0 <- t1
	t1 <- 0.01
	t2 <- 1
	while (t2 > 0) {
		t0 <- t0 + t1;
		t2 <- p1(t0)-t0
		}
	x_Fixed <- c(x_Fixed, t0)
	return (x_Fixed) }

mrFunctor(p1)

#Yes, the solution found computationally (x_Fixed) matches analytically calculated fixed points ((0,0) and (0.75,0.75).


