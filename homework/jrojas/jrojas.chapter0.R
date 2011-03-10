# Jose Rojas
# Chapter 0 Homework
# jrojas.chapter0.R

#1. (a)

fib <-function (k){
	x0 <- 0
	x1 <- 1
	while(x1 < k) {
		newX1 <- x0 + x1
		x0 <- x1
		x1 <-newX1
	}
if (k == x1) x1 else x0
}

#1. (b)

close <-function (k,x,y){
	kx <- abs(k - x)
	ky <- abs(k - y)
	if (kx > ky) y else x
	}

#1. (c)

closefib <-function (k){
	x0 <- 0
	x1 <- 1
	while(x1 < k) {
		newX1 <- x0 + x1
		x0 <- x1
		x1 <-newX1
	}

	close(k,x0,x1)
}

#2. (a)

two_one <- function (two, one) {
	if (all(two < one)) {
		c(two,one)
	} else if (all(two > one)) {
		c(one,two)
	} else c(two[1],one,two[2])
}

#2. (b)

two <- function (two)
	if (two[1]<two[2]) two else two[c(2,1)]


#2. (c)

Sort <- function (v) {
	v[1:2] <- two(v[1:2])
	for (k in 1:length(v)){
		for (i in 1:(length(v)-2)) {
			v[i:(i+2)] <- two_one(v[i:(i+1)],v[i+2])
		}
	}
	v
}

#3.

eq3 <- function (K) {
	unityPrimitive <- exp(1) ^ ( ( 2 * pi * (0+1i) ) / K )

	unity <- unityPrimitive ^ ( 0 : (K-1) )

	Primitive <- exp(1) ^ ( ( (0+1i) * pi ) / K )

	Primitive * unity
}
