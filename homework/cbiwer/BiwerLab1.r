# Craig Biwar
# Chapter 0

x0 <- 0;
x1 <- 1;
findFib <- function(k) {
	while(x1 <= k) {
		newX1 <- x0 + x1;
		x0 <- x1;
		x1 <- newX1;
	}
	x0
}

closestTo <- function(x, y, k) {
	if (abs(k-x) < abs(k-y)) {
		x
	}
	else {
		y
	}
}

x0 <- 0;
x1 <- 1;
nearestFib <- function(k) {
	while(x1 < k) {
		newX1 <- x0 + x1;
		x0 <- x1;
		x1 <- newX1;
	}
	closestTo(x0, x1, k)
}

addToSorted <- function(addThis, toThis) {
	if (addThis < toThis[1]) {
		showThis <- c(addThis, toThis)
	}
	else if (addThis < toThis[2]) {
		showThis <- c(toThis[1], addThis, toThis[2])
	}
	else {
		showThis <- c(toThis, addThis)
	}
	showThis
}

sortSmallVector <- function(sortMe) {
	if (sortMe[1] <= sortMe[2]) {
		showMe <- sortMe
	}
	else {
		showMe <- c(sortMe[2], sortMe[1])
	}
	showMe
}

sortBigVector <- function(sortMe) {
	flag <- 0
	oldVec <- sortMe
	while (flag == 0 | any(oldVec != sortMe)) {
		flag <- 1
		oldVec <- sortMe
		for(j in 1:9) {
			if (sortMe[j] > sortMe[j+1]) {
				holdOn <- sortMe[j];
				sortMe[j] <- sortMe[j+1];
				sortMe[j+1] <- holdOn
			}
		}
	}	
	sortMe
}

solveEquation <- function(exponent) {
	solutions <- c()
	for (j in 1:exponent) {
		solutions = c(solutions, exp((2*pi*j/exponent)*(0+1i)))
	}
	solutions
}