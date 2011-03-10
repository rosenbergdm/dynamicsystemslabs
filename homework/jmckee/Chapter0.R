# Jillian McKee
# Chapter 0

# 1a
fib <- function(k) {
x0 <- 0
x1 <- 1
while(x1<=k) {
newX1 <- x0 + x1
x0 <- x1
x1 <- newX1
}
return(x0);
}

# 1b
closer <- function (k, x, y) 
{
    s1 <- abs(k - x)
    s2 <- abs(k - y)
    if (s1 < s2) {
        return(x)
    }
    else {
        return(y)
    }
}

# 1c

closefib <- function(k) {
	x0 <- 0
	x1 <- 1
while(x1<=k) {
newX1 <- x0 + x1
x0 <- x1
x1 <- newX1
}
s1 <- abs(k-x0)
s2 <- abs(k-x1)
 if (s1 < s2) {
        return(x0)
    }
    else {
        return(x1)}
    }


# 2a

sortedVec <- c(x,y)
otherVal <- z
smallest <- min(sortedVec[1], otherVal)
middle <- min(max(sortedVec[1], otherVal), sortedVec[2])
largest <- max(max(sortedVec[1], otherVal), sortedVec[2])
c(smallest, middle, largest)

# 2b

unsortedVec <- c(x,y)
smallest <- min(unsortedVec[1], unsortedVec[2])
largest <- max(unsortedVec[1], unsortedVec[2])
c(smallest, largest)

# 2c

sort <- function(unsortedVec) {
for (i in 1:length(unsortedVec)){
	for (j in 1:(length(unsortedVec)-1)){
		x <- unsortedVec[j]
		y <- unsortedVec[j+1]
		if (x > y){
			unsortedVec[j+1] <- x
			unsortedVec[j] <- y
		}
	}	
}
return(unsortedVec)
}

# 3

xFinder <- function (k) {
	resultVec <- matrix(0,1,k)
	for (i in 1:k) {
		if (i ==1) {
			theta <- 0
			}
			else {
				theta <- (i-1)*2*pi/k
				}
		resultVec[i] <- exp(1i*theta)
		 }
	return(resultVec)
}

