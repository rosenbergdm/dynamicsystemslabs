## Brendan Colson
## 10/8/02
## Lab Exercise
## Chapter 0

## Ex. 1(a)

fib <- function(k) {
x0 <- 0;
x1 <- 1;
while(x1 <= k) {      
  newX1 <- x0 + x1;      
  x0 <- x1;              
  x1 <- newX1;           
}
return(x0);
}

###################################
# 1(b)

k <- 100; x <- 99; y<-50

dist_k_y <- abs(k-y)
dist_k_x <- abs(k-x)

if ( dist_k_y < dist_k_x) {
	dist_k_y
} else {
	dist_k_x
}

#################

#1(c)

fib <- function(k) {
x0 <- 0;
x1 <- 1;
while(x1 <= k) {      
  newX1 <- x0 + x1;      
  x0 <- x1;              
  x1 <- newX1;           
}
dist_k_x0 <- abs(k-x0);
dist_k_x1 <- abs(k-x1);	
if (dist_k_x0 < dist_k_x1) {
    return(x0);
} else {
        return(x1);
}
}

####################
# 2(a)

y <- 10
z <-20
numVector <- c(y, z)
x <- 25
if (x < y) {
c(x, numVector)
} else if (y < x & x < z) {
c(y, x, z)
} else { c(numVector, x)
}

######################
# 2(b)

x <- 25
y <- 15
unsortVec <- c(x, y)
if (x < y) {
c(x, y)
} else {
c(y, x)
}

######################
# 2(c) 

unsortVec <- c(4, 6, 9, 2, 5, 8, 1, 3, 10, 7)
for (ii in 1:10) {
  for (jj in ii:10) { 
    temp = unsortVec[ii]
    if (temp > unsortVec[jj]) {
      unsortVec[ii] = unsortVec[jj];
      unsortVec[jj] = temp}}}
      
######################

