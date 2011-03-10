#!/usr/bin/env r
# encoding: utf-8
# 
# name: Crnich, Joseph
# assignment: 5
# date: 11/20/2009
# filename: Joey_Crnich_Lab_Ch_5_Answers.R
#############################################################################


#/--#########################################################################
# name: Crnich, Joseph
# assignment: 5
# date: 11/20/2009
# question: 1
# subquestion: a
# other files:
##########################################################################/--

## The following code is copied from the Chapter 1 assignment, with a few
## modifications. Notably, the addition of r as an argument of the logistic
## model function. This allows r to be varied for creation of the bifurcation
## diagram.

init_value <- 0.7;
r <- 2.8;

logModFun <- function(x_at_t, r) {
      x_at_t1 <- r * x_at_t * (1 - x_at_t);
      return(x_at_t1);
}
max_iter <- 500;
x_array <- numeric(length=max_iter+1);
x_array[1] <- init_value;
for (ii in 1:max_iter) {
      x_array[ii+1] <- logModFun(x_array[ii], r);
}

## Here we plot the logistic. This example uses x0 = 0.7, and r=2.8:

plotTtl <- paste("Logistic Growth Model: x0=", init_value, ", r=", r)
plot(0:max_iter, x_array, type='l', main=plotTtl, xlab='t', ylab='x(t)', xaxs="i")
abline(0, 0)

## This is the code I used to find fixed points. It compares sequential
## values of x until they are the same, then displays that last value.
## In order to shorten the code, I substituted the isTRUE(all.equal)
## method David showed in lab.

nt <- init_value
nt_plus_1 <- logModFun(nt, r)
nt_plus_2 <- logModFun(nt_plus_1, r)
while (!isTRUE(all.equal(nt_plus_1, nt)) &
  !isTRUE(all.equal(nt_plus_1, nt_plus_2))) {
      nt_plus_1 <- logModFun(nt, r)
      nt_plus_2 <- logModFun(nt_plus_1, r)
      nt <- nt_plus_2
}
cat('The fixed point is', nt, '.')

## Ultimately, however, this code is not very good. Not only can it
## sometimes incorrectly give a value other than the equilibrium at which
## the model hovers momentarily; this code also is incapable of giving
## results for instances in which there are more than one "equilibria" (i.e.
## when the model cycles, or reaches chaos).


#/--#########################################################################
# name: Crnich, Joseph
# assignment: 5
# date: 11/20/2009
# question: 1
# subquestion: b
# other files:
##########################################################################/--

## Because the code from my Chapter 1 homework wasn't particularly useful,
## I wrote new code for the fixed points evaluation. What follows is a
## function that evaluates all x values from 1-500 for a particular r, and
## then returns a vector containing the fixed points. I created it this way
## so I could run it through a for loop with our r values (below).

wrapped <- function(init_value, r) {

    ## This first part evaluates x from 1:500 for the specific r:
	
    max_iter <- 500;
    x_array <- numeric(length=max_iter+1);
    x_array[1] <- init_value;
    for (ii in 1:max_iter) {
          x_array[ii+1] <- logModFun(x_array[ii], r);
    }

    ## While this part first evaluates whether or not the last and
    ## penultimate values are equivalent. If so, that is the 'fixed point'
    ## vector returned. Otherwise, it returns a vector of all the unique
    ## values near the tail end of the graph (in this way, we can get
    ## multiple 'fixed point' values).

    equil <- numeric(length=1)
    if(isTRUE(all.equal(x_array[ii], x_array[ii+1]))) {
        equil[1] <- x_array[ii]
    } else {
        equil <- unique(x_array[491:501])
    }
    return(equil)
}

## Finally, we define a vector of r values, and an empty matrix in which we
## will store all our fixed points. Then, we run a for loop that defines each
## 'fixed point' for all r and stores them systematically in the matrix.

rVec <- c((0:400)/100)
xMtrx <- matrix(ncol=401, nrow=15)
for(ii in 1:401) {
	vec <- wrapped(init_value, rVec[ii])
    xMtrx[1:length(vec),ii] <- vec
}



#/--#########################################################################
# name: Crnich, Joseph
# assignment: 5
# date: 11/20/2009
# question: 1
# subquestion: c
# other files: 
##########################################################################/--

## To plot the points, we initialize a plot with the first row of the matrix:

plot(rVec[201:401], xMtrx[1,201:401], type="p",
    main="Bifurcation Diagram: 2<r<4", xlab="r", ylab="x", xaxs="i")

## Then plot the points from every other row. Using two for loops allows us
## to plot every 'fixed point' (rows) from every r (columns).

for(ii in 2:10) {
    for(jj in 201:401) {
        points(rVec[jj], xMtrx[ii,jj])
    }
}


#/--#########################################################################
# name: Crnich, Joseph
# assignment: 5
# date: 11/20/2009
# question: 1
# subquestion: d
# other files: 
##########################################################################/--

## When r=2.8, our solution over time approaches the fixed point:

r <- 2.8
max_iter <- 500;
x_array <- numeric(length=max_iter+1);
x_array[1] <- init_value;
for (ii in 1:max_iter) {
    x_array[ii+1] <- logModFun(x_array[ii], r);
}

plotTtl <- paste("Logistic Growth Model: x0=", init_value, ", r=", r)
plot(0:40, x_array[1:41], type='l', main=plotTtl, xlab='t', ylab='x(t)',
        xaxs="i")
    

## When r=3.2, our solution over time cycles between two points:

r <- 3.2
max_iter <- 500;
x_array <- numeric(length=max_iter+1);
x_array[1] <- init_value;
for (ii in 1:max_iter) {
    x_array[ii+1] <- logModFun(x_array[ii], r);
}

plotTtl <- paste("Logistic Growth Model: x0=", init_value, ", r=", r)
plot(0:100, x_array[1:101], type='l', main=plotTtl, xlab='t', ylab='x(t)',
        xaxs="i")


## At r=3.5, our solution over time cycles between 4 points:

r <- 3.5
max_iter <- 500;
x_array <- numeric(length=max_iter+1);
x_array[1] <- init_value;
for (ii in 1:max_iter) {
    x_array[ii+1] <- logModFun(x_array[ii], r);
}

plotTtl <- paste("Logistic Growth Model: x0=", init_value, ", r=", r)
plot(0:100, x_array[1:101], type='l', main=plotTtl, xlab='t', ylab='x(t)',
        xaxs="i")


## And at r=3.9, our solution over time has reached deterministic chaos:

r <- 3.9
max_iter <- 500;
x_array <- numeric(length=max_iter+1);
x_array[1] <- init_value;
for (ii in 1:max_iter) {
    x_array[ii+1] <- logModFun(x_array[ii], r);
}

plotTtl <- paste("Logistic Growth Model: x0=", init_value, ", r=", r)
plot(0:100, x_array[1:101], type='l', main=plotTtl, xlab='t', ylab='x(t)',
        xaxs="i")


#/--#########################################################################
# name: Crnich, Joseph
# assignment: 5
# date: 11/20/2009
# question: 2
# subquestion: a
# other files: 
##########################################################################/--

## The variables x and y respresent the count of individuals in each stage.
## If the stages were defined by age, y would be older. The numbers in the
## matrix defines the change that will occur before the next time step. In 
## the top row are 'fecundity' terms, that describe the percentage of new
## individuals in stage x at the next time step, while the bottom row
## describes the percentage of indivuals that will be in stage y at the next
## time step. In this particular transition matrix, stage x will gain 2
## indivuals per indiv. at stage y, and half an indiv. per indiv. at stage x.
## Stage y will only get a tenth of an indiv. per each indiv. in stage x, and
## none from stage y.

#/--#########################################################################
# name: Crnich, Joseph
# assignment: 5
# date: 11/20/2009
# question: 2
# subquestion: b
# other files: 
##########################################################################/--

## First we define the transition matrix, our initial x and y:

transMtrx <- matrix(c(0.5, 2, 0.1, 0), nrow=2, byrow=TRUE)

x0 <- 10
y0 <- 10

## Then we create an empty matrix in which to put our resulting values:

nIter <- 30
finalMtrx <- matrix(numeric(length=(nIter + 1)*2), nrow=2)
finalMtrx[,1] <- matrix(c(x0, y0), nrow=2)

## This for loop calculates the next set of values, then stores them in the
## empty matrix from above. It then redefines the original value as the new
## one, and repeats.

for(ii in 1:nIter) {
	finalMtrx[,ii+1] <- transMtrx %*% finalMtrx[,ii]
}

plot(c(0:nIter), finalMtrx[1,], type="o", main="Leslie Model: x0=10, y0=10",
    xlab="Time Step", ylab="", xaxs="i")
lines(c(0:nIter), finalMtrx[2,], type="o", col=2)
abline(0,0)
legTxt <- c("x", "y")
legend(20, 10, legTxt, pch=1, col=c(1,2))


#/--#########################################################################
# name: Crnich, Joseph
# assignment: 5
# date: 11/20/2009
# question: 2
# subquestion: c
# other files: 
##########################################################################/--

## I defined a function to quickly run through a variety of initial
## conditions.

leslieFun <- function(x0, y0, transMtrx, nIter) {
    finalMtrx <- matrix(numeric(length=(nIter + 1)*2), nrow=2)
    finalMtrx[,1] <- matrix(c(x0, y0), nrow=2)
    for(ii in 1:nIter) {
	    finalMtrx[,ii+1] <- transMtrx %*% finalMtrx[,ii]
    }
    mainTtl <- paste("Leslie Model: x0=", x0, " y0=", y0)
    plot(c(0:nIter), finalMtrx[2,]+finalMtrx[1,], type="o",
        main=mainTtl, xlab="Time Step", ylab="",
        xaxs="i")
    lines(c(0:nIter), finalMtrx[2,], type="o", col=2)
    lines(c(0:nIter), finalMtrx[1,], type="o", col=3)
    abline(0,0)
}    

## Here we run through a variety of intial conditions...:

x0 <- 10
y0 <- 10
nIter <- 30
leslieFun(x0, y0, transMtrx, nIter)

x0 <- 200
y0 <- 150
leslieFun(x0, y0, transMtrx, nIter)

x0 <- 0.75
y0 <- 0.1
leslieFun(x0, y0, transMtrx, nIter)

## ... all of which shrink to zero for both x and y values.

#/--#########################################################################
# name: Crnich, Joseph
# assignment: 5
# date: 11/20/2009
# question: 2
# subquestion: d
# other files: 
##########################################################################/--

## Because the slope of the line that any intial value will converge to is
## equivalent to the dominant eigenvalue, we can take the slope of two points 
## way out near the maximum interation as a relatively accurate estimate.

sum(finalMtrx[,nIter]) / sum(finalMtrx[,nIter-1])

#/--#########################################################################
# name: Crnich, Joseph
# assignment: 5
# date: 11/20/2009
# question: 3
# subquestion: a
# other files: 
##########################################################################/--

## In biological terms, the parameter a would be the additional percentage of
## individuals in stage y that will remain in stage y in the next time step.
## Mathematically, a represents the uncoupled addition y will add to itself
## by the next time step.

#/--#########################################################################
# name: Crnich, Joseph
# assignment: 5
# date: 11/20/2009
# question: 3
# subquestion: b
# other files: 
##########################################################################/--

## Redefining the Leslie function from before to better graph a repetition of
## models with variations in the transition matrix (i.e. parameter a).

leslieA <- function(x0, y0, transMtrx, nIter, color, shape) {
    finalMtrx <- matrix(numeric(length=(nIter + 1)*2), nrow=2)
    finalMtrx[,1] <- matrix(c(x0, y0), nrow=2)
    for(ii in 1:nIter) {
	    finalMtrx[,ii+1] <- transMtrx %*% finalMtrx[,ii]
    }
    mainTtl <- paste("Leslie Model: x0=", x0, " y0=", y0)
    lines(c(0:nIter), finalMtrx[2,]+finalMtrx[1,], type="l", col=color)
    points(c(0:nIter), finalMtrx[2,]+finalMtrx[1,], col=color,
        pch=shape)
}    

## Defining the iterations and initializing the plot:

nIter <- 30

plot(-1, -1, xlim=c(0, nIter), ylim=c(0, 5), xaxs="i", main="Leslie Model: 
Variations in parameter a from 0.1 - 1", xlab="Time Step", ylab="")

## This for loop runs through ten values of a, from 0.1 to 1 by 0.1, graphing the population's outcome:

for(ii in 1:10) {
    a <- ii/10
    transMtrx <- matrix(c(0.5, 2, 0.1, a), nrow=2, byrow=TRUE)
    leslieA(1, 1, transMtrx, nIter, rainbow(10)[ii], ii)
}
legTxt <- paste("a=", 1:10/10)
legend(25, 3.25, legTxt, pch=c(1:10), col=rainbow(10))

## From this graph, we can see that transition matrices where a > 0.6 tend
## towards infinity, while matrices where a < 0.6 tend towards zero. At
## a = 0.6 (the bifurcation point), we reach an equilibrium of ~3 1/3, as
## calculated below:

a <- 0.6
transMtrx <- matrix(c(0.5, 2, 0.1, a), nrow=2, byrow=TRUE)

nIter <- 30

x0 <- 1
y0 <- 1
finalMtrx <- matrix(numeric(length=(nIter + 1)*2), nrow=2)
finalMtrx[,1] <- matrix(c(x0, y0), nrow=2)

for(ii in 1:nIter) {
	finalMtrx[,ii+1] <- transMtrx %*% finalMtrx[,ii]
}

sum(finalMtrx[,nIter+1])

## As for eiegnvalues, the function below will put out eigenvalue estimates
## for any Leslie Model:

leslieEig <- function(x0, y0, transMtrx, nIter) {
    finalMtrx <- matrix(numeric(length=(nIter + 1)*2), nrow=2)
    finalMtrx[,1] <- matrix(c(x0, y0), nrow=2)
    for(ii in 1:nIter) {
	    finalMtrx[,ii+1] <- transMtrx %*% finalMtrx[,ii]
    }
    sum(finalMtrx[,nIter]) / sum(finalMtrx[,nIter-1])
}

## Running it through the same for loop parameters as our graph (from 0.1-1),
## with a receptacle vector for the resulting eigenvalues:

eigVec <- numeric(length=10)

for(ii in 1:10) {
    a <- ii/10
    transMtrx <- matrix(c(0.5, 2, 0.1, a), nrow=2, byrow=TRUE)
    eigVec[ii] <- leslieEig(1, 1, transMtrx, nIter)
}

## The vector below shows that when a < 0.6, the lambda is less than one;
## this is in agreement with our graph, as these a values cause the plots
## to tend to zero:

eigVec[1:5]

## At a = 0.6, the lambda is exactly 1, meaning the population will
## approach its equilibrium. This value of a is the bifurcation point, as
## a values on either side of it exhibit different behaviors:

eigVec[6]

## And when a > 0.6, the lambdas are greater than 1, meaning the plots will
## tend to infinity:

eigVec[7:10]


#/--#########################################################################
# name: Crnich, Joseph
# assignment: 5
# date: 11/20/2009
# question: 3
# subquestion: c
# other files: 
##########################################################################/--

## See 3.b above ^


