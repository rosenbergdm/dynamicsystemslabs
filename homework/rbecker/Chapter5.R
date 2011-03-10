                    ## Russell Becker
                    ## Biomath lab 5

  #1(a)
x0 <- "user input"
r <- "user input"
maxt <- 500

logist <- function (x, r) {
   return (r * x * (1-x))
   }

xvector <- c(x0)

for (iter in 2:maxt) {
   xvector [iter] <- logist (xvector [iter - 1], r)
   }

# xvector holds the solution for 500 iterations



  #1(b & c)
# establish a functor to perform 1a
solvelogist <- function (r, x0) {
   solution <- c(x0)
   for (ii in 2:500) {
      solution [ii] <- logist (solution [ii-1], r)
      }
   return (solution)
   }
# establish the array of r values
rvals <- seq (from=0, to=4, by=0.01)

# solve for each r value & plot to bifurcation diagram
rrange <- c(0,4)
xrange <- c(0,1)
plot (rrange, xrange, type='n', xlab='r',
   ylab='x', main='bifurcation plot')

for (jj in 1:length(rvals)) {
   xvals <- solvelogist (rvals[jj], x0)
   for (kk in 201:500) {
      points (rvals[jj], xvals[kk], pch='.')
      }
   }

  #1(d)
trange <- c(0,300)
xrange <- c(0,1)
plot (trange, xrange, type='n',
  xlab='t', ylab='x', main='X(t)')

x0 <- 0.75
tvals <- 1:300

# r = 0.3 (single zero fixed point, red line)
red <- solvelogist (0.3, x0)
lines (tvals, red [201:500], col='red')
# r = 3.6 (complex oscillation, yellow line)
yellow <- solvelogist (3.6, x0)
lines (tvals, yellow [201:500], col='yellow')
# r = 3.1 (simple oscillation, green line)
green <- solvelogist (3.1, x0)
lines (tvals, green [201:500], col='green')
# r = 2.3 (single nonzero fixed point, blue line)
blue <- solvelogist (2.3, x0)
lines (tvals, blue [201:500], col='blue')



  #2(a)
# The variables x and y represent two distinct
# subsets of a population x + y, which have
# different dynamics and effects on one another.
# The numbers in the matrix represent the effects
# of each population subset on itself and on the
# other subset.  Specifically, the numbers in the
# trace represent survival rates of each subset,
# and the other numbers in the matrix represent
# production of one subset by the other subset.
# e.g., in the next generation, all of the previous
# x has survived, and for every previous y, 2 new x
# are produced.  This could be by production of new
# individuals, or transition of individuals between
# the two subsets.


  #2(b)
x0 <- "user input"
y0 <- "user input"
maxiter <- "user input"

solvex <- function (x1, y1) {
   return (x1 + (2 * y1))
   }
solvey <- function (x1, y1) {
   return (0.3 * x1)
   }

propagate <- function (x0, y0, maxiter) {
   initial <- c(x0, y0)
   xymatrix <- matrix (initial,2,1)
   for (ii in 2:maxiter) {
      x <- solvex (xymatrix [1,ii-1], xymatrix [2,ii-1])
      y <- solvey (xymatrix [1,ii-1], xymatrix [2,ii-1])
      xyvector <- c(x, y)
      xymatrixi <- matrix (xyvector,2,1)
      xymatrix <- cbind (xymatrix, xymatrixi)
      }
   return (xymatrix)
   }

propagate (x0, y0, maxiter)


  #2(c)
# For all non-negative initial conditions
# other than (x=0, y=0), both x and y grow
# unbounded toward positive infinity.
# Specifically, with each iteration, the
# number of individuals in each subset
# increases by a factor of positive 1.42


  #2(d)
# Because the system seems to have a single
# unstable fixed point at (0,0), both
# eigenvalues should be real and positive.
# Based on the information described in 2(c),
# I would estimate that the largest eigenvalue
# is positive 1.42.



  #3(a)
# the parameter a represents the survival rate
# of the y subset, expressed as a ratio, and
# can range from 0 to 1.


  #3(b)  (with the new matrix as shown below)
# (0.5  2)
# (0.1  a)
x0 <- 100
y0 <- 100
avals <- seq (0,1,0.01)
alleigens <- matrix (avals,3,101,byrow=TRUE)
equil <- matrix (avals,3,101,byrow=TRUE)

solvex <- function (x1, y1, a) {
   return ((0.5 * x1) + (2 * y1))
   }
solvey <- function (x1, y1, a) {
   return ((0.1 * x1) + (a * y1))
   }

for (hh in 1:101) {
   initial <- c(x0, y0)
   xymatrix <- matrix (initial,2,1)
   for (ii in 2:5000) {
      x <- solvex (xymatrix [1,ii-1], xymatrix [2,ii-1], avals [hh])
      y <- solvey (xymatrix [1,ii-1], xymatrix [2,ii-1], avals [hh])
      xyvector <- c(x, y)
      xymatrixi <- matrix (xyvector,2,1)
      xymatrix <- cbind (xymatrix, xymatrixi)
      }
   equil [2,hh] <- xymatrix [1,5000]
   equil [3,hh] <- xymatrix [2,5000]
   alleigens [2,hh] <- (xymatrix[1,5000] / xymatrix[1,4999])
   alleigens [3,hh] <- (xymatrix[2,5000] / xymatrix[2,4999])
   }

# The matrix 'alleigens' now contains, in each column
# descending, the a value, the eigenvalue of x, and
# the eigenvalue of y.  Meanwhile, the matrix 'equil'
# contains in each column descending the a value, the
# x value at t=5000, and the y value at t=5000.

alleigens

# the eigenvalue increases as a increases, from an
# eigenvalue of 0.7623475 at a=0 to an eigenvalue
# of 1.262348 at a=1.


  #3(c)

equil

# the equilibrium behavior changes radically at a=0.6.
# for a < 0.6, both x and y converge toward zero, whereas
# for a > 0.6, both x and y grow rapidly toward positive
# infinity.  At a=0.6, the largest eigenvalue appears to
# be 1.  This makes sense conceptually, because we know
# that in discrete time linear models, eigenvalues greater
# than one cause unbounded growth, whereas eigenvalues
# less than one cause decay toward zero.

x0 <- 100
y0 <- 100
avals <- seq (0,1,0.01)
alleigens <- matrix (avals,3,101,byrow=TRUE)
equil <- matrix (avals,3,101,byrow=TRUE)

solvex <- function (x1, y1, a) {
   return ((0.5 * x1) + (2 * y1))
   }
solvey <- function (x1, y1, a) {
   return ((0.1 * x1) + (a * y1))
   }

initial <- c(x0, y0)
xymatrix <- matrix (initial,2,1)

for (ii in 2:500) {
   x <- solvex (xymatrix [1,ii-1], xymatrix [2,ii-1], 0.65)
   y <- solvey (xymatrix [1,ii-1], xymatrix [2,ii-1], 0.65)
   xyvector <- c(x, y)
   xymatrixi <- matrix (xyvector,2,1)
   xymatrix <- cbind (xymatrix, xymatrixi)
   }

time <- 1:500
xvals <- xymatrix[1,]
yvals <- xymatrix[2,]
plot (time, xvals, col='red', type='l',
   main='a=0.65', ylab='population')
lines (time, yvals, col='blue')

# x values plot in red, y values in blue.
