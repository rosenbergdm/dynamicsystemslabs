        ##  Biomath R Assignment 3
        ##  Russell Becker

   #1(a)
See separate sheet


   #1(b)
nvector <- -20:300
fnvector <- (10 * nvector * (nvector/20 - 1) * (1 - nvector/200))
plot (nvector, fnvector, main = 'f(N)', xlab = 'N',
   ylab = 'f(N)', ylim = c(-10, 1000), type = 'l')
# For 20 < N < 200 (as well as N < 0), the population grows.
# For 0 < N < 20 and N > 200, the population decays.

nvector <- -20:300
gnvector <- (10 * (nvector^2) * (1 - nvector/200))
plot (nvector, gnvector, main = 'g(N)', xlab = 'N',
   ylab = 'g(N)', ylim = c(-10, 1000), type = 'l')
# For N < 0 and 0 < N < 200, the population grows.
# For N > 200, the population decays.


   #1(c)
# The principal difference in the dynamics of the two
# model systems is that in f(N), populations of a size
# below a certain threshold (in this case 20) decay to 0,
# whereas with g(N) any nonzero population grows to the
# nonzero fixed point (in this case 200).  f(N) could
# represent a situation in which a critical mass of
# individuals is necessary for long-term population
# survival.  g(N) would alternatively represent a
# situation in which any individual can give rise to
# a large population that will remain stable.


   #2(a)
dN/dt = f(N,t) = rN - sin (4t/2pi)


   #2(b)
See separate sheet


   #2(c)
## define the differential equation dx/dt = f(x,t)
instantGrowthRate <- function (x, t) {
   return (x - sin((4 * t) / (2 * pi)))
   }
## establish plotting parameters
tRange <- c(0, 15)
dt <- 0.5
xRange <- c(0, 2)
dx <- 0.1
h <- 0.7                   # delta t
k <- 0.5                   # aspect correction
tSteps <- diff (range (tRange)) / dt
xSteps <- diff (range (xRange)) / dx
## initialize the plot
plot (tRange, xRange, type = 'n',
   xlab = 't', ylab = 'x')
## main loop
for (ii in 0:(tSteps - 1)) {
   for (jj in 0:(xSteps - 1)) {
      t <- ii * dt                      # a t value
      x <- jj * dx                      # an x value
      m <- instantGrowthRate (x, t)     # slope
      u <- t + (h * dt)                 # a new t
      v <- x + (k * h * dt) * m         # a new x
      arrows (t, x, u, v, code = 2, length = 0.05)
      }
   }

# see additional file for plot
# there are no 'fixed points' in the sense
# that no initial condition will remain
# unchanged over time


   #2(d)
# see separate sheet for manual solution
# starting conditions:
# N(0) = 0
# N(0) = 3
# N(0) = 1
n0 <- 1
tvector <- 0:20
nvector <- (1.68 * sin(2*tvector / pi) 
   + 1.07 * cos(2*tvector / pi)
   + (n0 - 1.07) * exp(tvector))
plot (tvector, nvector, main = 'N(0) = 1',
   xlab = 't', ylab = 'x', type = 'l',
   xlim = c(0,50), ylim = c(-50, 50))

# see additional files for plots
# N(0) = 0 exponentially decreases
# N(0) = 3 exponentially grows
# N(0) = 1 grows at first, then exponentially decays
# These solutions all fit the vector field I plotted
# in 2(c), however my vector field does not encompass
# a large enough scope to trace all of these solutions
# to the same extent, in order to allow for visualization
# of the temporal differences in vector direction in
# the vector field plot.


  #3(a)
see separate sheet


  #3(b)
## define the differential equation dx/dt = f(x,t)
instantCurrent <- function (x, t) {
   return ((-101 * x - 6901.9) / 0.15)
   }
## establish plotting parameters
tRange <- c(0, .01)
dt <- 0.0005
xRange <- c(-100, 100)
dx <- 5
h <- 0.7                   # delta t
k <- 0.5                   # aspect correction
tSteps <- diff (range (tRange)) / dt
xSteps <- diff (range (xRange)) / dx
## initialize the plot
plot (tRange, xRange, type = 'n',
   xlab = 't', ylab = 'mV', main = 'Current Trends')
## main loop
for (ii in 0:(tSteps - 1)) {
   for (jj in -100:(xSteps - 1)) {
      t <- ii * dt                      # a t value
      x <- jj * dx                      # an x value
      m <- instantCurrent (x, t)        # slope
      u <- t + (h * dt)                 # a new t
      v <- x + (k * h * dt) * m         # a new x
      arrows (t, x, u, v, code = 2, length = 0.05)
      }
   }

# see additional file for plot
# this agrees with my manual fixed point
# analysis, showing a single stable fixed point
# at V = -68 mV


  #3(c)
# see separate sheet for manual analysis
v0 <- -55
tvector <- 0:10
vvector <- (-68.336 + ((v0 + 68.336) * exp(-673.3 * tvector)))
plot (tvector, vvector, main = 'V(0) = -55',
   xlab = 't', ylab = 'V', type = 'l',
   xlim = c(0,10), ylim = c(-100, 100))

# see additional files for plots
# The model predicts an almost instant
# jump to the equilibrium value, from
# any starting value.  This behavior
# was readily apparent from the vector
# field produced in 3(b), thus an analytical
# solution wasn't really necessary.


  #4(a)
define function f(N: REAL)           ## the differential equation
define range of N : nRange: REAL     ## range of N to be considered
solve f(N) for all N: solution <- f(nRange)
initialize vectors for pos & neg f(N) values
   aboveAxis: REAL empty vector
   belowAxis: REAL empty vector
create zero-filled vector
   zeroVector <- c(0)
for i: INT in nRange
begin
  if f(N) > 0, store the value of N in aboveAxis
  else if f(N) < 0, store the value of N in belowAxis
end
plot the solution curve f(N) for all N in nRange
add the x-axis
draw horizontal rightward arrows for all aboveAxis
draw horizontal leftward arrows for all belowAxis


  #4(b)
diffEq <- function (N) {
   return ((N/3) * (1 - N/5))
   }
nRange <- ((-10:20)/2)
solutionCurve <- diffEq (nRange)
aboveXAxis <- c()
belowXAxis <- c()
zeroFillerVector <- c(0)
for (ii in 1:length (nRange)) {
   if (solutionCurve [ii] > 0) {
      aboveXAxis <- c(aboveXAxis, nRange [ii])
      } 
   else if (solutionCurve [ii] < 0) {
      belowXAxis <- c(belowXAxis, nRange [ii])
      }
   }
plot (nRange, solutionCurve, type = 'l',
   main = 'Flow Lines', xlab = 'N',
   ylab = 'f(N)', ylim = c(-2,2))
lines (c(min(nRange), max(nRange)), c(0, 0))
arrows (aboveXAxis, zeroFillerVector,
   (aboveXAxis + h), zeroFillerVector,
   length = 0.08)
arrows (belowXAxis, zeroFillerVector,
   (belowXAxis - h), zeroFillerVector,
   length = 0.08)

# see additional file for plot
