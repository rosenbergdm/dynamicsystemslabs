# 
# name: Erin Mowers
# assignment: Chapter 3 Exercise
# date: 10/30/09
# filename: Mowers_Ch3_Exercise
#############################################################################






#/--#########################################################################
# name: Erin Mowers
# assignment: Chapter 3 Exercise
# date: 10/30/09
# question: 1
# subquestion: a
# other files: See submitted hard copy.
##########################################################################/--




#/--#########################################################################
# name: Erin Mowers
# assignment: Chapter 3 Exercise
# date: 10/30/09
# question: 1
# subquestion: b
# other files: See submitted hard copy for copies of graphs; code is below
##########################################################################/--

f <- function(N,r,K) {
    return(r*N*(N/20-1)*(1-N/K))
    }
g <- function(N,r,K) {
    return(r*N^2*(1-N/K))
    }
plot(1:250, g(1:250,10,200), xlim=c(0,210), ylim=c(-50000,56000),
    main='Mowers 1b: g(N)', xlab='N', ylab='dN/dt',
   type='l',col='red')
plot(1:250, f(1:250,10,200), xlim=c(0,210), ylim=c(-3000,2500), 
   main='Mowers 1b: f(N)', xlab='N', ylab='dN/dt',
   type='l',col='blue')



#/--#########################################################################
# name: Erin Mowers
# assignment: Chapter 3 Exercise
# date: 10/30/09
# question: 1
# subquestion: c
# other files: See submitted hard copy.
##########################################################################/--




#/--#########################################################################
# name: Erin Mowers
# assignment: Chapter 3 Exercise
# date: 10/30/09
# question: 2
# subquestion: a
# other files: See submitted hard copy.
##########################################################################/--




#/--#########################################################################
# name: Erin Mowers
# assignment: Chapter 3 Exercise
# date: 10/30/09
# question: 2
# subquestion: b
# other files: See submitted hard copy.
##########################################################################/--




#/--#########################################################################
# name: Erin Mowers
# assignment: Chapter 3 Exercise
# date: 10/30/09
# question: 1
# subquestion: b
# other files: See submitted hard copy for print outs of graphs; code is below.
##########################################################################/--

bact_growth <- function(N,t,r) {
    return(r*N - sin(2*t/pi))
    }

#This direction field highlights global changes for large N#
tRange <- c(0,5)
dt <- 0.2
xRange <- c(0,5000)
dx <- 100
h <- 0.5
k <- 0.5
tSteps <- diff(range(tRange)) / dt
xSteps <- diff(range(xRange)) / dx
plot(tRange, xRange, type='n', main='Mowers 2b: dN/dt = N - sin(4t/2pi)', xlab='t', ylab='x')
for(ii in 0:(tSteps -1)) {
    for(jj in 0:(xSteps-1)) {
        t <- ii * dt
        x <- jj * dx
        m <- bact_growth(x,t,1)
        u <- t + (h * dt)
        v <- x + (k * h * dt) * m
        arrows(t, x, u, v, code=2, length=0.05)
        }
    }

#This direction field highlights how t affects slope#
tRange <- c(0,5)
dt <- 0.2
xRange <- c(0,3)
dx <- 0.05
h <- 0.5
k <- 0.5
tSteps <- diff(range(tRange)) / dt
xSteps <- diff(range(xRange)) / dx
plot(tRange, xRange, type='n', main='Mowers 2b: dN/dt = N - sin(4t/2pi)',
    xlab='t', ylab='x')
for(ii in 0:(tSteps -1)) {
    for(jj in 0:(xSteps-1)) {
        t <- ii * dt
        x <- jj * dx
        m <- bact_growth(x,t,1)
        u <- t + (h * dt)
        v <- x + (k * h * dt) * m
        arrows(t, x, u, v, code=2, length=0.05)
        }
    }




#/--#########################################################################
# name: Erin Mowers
# assignment: Chapter 3 Exercise
# date: 10/30/09
# question: 2
# subquestion: d
# other files: See submitted hard copy for solution of ODE,
#              print outs of plots, and explanation.
#              Code for plotting is below
##########################################################################/--

#Solution of ODE#
bacteria <- function(t, r, N_0) {
    return(((pi / ((pi^2 * r^2) + 4)) *
        ((pi * r * sin(2*t/pi)) + (2 * cos(2*t/pi)) - (2 * exp(r*t)))) +
       (N_0 * exp(r*t)))}

plot(0:5, bacteria(0:5,1,100),
    main='Mowers 2d: Bacteria Culture',
    xlab='t time (days)',
    ylab='N population size',
    type='l',col='red')
lines(0:20, bacteria(0:20,1,50), type='l', col='blue')
lines(0:20, bacteria(0:20,1,10), type='l', col='green')
legend(0,5000,c('N_0 = 500', 'N_0 = 150', 'N_0 = 100'),
     col=c('red', 'blue', 'green'))




#/--#########################################################################
# name: Erin Mowers
# assignment: Chapter 3 Exercise
# date: 10/30/09
# question: 3
# subquestion: a
# other files: See submitted hard copy.
##########################################################################/--




#/--#########################################################################
# name: Erin Mowers
# assignment: Chapter 3 Exercise
# date: 10/30/09
# question: 3
# subquestion: b
# other files: See submitted hard copy for prints of graphs and commentary.
#              Code is below.
##########################################################################/--

memVoltage <- function(V) {
    return(1/0.15 * ((-(V - 58.1)) - (100 * (V + 69.6))))
    }

tRange <- c(0,1)
dt <- 0.02
xRange <- c(-500,500)
dx <- 25
h <- 0.006
k <- 1
tSteps <- diff(range(tRange)) / dt
xSteps <- diff(range(xRange)) / dx

plot(c(0,1), c(-150,100), type='n', main='Mowers 3b: dV/dt', xlab='t', ylab='V')

for(ii in 0:(tSteps -1)) {
    for(jj in 0:(xSteps-1)) {
        t <- ii * dt
        x <- jj * dx
        m <- memVoltage(x)
        u <- t + (h * dt)
        v <- x + (k * h * dt) * m
        arrows(t, x, u, v, code=2, length=0.05)
        }
    }
for(ii in 0:(tSteps -1)) {
    for(jj in 0:(xSteps-1)) {
        t <- ii * dt
        x <- -jj * dx
        m <- memVoltage(x)
        u <- t + (h * dt)
        v <- x + (k * h * dt) * m
        arrows(t, x, u, v, code=2, length=0.05)
        }
    }






#/--#########################################################################
# name: Erin Mowers
# assignment: Chapter 3 Exercise
# date: 10/30/09
# question: 3
# subquestion: c
# other files: See submitted hard copy for ODE solution (work), 
               prints of plots, and analysis. Code is below.
##########################################################################/--

Voltage <- function(t,V_0) {
    return((((58.1*1)+(-69.6*100))/(100+1)) - 
        ((((58.1*1)+(-69.6*100))/(100+1)) * exp(-(1/0.15)*(1+100)*t)) +
        (V_0 * exp(-(1/0.15)*(1+100)*t)))
    }
t_values <- seq(0, 0.05, by=0.0001)
plot(t_values, Voltage(t_values,80), ylim=c(-150,100),
    main='Mowers 3c: V(t)', xlab='time', ylab='V(t)',
    type='l', col='red')
lines(t_values, Voltage(t_values,-40), type='l', col='blue')
lines(t_values, Voltage(t_values,-100), type='l', col='green')
legend(0.035,0,c('N_0 = 80', 'N_0 = -40', 'N_0 = -100'),
     col=c('red', 'blue', 'green'))




#/--#########################################################################
# name: Erin Mowers
# assignment: Chapter 3 Exercise
# date: 10/30/09
# question: 4
# subquestion: a
# other files: NA
##########################################################################/--

define function F(x: REAL)          // single-variable ODE
var xmin, xmax : REAL               // plot limits of x
var dx : REAL                       // step size
var M : INT                         // number of steps
real array xrange [xmin..xmax]
draw F(x)

xrange <- seq(xmin, xmax, dx)  
M <- length(xrange)

for i : INT in 0 to M
begin
    if F(xrange[i]) > 0
    begin
        draw [(xrange[i], 0), (xrange[i + 1],0)]     //  arrow to right
    end
    else if F(xrange[i]) < 0
    begin
        draw [(xrange[i + 1],0), (xrange[i], 0)]   //  arrow to left
    end
end
        
        



#/--#########################################################################
# name: Erin Mowers
# assignment: Chapter 3 Exercise
# date: 10/30/09
# question: 4
# subquestion: b
# other files: See submitted hard copy for explaination and print out; code below.
##########################################################################/--

f <- function(x) {
    return(x / 3 * (1 - (x / 5)))
    }
xmin <- -5
xmax <- 10
dx <- 1
xrange <- seq(xmin, xmax, dx)
M <- length(xrange)

plot(xmin:xmax, f(xmin:xmax), type='l', col='blue',
    main='Mowers 4b: Flow lines', xlab='x', ylab='f(x)')

for(ii in 1:M) {
    if (f(xrange[ii]) > 0) {
        arrows(xrange[ii], 0, xrange[ii+1], 0, code=1, length=0.1, col='green')}
    else if (f(xrange[ii]) < 0) {
        arrows(xrange[ii], 0, xrange[ii+1], 0, code=2, length=0.1, col='green')}
    }




