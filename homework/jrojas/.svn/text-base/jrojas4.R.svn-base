#!/usr/bin/env r
# encoding: utf-8
# 
# name: Jose Rojas
# assignment: 4
# date: November 12 2009
# filename: jrojas4.R
#############################################################################


#/--#########################################################################
# name: Jose Rojas
# assignment: 3
# date: Novermber 5 2009
# question: 1
# subquestion: c
# other files:
##########################################################################/--

 # ARGUMENTS model function, step size, range of value over which to
 #           calculate the solution, and intial conditions.
 # RETURTNS  An array with a column of x values for a plot and a column of
 #           of the forward euler approximated solution for the plot

forwardEuler <- function (model, step, inter, intial){
  time <- seq(inter[1],inter[2],by=step)
  forward <- numeric(length(time))
  forward[1] <- intial
  for (i in 1:(length(time)-1)){
    forward[i+1] <- forward[i] + step * model(forward[i],time[i])
  }
  cbind(time,forward)
}

 ## Epidemic model
epedemic <- function(N, beta, gamma) {
  ret <- function (I,t){
    beta * (N - I) * I - gamma * I
  }
  ret
}

one.c <- epedemic(1, 0.3, 0.1)

 ## Log axis is unwarranted but it makes comparisons of the different
 ## step sizes easier
plot(NA, ylim=c(0,1), xlim=c(1,365), log="x",
     xlab="Time (days)", ylab="Fraction infected",
     main="1.(c) Forward Euler Aproximation of Epidemic Model",
     sub="beta=0.3/day  gamma=0.1/day)")
stp <- c(0.1, 0.5, 1, 5, 7.5, 10)
col <- rainbow(length(stp),v=0.85)
for (i in 1:length(stp))
  lines( forwardEuler(one.c, stp[i], c(0,365), 0.01), col=col[i] )
legend(100,.5,stp,col)

#/--#########################################################################
# name: Jose Rojas
# assignment: 3
# date: Novermber 5 2009
# question: 1
# subquestion: d
# other files:
##########################################################################/--

one.d <- epedemic(1, 0.1, 0.2)
 ## Changed intial condition to more interesting value
plot(NA, ylim=c(-0.5,1), xlim=c(1,365), log="x",
     xlab="Time (days)", ylab="Fraction infected",
     main="1.(d) Forward Euler Aproximation of Epidemic Model",
     sub="beta=0.1/day  gamma=0.2/day)")
stp <- c(0.1, 0.5, 1, 2, 4, 6, 8)
col <- rainbow(length(stp),v=0.85)
for (i in 1:length(stp))
  lines( forwardEuler(one.d, stp[i], c(0,365), 0.99), col=col[i] )
legend(100,.5,stp,col)


#/--#########################################################################
# name: Jose Rojas
# assignment: 3
# date: Novermber 5 2009
# question: 2
# subquestion: d
# other files:
##########################################################################/--

interval <- c(0,1000000)    ## Interval for Forward Euler aproximation
T <- 10^-2                  ## Because intial C = 0
Kminus <- 10^-5
Kplus <- 10^-3
S <- 10^-6
R0 <- 10^-2

receptor <- function(R,t){
  (T - R) * Kminus - S * Kplus * R
}
EQ <- (T * Kminus) / (S * Kplus + Kminus)

 ## A list to store the foward euler approximations for different step sizes
FEoutput <- list()
stp <- c(1000,10000,20000,40000,80000)
col <- rainbow(length(stp),v=0.85)
for (j in 1:length(stp)){
  FEoutput[[j]] <- forwardEuler(receptor,stp[j],interval,R0)
}
plot(NA, ylim=c(EQ,R0), xlim=interval,
     xlab="Time (seconds)", ylab="Amount of receptor molecule",
     main="2.(d) Forward Euler Aproximation of Signal Receptor Model")
for (k in 1:length(stp)){
  lines(FEoutput[[k]], col=col[k])
}

receptorSol <- function(t){
  EQ + (R0 - EQ) * exp(-(Kminus+S*Kplus) * t)
}
lines(0:1000000,receptorSol(0:1000000),lwd=2)

legend(6e5,R0,c(stp,"Sol"),c(col,"black"))


#/--#########################################################################
# name: Jose Rojas
# assignment: 3
# date: Novermber 5 2009
# question: 2
# subquestion: e
# other files:
##########################################################################/--

 ## ARGUMENTS the numerical solution produced by forwardEuler function
 ##           or a similar function, and a function for the analytical
 ##           solution of the model
 ## RETURNS   total error the  of numerical approximation averaged across
 ##           time interval
totErr <- function(numerical,analytical){
   ## A vector to hold the appropriate values of the analytical solution
  dA <- analytical(numerical[,1])
  error <- abs( dA - numerical[,2])
  mean(error)
}

meanErr <- sapply(FEoutput,totErr,receptorSol)

plot(log(stp),log(meanErr),
     main="2.e) Order of approximation method plot",
     xlab="log size of time step (seconds)", ylab="log Total Error")
modelOrder <- lm(log(meanErr)~log(stp))
summary(modelOrder)

 ## The regression analysis shows that the order of the model is 1.015099

#/--#########################################################################
# name: Jose Rojas
# assignment: 3
# date: Novermber 5 2009
# question: 2
# subquestion: f
# other files:
##########################################################################/--

 ## ARGUMENTS model function, step size, range of value over which to
 ##           calculate the solution, intial conditions
 ## RETURTNS  An array with a column of x values for a plot and a column of
 ##           of the backwar euler approximated solution for the plot

backwardEuler <- function (model, step, inter, intial){
   ## function to solve to get next value for backward Euler approximation
  backwardFun <- function(Xplus, X, time){
    X + step * model(Xplus,time) - Xplus
  }
   ## Interval over which to look for soutions
   ## I realize that this is very inefficient but I couldn't think of a
   ## a better way to do it while preserving generality
  solInter <- extendrange(forwardEuler(model,step,inter,intial)[,2])
  
  time <- seq(inter[1],inter[2],by=step)
  backward <- numeric(length(time))
  backward[1] <- intial
  for (i in 1:(length(time)-1)){
    temp  <- uniroot(backwardFun, solInter, tol=10^-10, backward[i], time[i])
    backward[i+1] <- temp$root
  }
  cbind(time,backward)
}

BEoutput <- list()
stp <- c(1000,10000,20000,40000,80000,120000)
col <- rainbow(length(stp),v=0.85)
for (j in 1:length(stp)){
  BEoutput[[j]] <- backwardEuler(receptor,stp[j],interval,R0)
}
plot(NA, ylim=c(EQ,R0), xlim=interval,
     xlab="Time (seconds)", ylab="Amount of receptor molecule",
     main="2.(e) Backward Euler Aproximation of Signal Receptor Model")
for (k in 1:length(stp)){
  lines(BEoutput[[k]], col=col[k])
}

receptorSol <- function(t){
  EQ + (R0 - EQ) * exp(-(Kminus+S*Kplus) * t)
}
lines(0:1000000,receptorSol(0:1000000),lwd=2)

legend(6e5,R0,c(stp,"Sol"),c(col,"black"))


#/--#########################################################################
# name: Jose Rojas
# assignment: 3
# date: Novermber 5 2009
# question: 2
# subquestion: g
# other files:
##########################################################################/--

meanErr <- sapply(BEoutput,totErr,receptorSol)

plot(log(stp),log(meanErr),
     main="2.e) Order of approximation method plot",
     xlab="log size of time step (seconds)", ylab="log Total Error")
modelOrder <- lm(log(meanErr)~log(stp))
summary(modelOrder)

 ## The regression analysis shows that the order of the model is 0.94593
 ## This value is less than the one found for the forward euler method
