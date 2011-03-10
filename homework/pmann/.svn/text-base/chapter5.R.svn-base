#!/usr/bin/env r
# encoding: utf-8
# 
# name: Patrick Mann
# assignment: 6
# date: 11-22-09
# filename: Chapter 5 Exercises
#############################################################################

#/--#########################################################################
# name: Patrick Mann
# assignment: 6
# date: 11-22-09
# question: 1
# subquestion: a
# other files: NAMES_OF_OTHER_RELATED_FILES
##########################################################################/--

disLogMod <- function(r, x0, nIter=500) {
  xArray <- numeric(length=nIter)
  xArray[1] <- x0
  for (ii in 1:nIter){
  	xArray[ii+1] <- r*xArray[ii]*(1 - xArray[ii])
  }
  return(xArray)
}

#/--#########################################################################
# name: Patrick Mann
# assignment: 6
# date: 11-22-09
# question: 1
# subquestion: b
# other files: NAMES_OF_OTHER_RELATED_FILES
##########################################################################/--

disLogMod <- function(r, x0, nIter=500) {
  xArray <- numeric(length=nIter)
  xArray[1] <- x0
  for (ii in 1:nIter){
  	xArray[ii+1] <- r*xArray[ii]*(1 - xArray[ii])
  }
  return(xArray)
}

rRange <- c(0,4)
rStep <- 1e-2
rVals <- seq(rRange[1], rRange[2], by=rStep)

dlmMatrix <- matrix(numeric(301*length(rVals)), nrow=301)

x0 <- 1e6

for (ii in 1:length(rVals)) {
  dlmMatrix[ ,ii] <- disLogMod(rVals[ii], x0)[201:501]
}

#/--#########################################################################
# name: Patrick Mann
# assignment: 6
# date: 11-22-09
# question: 1
# subquestion: c
# other files: NAMES_OF_OTHER_RELATED_FILES
##########################################################################/--

disLogMod <- function(r, x0, nIter=500) {
  xArray <- numeric(length=nIter)
  xArray[1] <- x0
  for (ii in 1:nIter){
  	xArray[ii+1] <- r*xArray[ii]*(1 - xArray[ii])
  }
  return(xArray)
}

rRange <- c(0,4)
rStep <- 1e-2
rVals <- seq(rRange[1], rRange[2], by=rStep)

dlmMatrix <- matrix(numeric(301*length(rVals)), nrow=301)

x0 <- 1e-6
plot(c(0,4), c(0,1), type='n', main='Bifurcation Plot', 
  ylab='Xt', xlab='r')

for (ii in 1:length(rVals)) {
  dlmMatrix[ ,ii] <- disLogMod(rVals[ii], x0)[201:501] 
  points(rep(rVals[ii], 301), dlmMatrix[ ,ii], pch=19, cex=0.01)
}

#/--#########################################################################
# name: Patrick Mann
# assignment: 6
# date: 11-22-09
# question: 1
# subquestion: d
# other files: NAMES_OF_OTHER_RELATED_FILES
##########################################################################/--

disLogMod <- function(r, x0, nIter=500) {
  xArray <- numeric(length=nIter)
  xArray[1] <- x0
  for (ii in 1:nIter){
  	xArray[ii+1] <- r*xArray[ii]*(1 - xArray[ii])
  }
  return(xArray)
}

nIter <- 500
x0 <- 1e-6
snglFx <- disLogMod(2, x0)[201:501]
dblFx <- disLogMod(3, x0)[201:501]
nFx <- disLogMod(3.4, x0)[201:501]
Chaos <- disLogMod(4, x0)[201:501]
cols <- rainbow(4)

plot(201:(nIter+1), nFx, main='Behavior at Dif. Bifurcations',
  xlab='X_t', ylab='X_t+1', col=cols[1], type='l' 
)
lines(201:(nIter+1), snglFx, lwd=3, col=cols[2])
lines(201:(nIter+1), dblFx, col=cols[3])
points(201:(nIter+1), Chaos, pch=13, cex=1, col=cols[4])

#/--#########################################################################
# name: Patrick Mann
# assignment: 6
# date: 11-22-09
# question: 2
# subquestion: a
# other files: NAMES_OF_OTHER_RELATED_FILES
##########################################################################/--

# Generally, x is the number of mature individuals 
#   in a population and y is the number of juvenile 
#   individuals in a population.  In this matrix, '1' 
#   represents the number of individuals a juvenile 
#   individual will reproduce and '0.3' represents the 
#   percentage of juveniles that will survive into the 
#   next time step.  '2' represents the number of 
#   individuals a mature individual reproduce and '0' 
#   represents the percentage of mature individuals 
#   that will survive into the next time step.

#/--#########################################################################
# name: Patrick Mann
# assignment: 6
# date: 11-22-09
# question: 2
# subquestion: b
# other files: NAMES_OF_OTHER_RELATED_FILES
##########################################################################/--

Les <- matrix(c(0.5, 0.1, 2, 0), nrow=2)
LesliePop <- function (x0, y0, nIter=30) {
  Pop_array <- matrix(numeric((nIter+1)*2), nrow=2)
  Pop_array[,1] <- c(x0, y0) 
  for (ii in 1:nIter) {
    Pop_array[ ,(ii+1)] <- Les%*%Pop_array[ ,ii]
  }
  return(Pop_array)
}

#/--#########################################################################
# name: Patrick Mann
# assignment: 6
# date: 11-22-09
# question: 2
# subquestion: c
# other files: NAMES_OF_OTHER_RELATED_FILES
##########################################################################/--

Les <- matrix(c(0.5, 0.1, 2, 0), nrow=2)
LesliePop <- function (x0, y0, nIter=30) {
  Pop_array <- matrix(numeric((nIter+1)*2), nrow=2)
  Pop_array[,1] <- c(x0, y0) 
  for (ii in 1:nIter) {
    Pop_array[ ,(ii+1)] <- Les%*%Pop_array[ ,ii]
  }
  return(Pop_array)
}

plot(1:31,LesliePop(30,20)[1,], type='l',
  main='Leslie Matrix Plot',
  xlab='Time Step', ylab='Population Size'
)
lines(1:31, LesliePop(3,4)[1,])
lines(1:31, LesliePop(1,1)[1,])

# Even with different starting values, the plots 
#   all appear to converge into a similar exponential plot.  
#   If the y-axis were logarithmic (not shown in this answer), 
#   we would see the plots all converging to a line 
#   with the same negative slope.

#/--#########################################################################
# name: Patrick Mann
# assignment: 6
# date: 11-22-09
# question: 2
# subquestion: d
# other files: NAMES_OF_OTHER_RELATED_FILES
##########################################################################/--

Les <- matrix(c(0.5, 0.1, 2, 0), nrow=2)
LesliePop <- function (x0, y0, nIter=30) {
  Pop_array <- matrix(numeric((nIter+1)*2), nrow=2)
  Pop_array[,1] <- c(x0, y0) 
  for (ii in 1:nIter) {
    Pop_array[ ,(ii+1)] <- Les%*%Pop_array[ ,ii]
  }
  return(Pop_array)
}

plot(1:31,LesliePop(1,2)[1,], type='l',
  main='Leslie Matrix Plot',
  xlab='Time Step', ylab='Population Size'
)

largestEigenvalue <- LesliePop(1,2)[1 ,31]/
  LesliePop(1,2)[1 ,30]
largestEigenvalue

#/--#########################################################################
# name: Patrick Mann
# assignment: 6
# date: 11-22-09
# question: 3
# subquestion: a
# other files: NAMES_OF_OTHER_RELATED_FILES
##########################################################################/--

# Parameter "a" represents the percent of mature individuals 
#   that will survive into the next time step.

#/--#########################################################################
# name: Patrick Mann
# assignment: 6
# date: 11-22-09
# question: 3
# subquestion: b
# other files: NAMES_OF_OTHER_RELATED_FILES
##########################################################################/--

UsherPop <- function (a, x0, y0, nIter=30) {
  Ush <- matrix(numeric(4), nrow=2)
  Ush <- matrix(c(0.5, 0.1, 2, a), nrow=2)
  Pop_array <- matrix(numeric((nIter+1)*2), nrow=2)
  Pop_array[,1] <- c(x0, y0) 
  for (ii in 1:nIter) {
    Pop_array[ ,(ii+1)] <- Ush%*%Pop_array[ ,ii]
  }
  return(Pop_array)
}

cols <- rainbow(10)

plot(1:31, UsherPop(0.7, 1, 2)[1,], type='l', col=cols[1],
  main='Usher Matrix Plot on Either Side of Parameter "a" Bifurcation',
  xlab='Time Step', ylab='Population Size'
)

lines(1:31, UsherPop(0.1, 1, 2)[1,], col=cols[1])
lines(1:31, UsherPop(0.2, 1, 2)[1,], col=cols[2])
lines(1:31, UsherPop(0.3, 1, 2)[1,], col=cols[3])
lines(1:31, UsherPop(0.4, 1, 2)[1,], col=cols[4])
lines(1:31, UsherPop(0.5, 1, 2)[1,], col=cols[5])
lines(1:31, UsherPop(0.6, 1, 2)[1,], col=cols[6])
lines(1:31, UsherPop(0.7, 1, 2)[1,], col=cols[7])
lines(1:31, UsherPop(0.8, 1, 2)[1,], col=cols[8])
lines(1:31, UsherPop(0.9, 1, 2)[1,], col=cols[9])
lines(1:31, UsherPop(1, 1, 2)[1,], col=cols[10])

legend(20, 400, bg='white', legend=c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1), col=cols, pch=19)

Ush <- matrix(c(0.5, 0.1, 2, 1), nrow=2)
eigen(Ush)

# The population hits equilibrium at a=0.6. 
#   The largest eigenvalue is at a=1 and 
#   is approximately equal to 1.2623475.

#/--#########################################################################
# name: Patrick Mann
# assignment: 6
# date: 11-22-09
# question: 3
# subquestion: c
# other files: NAMES_OF_OTHER_RELATED_FILES
##########################################################################/--

UsherPop <- function (a, x0, y0, nIter=30) {
  Ush <- matrix(numeric(4), nrow=2)
  Ush <- matrix(c(0.5, 0.1, 2, a), nrow=2)
  Pop_array <- matrix(numeric((nIter+1)*2), nrow=2)
  Pop_array[,1] <- c(x0, y0) 
  for (ii in 1:nIter) {
    Pop_array[ ,(ii+1)] <- Ush%*%Pop_array[ ,ii]
  }
  return(Pop_array)
}

aRange <- c(-1, 0.7)
aStep <- 0.0001
aVals <- seq(aRange[1], aRange[2], by=aStep)

storageMatrix <- matrix(numeric(31*length(aVals)), nrow=31)

x0 <- 1
y0 <- 0
plot(aRange, c(-0.2, 0.7), type='n', 
  main='Bifurcation Plot', 
  ylab='Usher Matrix', xlab='a Values')

for (ii in 1:length(aVals)) {
  storageMatrix[ ,ii] <- UsherPop(aVals[ii], x0, y0)[1, ] 
  points(rep(aVals[ii], 31), storageMatrix[ ,ii], pch=19, cex=0.05)
}

Ush <- matrix(c(0.5, 0.1, 2, 0.6), nrow=2)
eigen(Ush)

# The two bifurcation values of parameter "a" appear 
#   between -0.85 and -0.86 and around 0.6.  
#   Calculated algebraically, the largest Eigenvalue 
#   at a=0.6 is equal to 1.0.

plot(1:31, UsherPop(-1, 1, 2)[1,], type='l', col='red',
  main='Usher Matrix Plot on Either Side of Parameter "a" Bifurcation',
  xlab='Time Step', ylab='Population Size'
)

lines(1:31, UsherPop(0.7, 1, 2)[1,], col='blue')
lines(1:31, UsherPop(0.6, 1, 2)[1,], col='green')
lines(1:31, UsherPop(0.3, 1, 2)[1,], col='purple')
