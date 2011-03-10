# name: Christina Baer
# assignment: Ch 4
# date: 11/8/09
# filename: Bios Lab Ch 4.R
#############################################################################

#/--#########################################################################
# name: Christina Baer
# assignment: Ch 4
# date: 11/8/09
# question: 1
# subquestion: a
# other files: 
##########################################################################/--
# B = beta, y = gamma
# dI / dt = B * (N - I) * I - y * I = f(I)
# dI / dt = B * N * I - B * I^2 - y * I
# Fixed points when f(I) = 0.
# 0 = B * N * I - B * I^2 - y * I
# 0 = I * (B * N - B * I - y)
# I* = 0
# 0 = B * N - B * I - y
# B * I = B * N - y
# I* = N - (y / B)
# The only fixed point which is of biological interest is I* = N - (y / B).  
# This means that when I equals the entire population minus a relation of the
# infection and recovery parameters, a constant proportion of the population 
# will be infected.

#/--#########################################################################
# name: Christina Baer
# assignment: Ch 4
# date: 11/8/09
# question: 1
# subquestion: b
# other files: 
##########################################################################/--
# f'(I) = [B * N - y - 2 * B * I] * dI

# f'(0) = B * N - y
# B * N - y < 0
# B * N < y
# I* = 0 is stable when y > B (assuming that N equals 1 and I is the 
# infected fraction of the population.

# f'(N - (y / B)) = B * N - 2 * B * (N - (y / B)) - y
# f'(N - (y / B)) = B * N - 2 * B * N - 2 * y - y
# f'(N - (y / B)) = -B * N - 3 * y
# -B * N - 3 * y < 0
# -B * N < 3 * y
# I* = N - (y / B) is always stable because B and y are both positive 
# parameters, and -B will always be less than any positive multiple of y.

# The stability of the second fixed point means that if the infected portion
# of the population approaches N - (y / B), the disease will maintain itself
# within the population in a stable portion of that population.
# The stability of the fixed point at 0 means that if the disease dies out,
# it cannot reappear.  This matches the assumption that the population is 
# closed and has no outside contact with the disease.

#/--#########################################################################
# name: Christina Baer
# assignment: Ch 4
# date: 11/8/09
# question: 1
# subquestion: c
# other files: BiosLabCh4-CSB-1c.pdf
##########################################################################/--
# PDF copy of plots
pdf(file="BiosLabCh4-CSB-1c.pdf")

f <- function(I, t) {
return (0.3 * (1 - I) * I - 0.1 * I)
}

forwardEuler <- function(f, max_iter, step, I0, t0) {
Ivals<- tvals <- numeric(length=(max_iter+1))
Ivals[1]<- I0
tvals[1]<- t0
for (ii in 1:max_iter) {
Ivals[ii+1]<- Ivals[ii] + step * f(Ivals[ii], tvals[ii])
tvals[ii+1]<- tvals[ii] + step
}
return(data.frame(t=tvals, I=Ivals))
}

step_size<- 0.1
nSteps<- 365 / step_size
fwdeulerI1<- forwardEuler(f, nSteps, 0.1, 0.01, 0)
fwdeulerI1$I[length(fwdeulerI1$x)]
plot(fwdeulerI1, type='l', main="Question 1c, Infection Model with Step Size 
0.1", xlab="Time (days)", ylab="I (fraction infected)") 

step_size<- 1
nSteps<- 365 / step_size
fwdeulerI2<- forwardEuler(f, nSteps, 1, 0.01, 0)
fwdeulerI2$I[length(fwdeulerI2$x)]
plot(fwdeulerI2, type='l', main="Question 1c, Infection Model with Step Size 
1", xlab="Time (days)", ylab="I (fraction infected)")

step_size<- 10
nSteps<- 365 / step_size
fwdeulerI3<- forwardEuler(f, nSteps, 10, 0.01, 0)
fwdeulerI3$I[length(fwdeulerI3$x)]
plot(fwdeulerI3, type='l', main="Question 1c, Infection Model with Step Size 
10", xlab="Time (days)", ylab="I (fraction infected)")

dev.off()

# I* = 1 - (0.1 / 0.3) = 2 / 3.  This holds true for all three plots, 
# although the time step = 10 plot still has noticeable oscillations around
# I = 2 / 3.

#/--#########################################################################
# name: Christina Baer
# assignment: Ch 4
# date: 11/8/09
# question: 1
# subquestion: d
# other files: BiosLabCh4-CSB-1d.pdf
##########################################################################/--
# PDF copy of plots
pdf(file="BiosLabCh4-CSB-1d.pdf")

f<- function(I, t) {
return (0.1 * (1 - I) * I - 0.2 * I)
}

step_size<- 0.1
nSteps<- 365 / step_size
fwdeulerI21<- forwardEuler(f, nSteps, 0.1, 0.01, 0)
fwdeulerI21$I[length(fwdeulerI21$x)]
plot(fwdeulerI21, type='l', main="Question 1d, Infection Model with Step Size 
0.1", xlab="Time (days)", ylab="I (fraction infected)") 

step_size<- 1
nSteps<- 365 / step_size
fwdeulerI22<- forwardEuler(f, nSteps, 1, 0.01, 0)
fwdeulerI22$I[length(fwdeulerI22$x)]
plot(fwdeulerI22, type='l', main="Question 1d, Infection Model with Step Size 
1", xlab="Time (days)", ylab="I (fraction infected)")

step_size<- 10
nSteps<- 365 / step_size
fwdeulerI23<- forwardEuler(f, nSteps, 10, 0.01, 0)
fwdeulerI23$I[length(fwdeulerI23$x)]
plot(fwdeulerI23, type='l', main="Question 1d, Infection Model with Step Size 
10", xlab="Time (days)", ylab="I (fraction infected)")

step_size<- 5
nSteps<- 365 / step_size
fwdeulerI24<- forwardEuler(f, nSteps, 5, 0.01, 0)
fwdeulerI24$I[length(fwdeulerI24$x)]
plot(fwdeulerI24, type='l', main="Question 1d, Infection Model with Step Size 
10", xlab="Time (days)", ylab="I (fraction infected)")

dev.off()


# At time step = 5 or 10, the model does not behave like when smaller time 
# steps are used.  Instead of exponentially decaying to 0, it actually 
# becomes less than 0 before stabilizing.

#/--#########################################################################
# name: Christina Baer
# assignment: Ch 4
# date: 11/8/09
# question: 2
# subquestion: a
# other files: 
##########################################################################/--
# k = k_(+)
# c = k_(-)

# dR / dt = R - k * S * R - c * k * S * R = f(R)


#/--#########################################################################
# name: Christina Baer
# assignment: Ch 4
# date: 11/8/09
# question: 2
# subquestion: b
# other files: 
##########################################################################/--
# k = k_(+)
# c = k_(-)

# Fixed points at f(R) = 0.
# 0 = R - k * S * R - c * k * S * R
# 0 = R * (1 - k * S - c * k * S)
# There is a single fixed point at R = 0
# Fixed points stable when f'(R*) < 0.
# f'(R) = 1 - k * S + c * k * S
# f'(0) = 1 - k * S + c * k * S
# 1 - k * S + c * k * S < 0
# 1 < k * S * (1 - c)
# This fixed point can only be stable if one of the parameters has an absolute
# value larger than 1, large enough to offset the other values.


#/--#########################################################################
# name: Christina Baer
# assignment: Ch 4
# date: 11/8/09
# question: 2
# subquestion: c
# other files: 
##########################################################################/--
# k = k_(+)
# c = k_(-)

# dR / dt = R * (1 - k * S + c * k * S)
# S dR / R = S [1 - k * S + c * k  * S] * dt
# ln |R| = (1 - k * S + c * k  * S) * t + C
# R = A * e^((1 - k * S + c * k  * S) * t)
# R(0) = R_0
# R_(0) = A * e^1
# R(t) = R_0 * e^((1 - k * S + c * k  * S) * t)

# LaTeX: R(t) = R_{0} e^{{(S c k - S k + 1)} t} 

#/--#########################################################################
# name: Christina Baer
# assignment: Ch 4
# date: 11/8/09
# question: 2
# subquestion: d
# other files: BiosLabCh4-CSB-2d.pdf
##########################################################################/--
# f(R) = R - k * S * R - c * k * S * R
# S = 10^-6
# k = 10^-3
# c = 10^-5
# f(R, t) = R - 10^-3 * 10^-6 * R - 10^-5 * 10^-3 * 10^-6 * R

f<- function(R, t) {
return (R - (10^-9 * R) + (10^-14 * R))
}

# PDF copy for plot
pdf(file="BiosLabCh4-CSB-2d")

step_size<- 0.01
nSteps<- (5 / step_size)
fwdeulerR<- forwardEuler(f, nSteps, 1, 10^-4, 0)
fwdeulerR$I[length(fwdeulerR$x)]
plot(fwdeulerR, type='l', main="Question 2d, Binding Model with Step Size 
0.1", sub="Christina Baer, Ch 4, 2d", xlab="Time (step)", 
ylab="Concentration of Free Receptors (M)") 

dev.off()
#/--#########################################################################
# name: Christina Baer
# assignment: Ch 4
# date: 11/8/09
# question: 2
# subquestion: e
# other files: BiosLabCh4-CSB-2e.pdf
##########################################################################/--
# S = 10^-6
# k = 10^-3
# c = 10^-5
# R_(0) = 10^-4
# R(t) = 10^-4 * e^((1 - 10^-3 * 10^-6 + 10^-5 * 10^3  * 10^-6) * t)

true<- function(t) {
return(10^-4 * exp((1 - 10^-9 + 10^-14) * t))
}

step_size<- 0.01
nSteps<- (10 / step_size)
fwdeulerR<- forwardEuler(f, nSteps, 1, 10^-4, 0)

# This finds the total error for each step by comparing the foward Euler and 
# true values.

terror<- function(g, fwdEname='', step_size, n, terrorsname) {
nSteps<- (n / step_size)
fwdEname<- g(f, nSteps, 1, 10^-4, 0)
terrorsname<- c()
for (ii in 0:nSteps) {
terrorsname[ii]<- abs(true(step_size * ii) - fwdEname$I[ii])
}
return (terrorsname)
}

# Finds the average error over the run of the approximation.

avterror<- function(terrorresult, avterrorsname='') {
sumterror<- sum(terrorresult)
avterrorsname<- sumterror / nSteps
return (avterrorsname)
} 

fwdE1<-terror(forwardEuler, fwdE1, 0.1, 100, toterror1)
fwdE2<-terror(forwardEuler, fwdE2, 1, 100, toterror2)
fwdE3<-terror(forwardEuler, fwdE3, 10, 100, toterror3)

avfwdE1<-avterror(fwdE1, avfwdE1)
avfwdE2<-avterror(fwdE2, avfwdE2)
avfwdE3<-avterror(fwdE3, avfwdE3)

avfwdE1
avfwdE2
avfwdE3

# avfwdE1 = 1.0715 * 10^295
# avfwdE2 = 4.2525 * 10^37
# avfwdE3 = 2.6882 * 10^37

#PDF copy of plot
pdf(file="BiosLabCh4-CSB-2e.pdf")

plot(1:1000, log(fwdE1), main="Total Error for Forward Euler, 
Time Step = 0.1", xlab="Time Step", ylab="log(Total Error)", type='l')

dev.off()

# The slope of the log(error) vs. time step plot is approximately 0.7.  Can 
# you have a 0.7 order method?

#/--#########################################################################
# name: Christina Baer
# assignment: Ch 4
# date: 11/8/09
# question: 2
# subquestion: f
# other files: BiosLabCh4-CSB-2f.pdf
##########################################################################/--
f<- function(R, t) {
return (R - (10^-9 * R) + (10^-14 * R))
}

# Basically the same as the foward Euler function, but uses the backward Euler
# value for N_(t + 1).

backwardEuler <- function(f, max_iter, step, I0, t0) {
Ivals<- tvals <- numeric(length=(max_iter))
Ivals[1]<- I0
tvals[1]<- t0
for (ii in 1:max_iter) {
Ivals[ii+1]<- Ivals[ii] / (1 + step * f(Ivals[ii], tvals[ii]))
tvals[ii+1]<- tvals[ii] + step
}
return(data.frame(t=tvals, I=Ivals))
}

# PDF copy of plot
pdf(file="BiosLabCh4-CSB-2f.pdf")

step_size<- 0.01
nSteps<- (5 / step_size)
bwdeuler1<-backwardEuler(f, nSteps, 1, 10^-4, 0)
plot(bwdeuler1, type='l', main="Question 2f, Binding Model with Step Size 
0.1", xlab="Time (step)", ylab="Concentration of Free Receptors (M)") 

dev.off()
#/--#########################################################################
# name: Christina Baer
# assignment: Ch 4
# date: 11/8/09
# question: 2
# subquestion: g
# other files: BiosLabCh4-CSB-2g.pdf
##########################################################################/--

bwdE1<-terror(backwardEuler, bwdE1, 0.1, 100, toterrorb1)
bwdE2<-terror(backwardEuler, bwdE2, 1, 100, toterrorb2)
bwdE3<-terror(backwardEuler, bwdE3, 10, 100, toterrorb3)

avbwdE1<-avterror(bwdE1, avbwdE1)
avbwdE2<-avterror(bwdE2, avbwdE2)
avbwdE3<-avterror(bwdE3, avbwdE3)

avbwdE1
avbwdE2
avbwdE3

# avbwdE1 = 5.6495 * 10^37
# avbwdE2 = 8.5050 * 10^36
# avbwdE3 = 5.3765 * 10^36

# PDF copy of plot
pdf(file="BiosLabCh4-CSB-2g.pdf")

plot(1:1000, log(bwdE1), main="Total Error for Backward Euler, 
Time Step = 0.1", xlab="Time Step", ylab="log(Total Error)", type='l')

dev.off()

# The slope appears to be approximately 1, so this is a first-order method.