# name: Christina Baer
# assignment: 3
# date: 10//09
# filename: Bios Lab 3.R
#############################################################################

## Chapter 2 ##

#/--#########################################################################
# name: Christina Baer
# assignment: 3
# date: 10//09
# question: 1
# subquestion: a
# other files: 
##########################################################################/--

# f(x) = 41 * N_(t) - 10 * N_(t)^2
# f'(x) = 41 - 20 * N_(t)

# Fixed points: 0, (r - 1) / k
# (r - 1) / k = (41 - 1) / 10 = 4
# f'(0) = 41, |f'(0)| > 1; unstable
# f'(4) = -39, |f'(4)| > 1; unstable

# There is no carrying capacity because both points are unstable.

#/--#########################################################################
# name: Christina Baer
# assignment: 3
# date: 10//09
# question: 1
# subquestion: b
# other files: 
##########################################################################/--

# f(x) = 41 * N_(t) + 2 * N_(t)^2
# f'(x) = 41 + 4 * N_(t)

# Fixed points: 0 and (r - 1) / k
# (r - 1) / k = (41 - 1) / -2 = -20
# f'(0) = 41, |f'(0)| > 1; unstable
# f'(-20) =1, |f'(4)| = 1; stability unclear

# There is no carrying capacity, since even if f'(-20) were stable, such a 
# condition would make no biological sense.

#/--#########################################################################
# name: Christina Baer
# assignment: 3
# date: 10//09
# question: 2
# subquestion: a
# other files: BiosLab3-CSB-2a.pdf
##########################################################################/--
cobwebFun<- function(x) {
return(3 * x - 0.75 * x ^ 2 + 1)
}

max_iter <- 50
y_vec <- x_vec <- numeric (max_iter * 2)
y_vec[1] <- 0
x_vec[1] <- 0.5
for (ii in 1:max_iter) {
y_vec[2 * ii] <- cobwebFun(x_vec[(2 * ii) - 1])
y_vec[2 * ii + 1] <- y_vec[2 * ii]
x_vec[2 * ii] <- x_vec[(2 * ii) - 1]
x_vec[2 * ii + 1] <- y_vec[2 * ii + 1]
}
plot2a<- plot(-10:max_iter / 10, cobwebFun(-10:max_iter / 10), xlab="x", 
ylab="y", main="Question 2a", type='l', xaxs='i', yaxs='i')
abline(0, 1, col='red')
lines(x_vec, y_vec, col='blue')

# PDF copy of plot
pdf(file="BiosLab3-CSB-2a.pdf")
plot2a<- plot(-10:max_iter / 10, cobwebFun(-10:max_iter / 10), xlab="x", 
ylab="y", main="Question 2a", type='l', xaxs='i', yaxs='i')
abline(0, 1, col='red')
lines(x_vec, y_vec, col='blue')
dev.off()


# There are fixed points at x = -0.431 and 3.097 and neither of them is 
# stable.
#/--#########################################################################
# name: Christina Baer
# assignment: 3
# date: 10//09
# question: 2
# subquestion: b
# other files: BiosLab3-CSB-2b.pdf
##########################################################################/--
cobwebFun<- function(x) {
return(100 * x - 2 * x ^ 2)
}

max_iter <- 70
y_vec <- x_vec <- numeric (max_iter * 2)
y_vec[1] <- 0
x_vec[1] <- 0.5
for (ii in 1:max_iter) {
y_vec[2 * ii] <- cobwebFun(x_vec[(2 * ii) - 1])
y_vec[2 * ii + 1] <- y_vec[2 * ii]
x_vec[2 * ii] <- x_vec[(2 * ii) - 1]
x_vec[2 * ii + 1] <- y_vec[2 * ii + 1]
}

plot2b<- plot(-10:70, cobwebFun(-10:70), xlab="x", ylab="y", 
main="Question 2b", type='l', xaxs='i', yaxs='i')
abline(0, 1, col='red')
lines(x_vec, y_vec, col='blue')

# PDF copy of plot
pdf(file="BiosLab3-CSB-2b.pdf")
plot2b<- plot(-10:70, cobwebFun(-10:70), xlab="x", ylab="y", 
main="Question 2b", type='l', xaxs='i', yaxs='i')
abline(0, 1, col='red')
lines(x_vec, y_vec, col='blue')
dev.off()

# There are fixed points at x = 0 and 49.5 and neither is stable.
#/--#########################################################################
# name: Christina Baer
# assignment: 3
# date: 10//09
# question: 2
# subquestion: c
# other files: BiosLab3-CSB-2c.pdf
##########################################################################/--

cobwebFun<- function(x) {
return(-100 * x + 0.5 * x ^ 2)
}

max_iter <- 250
y_vec <- x_vec <- numeric (max_iter * 2)
y_vec[1] <- 0
x_vec[1] <- .5
for (ii in 1:max_iter) {
y_vec[2 * ii] <- cobwebFun(x_vec[(2 * ii) - 1])
y_vec[2 * ii + 1] <- y_vec[2 * ii]
x_vec[2 * ii] <- x_vec[(2 * ii) - 1]
x_vec[2 * ii + 1] <- y_vec[2 * ii + 1]
}

plot2c<- plot(-10:max_iter, cobwebFun(-10:max_iter), xlab="x", ylab="y", 
main="Question 2c", type='l', xaxs='i', yaxs='i')
abline(0, 1, col='red')
lines(x_vec, y_vec, col='blue')

# PDF copy of plot
pdf(file="BiosLab3-CSB-2c.pdf")
plot2c<- plot(-10:max_iter, cobwebFun(-10:max_iter), xlab="x", ylab="y", 
main="Question 2c", type='l', xaxs='i', yaxs='i')
abline(0, 1, col='red')
lines(x_vec, y_vec, col='blue')
dev.off()

# There are fixed points at x = 0 and 202 and neither is stable.

#/--#########################################################################
# name: Christina Baer
# assignment: 3
# date: 10//09
# question: 3
# subquestion: 
# other files: 
##########################################################################/--
cobwebFun<- function(x) {
return(2 * x - 0.5 * x ** 2)
}

spider<-function(cobwebFun, title='', plotname='', minx, maxx) {
max_iter <- 200
y_vec <- x_vec <- numeric (max_iter * 2)
y_vec[1] <- 0
x_vec[1] <- 0.5
for (ii in 1:max_iter) {
y_vec[2 * ii] <- cobwebFun(x_vec[2 * ii - 1])
y_vec[2 * ii + 1] <- y_vec[2 * ii]
x_vec[2 * ii] <- x_vec[2 * ii - 1]
x_vec[2 * ii + 1] <- y_vec[2 * ii + 1]
plotname<-plot(minx:maxx, cobwebFun(minx:maxx), xlab="x", ylab="y", 
main=title, type='l', xaxs='i', yaxs='i')
abline(0, 1, col='red')
lines(x_vec, y_vec, col='blue')
}
return<-plotname
}
plot3<-spider(cobwebFun, "Question 3", plot3, 0, 5)

# PDF copy of plot
pdf(file="BiosLab3-CSB-3.pdf")
plot3<-spider(cobwebFun, "Question 3", plot3, 0, 5)
dev.off()


#/--#########################################################################
# name: Christina Baer
# assignment: 3
# date: 10//09
# question: 4
# subquestion: a
# other files: 
##########################################################################/--

#dG/dt= G - (k * G) = (1 - k) * G
# This is a first order homogeneous ODE.

#/--#########################################################################
# name: Christina Baer
# assignment: 3
# date: 10//09
# question: 4
# subquestion: b
# other files: 
##########################################################################/--
# "S" is used here as an integral sign.
# dG / dt= (1 - k) * G
# dG /G= (1 - k) * dt
# S [dG /G] = S [(1 - k) * dt]
# ln |G|= (1 - k) * t + C
# G= e^((1 - k) * t + C)

#/--#########################################################################
# name: Christina Baer
# assignment: 3
# date: 10//09
# question: 4
# subquestion: c
# other files: BiosLab3-CSB-4c.pdf
##########################################################################/--
# G = (1 - k) * G
# k = 0.01
# G = 0.99 * G

glucose<- function(x) {
return(0.99 * x)
}

modelGlu<-function(x_at_t) {
x_at_t1<-glucose(x_at_t)
return(x_at_t1)
}

init_value <-100;
max_iter <-300
x_array<-numeric(length=max_iter);
x_array[1]<-init_value;
for (ii in 2:max_iter) {
new_value<-modelGlu(x_array[ii-1]);
x_array[ii]<-new_value
}

plot4c<- plot(1:max_iter, x_array, 
main='Question 4c', xlab='Time (min)', type='l',
ylab='Glucose Concentration (mg/dL)')

# PDF copy of plot
pdf(file="BiosLab3-CSB-4c.pdf")
plot4c<- plot(1:max_iter, x_array, 
main='Question 4c', xlab='Time (min)', type='l',
ylab='Glucose Concentration (mg/dL)')
dev.off()
#/--#########################################################################
# name: Christina Baer
# assignment: 3
# date: 10//09
# question: 4
# subquestion: d
# other files: 
##########################################################################/--

# 100mg/dL is an unstable fixed point and 0mg/dL is a stable fixed point.

#/--#########################################################################
# name: Christina Baer
# assignment: 3
# date: 10//09
# question: 5
# subquestion: a
# other files: 
##########################################################################/--

# dG / dt = (1 - k) * G + a
# This is a first order inhomogenous ODE.

#/--#########################################################################
# name: Christina Baer
# assignment: 3
# date: 10//09
# question: 5
# subquestion: b
# other files: 
##########################################################################/--

# "S" is used here as an integral sign.
# dG / dt = (1 - k) * G + a
# e^(-S [a(t) * dt])= e^(-((1 - k) * t + C))
# d(G * e^(-((1 - k) * t + C)) / dt = a *  e^(-((1 - k) * t + C))
# S [d(G * e^(-((1 - k) * t + C))) / dt * dt] = 
 # S [a * e^(-((1 - k) * t + C)) * dt]
# G * e^(-((1 - k) * t + C)) = S [a * e^(-((1 - k) * t + C)) * dt] + C_(2)
# G =  e^((1 - k) * t + C) * S [a *  e^((1 - k) * t + C) * dt] + C_(2)

#/--#########################################################################
# name: Christina Baer
# assignment: 3
# date: 10//09
# question: 5
# subquestion: c
# other files: BiosLab3-CSB-5c.pdf
##########################################################################/--

glucose<- function(x) {
return(0.99 * x + 4)
}

modelGlu<-function(x_at_t) {
x_at_t1<-glucose(x_at_t)
return(x_at_t1)
}

init_value <-100;
max_iter <-400
x_array<-numeric(length=max_iter);
x_array[1]<-init_value;
for (ii in 2:max_iter) {
new_value<-modelGlu(x_array[ii-1]);
x_array[ii]<-new_value
}

plot5c<- plot(1:max_iter, x_array, 
main='Question 5c', xlab='Time (min)', type='l',
ylab='Glucose Concentration (mg/dL)')

# PDF copy of plot
pdf(file="BiosLab3-CSB-5c.pdf")
plot5c<- plot(1:max_iter, x_array, 
main='Question 5c', xlab='Time (min)', type='l',
ylab='Glucose Concentration (mg/dL)')
dev.off()
#/--#########################################################################
# name: Christina Baer
# assignment: 3
# date: 10//09
# question: 5
# subquestion: d
# other files: 
##########################################################################/--
# The blood sugar concentration in this model has a stable equilibrium at 
# 400mg/dL.
