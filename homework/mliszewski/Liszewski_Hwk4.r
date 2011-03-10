#!/usr/bin/env r
# encoding: utf-8
# 
# name: MEGAN LISZEWSKI
# assignment: 4
# date: 10/30/09
# filename: Liszewski_Hwk4.r
#############################################################################





#/--#########################################################################
# name: MEGAN LISZEWSKI
# assignment: 4
# date: 10/30/09
# question: 1
# subquestion: a
# other files: PLEASE SEE hand-written work
##########################################################################/--

#/--#########################################################################
# name: MEGAN LISZEWSKI
# assignment: 4
# date: 10/30/09
# question: 1
# subquestion: b
# other files: graph printouts and hand-written work
##########################################################################/--


N<-1:250
r<-10
K<-200
U<-20
f_N <-r*N*(N/U-1)*(1-N/K)
plot(N,f_N,main='f(N)', type='l',ylab='f(N)')

g_N<-r*(N^2)*(1-N/K)
plot(N,g_N,main='g(N)', type='l',ylab='g(N)')


#/--#########################################################################
# name: MEGAN LISZEWSKI
# assignment: 4
# date: 10/30/09
# question: 1
# subquestion: c
# other files: PLEASE SEE hand-written work
##########################################################################/--


#/--#########################################################################
# name: MEGAN LISZEWSKI
# assignment: 4
# date: 10/30/09
# question: 2
# subquestion: a&b
# other files: PLEASE SEE hand-written work
##########################################################################/--


#/--#########################################################################
# name: MEGAN LISZEWSKI
# assignment: 4
# date: 10/30/09
# question: 2
# subquestion: c&d
# other files: graph printouts and hand-written work
##########################################################################/--

f<-function(x,t) {
	return(x-sin(2*t/pi))
	}

tRange<-c(0,30)
dt<-.8
xRange<-c(-10,10)
dx<-.3
h<-0.7
k<-0.5

tSteps<-diff(range(tRange))/dt
xSteps<-diff(range(xRange))/dx

plot(c(0,20),c(-4,4),type='n',main='2c&d',ylab='x',xlab='t')

for(ii in 0:(tSteps-1)){
	for (jj in 0:(xSteps-1)){
		t<-ii*dt
		x<-jj*dx
		m<-f(x,t)
		u<-t+(h*dt)
		v<-x+(k*h*dt)*m
		arrows(t,x,u,v,code=2,length=0.05)
	}
}
for(ii in 0:(tSteps-1)){
	for (jj in 0:(xSteps-1)){
		t<-ii*dt
		x<--jj*dx
		m<-f(x,t)
		u<-t+(h*dt)
		v<-x+(k*h*dt)*m
		arrows(t,x,u,v,code=2,length=0.05)
	}


}
t1<-(0:200)/10
fixpt<-sin(2*t1/pi)
lines(t1,fixpt,col='blue')

for (kk in 1:3){
No<-c(2,0,.46)

N_t<-(pi/(pi^2+4))*(pi*sin(2*t1/pi)+2*cos(2*t1/pi))+(No[kk]-2*pi/(pi^2+4))*exp(t1)

lines(t1,N_t,col='red')}
abline(0,0)

x1<-c(14,20,20,14)
y1<-c(4,4,3,3)
polygon(x1,y1,col='white')
text(17,3.7,'____ fixed points',col='blue')
text(16.6,3.3,'____ solutions',col='red')


#/--#########################################################################
# name: MEGAN LISZEWSKI
# assignment: 4
# date: 10/30/09
# question: 3
# subquestion: a
# other files: PLEASE SEE hand-written work
##########################################################################/--


#/--#########################################################################
# name: MEGAN LISZEWSKI
# assignment: 4
# date: 10/30/09
# question: 3
# subquestion: b&c
# other files: graph printouts and hand-written work
##########################################################################/--

C<-0.15
g_Na<-1
g_K<-100
V_Na<-58.1
V_K<--69.6

f <-function(x,t) {
	return((-g_Na/C)*(x-V_Na)-(g_K/C)*(x-V_K))
	}

tRange<-c(0,2.3)
dt<-.06
xRange<-c(-110,20)
dx<-6
h<-0.05
k<-0.03

tSteps<-diff(range(tRange))/dt
xSteps<-diff(range(xRange))/dx

plot(c(0,2),c(-100,10),type='n',main='3b&c',ylab='Voltage (mV)',xlab='time')

for(ii in 0:(tSteps-1)){
	for (jj in 0:(xSteps-1)){
		t<-ii*dt
		x<-jj*dx
		m<-f(x,t)
		u<-t+(h*dt)
		v<-x+(k*h*dt)*m
		arrows(t,x,u,v,code=2,length=0.05)
	}
}
for(ii in 0:(tSteps-1)){
	for (jj in 0:(xSteps-1)){
		t<-ii*dt
		x<--jj*dx
		m<-f(x,t)
		u<-t+(h*dt)
		v<-x+(k*h*dt)*m
		arrows(t,x,u,v,code=2,length=0.05)
	}


}
a<--((g_Na+g_K)/C)
b<-(g_Na*V_Na+g_K*V_K)/C
t1<-(0:20)/10
for (kk in 1:3){
Vo<-c(0,-50,-100)

V_t<-(-b/a+(b/a+Vo[kk])*exp(a*t1))

lines(t1,V_t,col='red')}
abline(0,0)
abline(-68.34,0,col='blue')
x1<-c(1.4,2,2,1.4)
y1<-c(13,13,1,1)
polygon(x1,y1,col='white')
text(1.7,9.2,'____ fixed points',col='blue')
text(1.66,4.7,'____ solutions',col='red')

#/--#########################################################################
# name: MEGAN LISZEWSKI
# assignment: 4
# date: 10/30/09
# question: 4
# subquestion: a
# other files: PLEASE SEE hand-written work
##########################################################################/--


#/--#########################################################################
# name: MEGAN LISZEWSKI
# assignment: 4
# date: 10/30/09
# question: 4
# subquestion: b
# other files: graph printouts
##########################################################################/--

x<-(-20:60)/10
f_x<-(x/3)*(1-x/5)
plot(x,f_x,type='l',col='blue',main='#4: Flow lines',ylab='f(x)')
abline(0,0)
pos<-c()
neg<-c()
for (ii in 1:length(x)){
	if (f_x[ii]>0)
	pos<-c(pos,x[ii])
	else if (f_x[ii]<0)
	neg<-c(neg,x[ii])}
	
neg1<-neg[]<0
neg1<-neg[neg1]
arrows(neg1[length(neg1)],0,neg1[1],0,col='red')

neg2<-neg[]>0
neg2<-neg[neg2]
arrows(neg2[length(neg2)],0,neg2[1],0,col='red')

arrows(pos[1],0,pos[length(pos)],0,col='red')