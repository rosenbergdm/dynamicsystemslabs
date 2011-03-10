#!/usr/bin/env r
# encoding: utf-8
# 
# name: Katie Sirianni
# assignment: 4
# date: 10/29/09
# filename: Sirianni Homework 4
#############################################################################

####### HARD COPY of some questions IN ADDITION TO THIS FILE!!####

#/--#########################################################################
# name: Katie Sirianni
# assignment: 4
# date: 10/29/09
# question: 1
# subquestion: b
# other files: 1a done on paper, handed in seperately
##########################################################################/--

r<- 10
k<-200
u<- 20
t<- 200
dNDt<- numeric(length=t)
dNDt[1]<- 1
N<- 1:t
for(i in 2:t) {
	dNDt[i]<- r* N[i]*(N[i]/u -1)*(1-N[i]/k)
	}
plot(N, dNDt,  
	xlim= c(0, 250), 
	ylim= c(-1, 2500),
	type = "l", col= "blue")
abline(0, 0)
points(20, 0)
points(200, 0)

# In this scenario, the population decreases when the population
# is between 0 and 20, increases between 20 and 200, and decreases 
# when the population is greater than 200

r<- 10
k<- 200
t<- 200
dNDt<- numeric(length=t)
dNDt[1]<- 1
N<- 1:t
for(i in 2:t){
	dNDt[i]<- r*(N[i]**2)*(1-N[i]/k)
	}
	plot(N, dNDt, 
	xlim= c(0, 250), 
	ylim= c(0, 60000),
	type= "l")
# The population grows when 0<N<200, and when N> 200 the
# population decreases.

#/--#########################################################################
# name: Katie Sirianni
# assignment: 4
# date: 10/29/09
# question: 1
# subquestion: c
##########################################################################/--

#1c) Thus, the principle difference between these two equations
# is that the first decreases between 0 and 20, and the 
# second one doesn't do this.  Biologically speaking, this is 
# likely because at very low population sizes, individuals (or their gametes) might
# not encounter each other enough to mate and reproduce enough
# to replace themselves in the population, causing the population
# to decrease to extinction. 
# The other interval of growth, in both the populations, means
# that the population is growing because there are more than enough 
# resources for individuals, and so the population can continue
# to grow.  Once the carrying capacity (k) is reached, there are 
# too many individuals for the available resources, and so 
# more die than are born, such that the population declines to (k)
# AT (k), which is a stable equilibrium point in these models, 
# each individual is just replacing itself in the population.

#/--#########################################################################
# name: Katie Sirianni
# assignment: 4
# date: 10/29/09
# question: 2
# subquestion: a
##########################################################################/--

#a)  dN/dt = rN - sin(4t/2pi)

#/--#########################################################################
# name: Katie Sirianni
# assignment: 4
# date: 10/29/09
# question: 2
# subquestion: b
# other files: NAMES_OF_OTHER_RELATED_FILES
##########################################################################/--

#b) fixed point: N= 1/r*sin(4t/2pi)
# stability: d/dt(rN-sin(4t/2pi)) = r
#    , so the fixed point is unstable for any positive value of r, including the one used in the directional plot (1)

#/--#########################################################################
# name: Katie Sirianni
# assignment: 4
# date: 10/29/09
# question: 2
# subquestion: c
# other files: NAMES_OF_OTHER_RELATED_FILES
##########################################################################/--

## So for this r, the fixed point is 0.  It is unstable, however, because r is greater than 1.  This is shown in the direction field by the arrows moving away from 0 if x is not exactly 0

r<- 1
directionField<- function(x,t) {
	return(r*x- sin((4*t)/(2*pi)))
	}
	
rangeT<- c(-10,10)
dt<- 0.5
rangeX<- c(-100,100)
dx<- 5
deltaT<- 0.5
aspCorr<- 0.5
tSteps<- diff(range(rangeT)) / dt
xSteps<- diff(range(rangeX)) / dx

plot(c(0,9), c(-90,90), type= "n", 
	main= "f(N)= r*x - sin(4t/2pi)",
	xlab= "t",
	ylab= "x",
	yaxs= "i", xaxs= "i")
	
for(ii in -10: (tSteps-1)) {
	for( jj in -90: (xSteps-1)) {
		t<- ii * dt
		x<- jj * dx
		m<- directionField(x, t)
		u<- t + (deltaT*dt)
		v<- x + (aspCorr*deltaT*dt)* m
		arrows(t, x, u, v, code = 2, 
			length= .05)
	}
}


#/--#########################################################################
# name: Katie Sirianni
# assignment: 4
# date: 10/29/09
# question: 2
# subquestion: d
# other files: NAMES_OF_OTHER_RELATED_FILES
##########################################################################/--

r<- 10
end<- 50
No<- .5
N<- numeric()
t<- No:(end) 
N[1]<- No
for(i in 2:end-No+1) {
	N[i]<- (4* (pi**2)* r* sin((4*t[i])/(2*pi)) - 
	           8* pi* cos((4*t[i])/ (2*pi)))/ 
	           (4*(pi**2)*(r**2) - 16) +
	           (No - 8*pi/(4*(pi**2)*(r**2) - 16))*exp(r*t[i])
	}
	plot(t, N, 
	xlim= c(-1, 100), 
	ylim= c(-10, 300),
	type= "l", 
	main= "N(0)= 0.5")
	
r<- 10
end<- 50
No<- 2.5
N<- numeric()
t<- No:(end) 
N[1]<- No
for(i in 2:end-No+1) {
	N[i]<- (4* (pi**2)* r* sin((4*t[i])/(2*pi)) - 
	           8* pi* cos((4*t[i])/ (2*pi)))/ 
	           (4*(pi**2)*(r**2) - 16) +
	           (No - 8*pi/(4*(pi**2)*(r**2) - 16))*exp(r*t[i])
	}
	plot(t, N, 
	xlim= c(-1, 100), 
	ylim= c(-10, 1000000000000000),
	type= "l", 
	main= "N(0)= 2.5")
	
r<- 10
end<- 50
No<- -1
N<- numeric()
t<- No:(end) 
N[1]<- No
for(i in 0:end-No+1) {
	N[i]<- (4* (pi**2)* r* sin((4*t[i])/(2*pi)) - 
	           8* pi* cos((4*t[i])/ (2*pi)))/ 
	           (4*(pi**2)*(r**2) - 16) +
	           (No - 8*pi/(4*(pi**2)*(r**2) - 16))*exp(r*t[i])
	}
	plot(t, N, 
	xlim= c(-1, 10), 
	ylim= c(-100000000, 10),
	type= "l", 
	main= "N(0)= -1.0")
	
## These three graphs show that if No is greater than 0, then N increases to positive inf, and if No is less than 0, then N dec. to neg inf. This is what the directional field shows us, too, so solving the equation analytically for N wasn't really necessary.  

#/--#########################################################################
# name: Katie Sirianni
# assignment: 4
# date: 10/29/09
# question: 3
# subquestion: b
# other files: 3a done on paper, handed in seperately.
##########################################################################/--

gna<- 1
gk<- 100
vna<- 58.1
vk<- 69.6
c<- 0.15
r<- 1
directionField<- function(x,t) {
	return((1*(-gna*(x-vna)/c- gk*(x-vk)/c)))
	}
	
rangeT<- c(-10,10)
dt<- 0.5
rangeX<- c(-100,100)
dx<- 1
deltaT<- 0.00091
aspCorr<- 0.4
tSteps<- diff(range(rangeT)) / dt
xSteps<- diff(range(rangeX)) / dx

plot(c(0,5), c(65,75), type= "n", 
	main= "f(N)= r*x - sin(4t/2pi)",
	xlab= "t",
	ylab= "x",
	yaxs= "i", xaxs= "i")
	
for(ii in -10: (tSteps-1)) {
	for( jj in -100: (xSteps-1)) {
		t<- ii * dt
		x<- jj * dx
		m<- directionField(x, t)
		u<- t + (deltaT*dt)
		v<- x + (aspCorr*deltaT*dt)* m
		arrows(t, x, u, v, code = 2, 
			length= .05)
	}
}

### Yep, it agrees with the fixed point and stability analysis, there is a stable fixed point at N = -gkVk - gnVna   / (gk + gna)

#/--#########################################################################
# name: Katie Sirianni
# assignment: 4
# date: 10/29/09
# question: 3
# subquestion: c
# other files: Solution worked out on paper, handed in as hard copy
##########################################################################/--

### Init cond= 80:

Vo<- -80
gna<- 1
gk<- 100
vna<- 58.1
vk<- 69.6
c<- 0.15
end<- 100
V<- numeric(length=end+1)
t<- 0:end 

for(i in 0:end+1) {
	V[i]<- (gna*vna+ gk*vk)/(1*(gna+gk)) + (Vo- 
	(gna*vna+ gk*vk)/(1*(gna+gk)))*exp(-1* ((gna+gk)/c) * t[i])
}
plot( t, V, type= "l", main= N(0)= -80)

### init cond = 80:

Vo<- 80
gna<- 1
gk<- 100
vna<- 58.1
vk<- 69.6
c<- 0.15
end<- 100
V<- numeric(length=end+1)
t<- 0:end 

for(i in 0:end+1) {
	V[i]<- (gna*vna+ gk*vk)/(1*(gna+gk)) + (Vo- 
	(gna*vna+ gk*vk)/(1*(gna+gk)))*exp(-1* ((gna+gk)/c) * t[i])
}
plot( t, V, type= "l", main= "N(0)= 80")

### init cond = 80:

Vo<- 30
gna<- 1
gk<- 100
vna<- 58.1
vk<- 69.6
c<- 0.15
end<- 100
V<- numeric(length=end+1)
t<- 0:end 

for(i in 0:end+1) {
	V[i]<- (gna*vna+ gk*vk)/(1*(gna+gk)) + (Vo- 
	(gna*vna+ gk*vk)/(1*(gna+gk)))*exp(-1* ((gna+gk)/c) * t[i])
}
plot( t, V, type= "l", main= "N(0)= 30)"

## The membrane poptential jumps from whatever point it starts at
## to the fixed point.  The membrane potential doesn't change 
## after this, it stays at the fixed point.  If it were
## to be perturbed it would still go back to the fixed point, 
## because the fixed point is stable.  The directional field 
## tells us this, so finding the solution analytically wasn't 
## really necessary. 

#/--#########################################################################
# name: Katie Sirianni
# assignment: 4
# date: 10/29/09
# question: 4
# subquestion: a
# other files: NAMES_OF_OTHER_RELATED_FILES
##########################################################################/--

#Pseudocode for plotting flow lines:

#1: Define the function f(x)

#2: find fixed points (analytically)

#3: Between each sequential pair of fixed points:
	# define the function between only those fixed points
	# find min f(x) in that interval, add .02
	# find max f(x) in that interval, add .02
	# for intervals where f(x) > 0, make arrow from min to max
	# for intervals where f(x) < 0, make arrow from max to min

#4: for intervals with (fixed point, pos inf):
	# find min f(x) in that interval, add .02
	# for other side of arrow, use min f(x) + 2 
	# if f(x)> 0 in this interval, arrow goes from min to min+2
	# if f(x)< 0 in this interval, arrow goes from min + 2 to min
	
#5: for intervals with (neg inf, fixed point):
	# find max f(x) in that interval, subtract .02
	# for other side of arrow, use min f(x) - 2
	# if f(x)> 0 in this interval, arrow goes from max - 2 to max
	# if f(x)< 0 in this interval, arrow goes from max to max - 2

#/--#########################################################################
# name: Katie Sirianni
# assignment: 4
# date: 10/29/09
# question: 4
# subquestion: b
# other files: NAMES_OF_OTHER_RELATED_FILES
##########################################################################/--

# for this to work, need to find fixed points by hand (see pseudocode)

f<- function(x) {
	return((x/3)*(1-x/5))
	}   # enter function here
	
# fixed points at 0,5, so we need to look between 0 and 5, between 0 and neg inf, and between 5 and pos inf. 

x<- -100:200 / 20  # range x, div 20 for smoother curve

plot(x, f(x), type= "l", xlim= c(-3, 8), ylim= c(-1, 1), col = "blue", main = "My Beautiful Line Flow Analysis")
abline(0,0)

#between 0 and 5:

xo<- x[min(which(f(x)>0))] + 0.2
yo<- f(x)[min(which(f(x)>0))]
x1<- x[max(which(f(x)> 0))] - 0.2
y1<- f(x)[max(which(f(x)>0))]


arrows(xo, yo, 
	   x1, y1, 
	   col= "red", 
	   lwd= 3, length= .05)

#between neg inf and 0:

x<- -100:0 / 20

xo<-x[max(which(f(x)<0))] - 0.2
yo<-f(x)[max(which(f(x)<0))]
x1<-x[max(which(f(x)<0))] - 2
y1<-f(x)[max(which(f(x)<0))]

arrows(xo, yo, 
	   x1, y1, 
	   col= "red", 
	   lwd= 3, length= .05)
	   
#between 5 and pos inf:

x<- 100:200 / 20

x1<- x[min(which(f(x)<0))] + 0.2
y1<- f(x)[min(which(f(x)<0))]
xo<- x[min(which(f(x)<0))] + 2
yo<- f(x)[min(which(f(x)<0))]

arrows(xo, yo, 
	   x1, y1, 
	   col= "red", 
	   lwd= 3, length= .05)