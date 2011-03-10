##################################################
##Name: Melissa Runfeldt
##Assignment: Ch4 lab exercise
##Problem 1a
##################################################

#dI/dt=I(b(N-I)-g)

#Fixed points (FPs) occur when: I(b(N-I)-g)=0

# I = 0

# b(N-I)-g=0 -> I = N - g/b


#What does this mean?#

# When I=0 there are no infected individuals
# and there is no change in population.

# At I = N-g/b, the population is stable. This occurs when the
# number of infected individuals equals the total population
# minus the recovery rate divided by the infection rate. That is,
# g/b = S, so I=N-S. 


##################################################
##Name: Melissa Runfeldt
##Assignment: Ch4 lab exercise
##Problem 1b
##################################################

#FP stability#

#f(I) = IbN -I^2b - gI

# f'(I) = bN - Ib - g

# f'(0) = bN - g : Stable when bN - g < 0 OR bN < g

# f'(N - g/b) = -2g : Stable when -2g < 0 OR g > 0


#What does this mean?#

# When the infection rate times the total population is less than
# the recovery rate, the population moves towards having no (zero)
# infected individuals (FP I=0).

# When the recovery rate is greater than zero, the population moves
# towards an equilibrium wherein the number of infected and susceptible 
# indivduals are in balance (FP= N - g/b). 


##################################################
##Name: Melissa Runfeldt
##Assignment: Ch4 lab exercise
##Problem 1c
##################################################

## Time step stability ###

# -1 < 1+b*dt*(N-dt)-g*dt < 1

# -2 < dt*(b*(N-1)-g <0

# -2 / (b*(N-1)-g) > dt > 0 

N<-1;
b<-0.3;
g<-0.1;


# 20 < dt
# time step should be less than 20

################################

# bN = 0.6/day
# g = 0.1/day

# bN > g : thus, FP I=0 is unstable
# g > 0 : thus, FP I= N - g/b is stable

# N - g/b = 2/3 with given parameters

###Forward Euler###

N<-1;
b<-0.3;
g<-0.1;

SIS<-function (I,t){
  return (b*I*(N-I)-g*I);
}

forwardEuler <-function(SIS, max_iter, step, I0, t0) {
Ivals<-tvals<-numeric(length=(max_iter+1));
Ivals[1]<-I0;
tvals[1]<-t0;

for(ii in 1:max_iter){
Ivals[ii+1]<-Ivals[ii]+step*SIS(Ivals[ii],tvals[ii]);
tvals[ii+1]<-tvals[ii]+step
}
return(data.frame(t=tvals,I=Ivals));
}

step_size<-12
nSteps<-365/step_size;
fEuler<-forwardEuler(SIS,nSteps,12,0.01,0);
fEuler$I[length(fEuler$I)];
  


#step_size= 0.1; at t=365, I= 0.6666667

#step_size= 0.5; at t=365, I= 0.6666667

#step_size= 1; at t=365, I= 0.6666667

#step_size= 8; at t=365, I= 0.6666667

#step_size= 9; at t=365, I= 0.6666481 

#step_size= 10; at t=365, I= 0.6273892

#step_size= 11; at t=365, I= 0.7949955
 

# Theoretical soln: pop approaches N - g/b = 2/3.
# Smaller time-steps have less error.
# Time steps less than 8 give correct numerical approx
# within 7 significant digits.

  
##################################################
##Name: Melissa Runfeldt
##Assignment: Ch4 lab exercise
##Problem 1d
##################################################

## Time step stability ###

# -1 < 1+b*dt*(N-dt)-g*dt < 1

# -2 < dt*(b*(N-1)-g <0

# -2 / (b*(N-1)-g) > dt > 0 

N<-1;
b<-0.1;
g<-0.2;


# 10 < dt
# time step should be less than 10

###Forward Euler###

N<-1;
b<-0.1;
g<-0.2;

SIS<-function (I,t){
  return (b*I*(N-I)-g*I);
}

forwardEuler <-function(SIS, max_iter, step, I0, t0) {
Ivals<-tvals<-numeric(length=(max_iter+1));
Ivals[1]<-I0;
tvals[1]<-t0;

for(ii in 1:max_iter){
Ivals[ii+1]<-Ivals[ii]+step*SIS(Ivals[ii],tvals[ii]);
tvals[ii+1]<-tvals[ii]+step
}
return(data.frame(t=tvals,I=Ivals));
}

step_size<-1
nSteps<-365/step_size;
fEuler<-forwardEuler(SIS,nSteps,1,0.01,0);
fEuler$I[length(fEuler$I)];


#step_size= 0.01; at t=365, I= 1.367712e-18

#step_size= 0.1; at t=365, I= 1.159032e-18

#step_size= 1; at t=365, I= 1.966592e-19

#step_size= 5; at t=365, I= 1.304351e-112

#step_size= 10; at t=365, I= 0 

#step_size= 20; at t=365, I= 0.00992798


# Appreciable error is accumilated at time steps larger than 10;
# this is when instability emerges.


##################################################
##Name: Melissa Runfeldt
##Assignment: Ch4 lab exercise
##Problem 2a
##################################################

# I will call forward rate constant "k1"
  and backward rate constant "k2."

# S + R [ k1 -> , <- K2 ]  C

# S(0) = S : constant

# C = R0 - R

# dR/dt = -k1*S*R + k2*(R0-R) = -k1*S*R + k2*Ro - R*k2

# dR/dt = -R*(k1*S+k2)+(k2*R0)

##################################################
##Name: Melissa Runfeldt
##Assignment: Ch4 lab exercise
##Problem 2b
##################################################

## Finding FPs##

# dR/dt = -R*(k1*S+k2)+(k2*R0) = 0

# R*(k1*S+k2)=(k2*R0)

# FP:  R = (k2*R0) / (k1*S + k2)


## FP Stability ##

# f(R) = -R*(k1*S+k2)+(k2*R0) = -k1*S*R - R*k2 + k2*R0

# f'(R) = -k1*S - k2

# Stable when -k1*S - k2 < 0

##################################################
##Name: Melissa Runfeldt
##Assignment: Ch4 lab exercise
##Problem 2c
##################################################

# dR/dt  = R' = -R*(k1*S+k2)+(k2*R0)

# R' = k2*(R0-R) - k1*S*R 
     = k2*R0 - k2*R - k1*S*R

# Where S is constant

# Using Laplace transform methods...#

# s*L{R} - R(0) = 1/s * k2*R0 - (k2+k1*S)* L{R}
  
# L{R} = (k2*R0)/(k2+k1*S) * [1/s - 1/(s+(k2+k1*S)*R0]
  
  #Take the inverse Laplace


## R (t) = k2*R0 / k2+k1*S - R0*[1 - e^-t*(k2+k1*S] ##


# As t --> infinity, R(t) --> (k2*R0) / (k1*S + k2),
#  which is the fixed point.


Ranalytic<-function(t){
  return (k2*R0 / k2+k1*S - R0*(1 - exp(-t*(k2+k1*S))));
}

Ranalytic(1000)
[1] 9.900588e-05


(k2*R0) / (k1*S + k2)
[1] 9.999e-05

##################################################
##Name: Melissa Runfeldt
##Assignment: Ch4 lab exercise
##Problem 2d
##################################################

S<-10^-6;
k1<-10^-3;
k2<-10^-5;
R0<-10^-2;

# dR/dt = k2*C - k1*S*R

# To keep error from growing:
#  -1 < 1 + k2*C - k1*S*dt < 1 
#  -2 < k2*C - k1*S*dt < 0
#  -2-k2*C < - k1*S*dt < -k2*C

## Step size: 2+k2*C / k1*S > dt > k2*C / k1*S

## 2 > dt > 0  #Set time step between 2 and 0


## FP = (k2*R0) / (k1*S + k2). So R will decay to 0.009999.


fReceptor<-function(R,t){
  return (-R*(k1*S+k2)+(k2*R0));
}
forwardEuler<-function(fReceptor,max_iter,step,R0,t0){
  Rvals<-tvals<-numeric(length=(max_iter+1));
  Rvals[1]<-R0;
  tvals[1]<-t0;
  
  for (ii in 1:max_iter){
  Rvals[ii+1]<-Rvals[ii]+step*fReceptor(Rvals[ii],tvals[ii]);
  tvals[ii+1]<-tvals[ii]+step
}
return(data.frame(t=tvals, R=Rvals));
}

step_size<-0.01;
nSteps<-1000;
feuler1<- forwardEuler(fReceptor,nSteps,35, 10^-2, 0);
feuler1$R[length(feuler1$R)];


# Solution, using Forward Euler and dt of 0.01, = 0.00999901 #

##################################################
##Name: Melissa Runfeldt
##Assignment: Ch4 lab exercise
##Problem 2e
##################################################

S<-10^-6;
k1<-10^-3;
k2<-10^-5;
R0<-10^-2;

> (k2*R0) / (k1*S + k2)
[1] 0.009999

### Using Forward Euler ###


dt1<-0.1;
FE1<-0.01;

dt2<-1;
FE2<-0.009990104;

dt3<-1;
FE3<-0.00999999;

dt4<-35;
FE4<-0.009999705;

dt5<-100;
FE5<-0.009999368;

dt6<-388;
FE6<-0.00999902;

# Make vectors to plot #

dt<-c(dt1,dt2,dt3,dt4,dt5);

FE<-c(abs(0.009999-FE1),abs(0.009999-FE2),
  abs(0.009999-FE3),abs(0.009999-FE4),abs(0.009999-FE5));

plot(log(dt), log(FE),
main='Forward Euler Error vs Time Step',
sub='melissa runfeldt. Ch4.2e',
xlab='Log (Time Step (sec))',
ylab='Log (Total Error)')



## slope ##
 

# This is a second order method.

##################################################
##Name: Melissa Runfeldt
##Assignment: Ch4 lab exercise
##Problem 2f
##################################################

# Using implicit method, we get R(t+1)=(R0*k2)/(1+dt*(k1*S+k2)

S<-10^-6;
k1<-10^-3;
k2<-10^-5;


f<-function(R,t){
  return (-R*(k1*S+k2)+(k2*R0));
}
backwardEuler<-function(f,max_iter,step,R0,t0){
  Rvals<-tvals<-numeric(length=(max_iter+1));
  Rvals[1]<-R0;
  tvals[1]<-t0;
  
  for (ii in 1:max_iter){
  tvals[ii+1]<-tvals[ii]+step
  Rvals[ii+1]<-Rvals[ii]+step*f(Rvals[ii]*step*(R0*k2)/(1+step*(k1*S+k2)), 
  tvals[ii]*step*(R0*k2)/(1+step*(k1*S+k2)));
}
return(data.frame(t=tvals, R=Rvals));
}

step_size<-100;
nSteps<-100/step_size;
beuler1<-backwardEuler(f,nSteps,100, 10^-2, 0);
beuler1$R[length(beuler1$R)];


## Solution, at dt=100, using backward euler = 0.01001



