##################################################
##Name: Melissa Runfeldt
##Assignment: Ch2 lab exercise
##Problem 1a:  N(t+1)=41Nt-10Nt^2
##################################################

#Nt+1=41Nt-10Nt^2=Nt(41-10Nt)

#r=41  <  3  : No stable fixed point. No carrying capacity.

###Analytically finding fixed points###
# x = x(41-10x)
# x = 0 , 4


curve(x*41-10*x^2,from=-5, to=5, main="Nt+1=41Nt-10Nt^2", sub="melissa runfeldt.Ch2.#1a")

##################################################
##Name: Melissa Runfeldt
##Assignment: Ch2 lab exercise
##Problem 1b:  N(t+1)=41Nt+2Nt^2
##################################################

#Nt+1=41Nt-2Nt^2=Nt(41-2Nt)

#r=41  <  3  : No stable fixed points. No carrying capacity.

###Analytically finding fixed points###
# x = x(41-2x)
# x = 0 , -20

curve(x*41-2*x^2,from=-10, to=50, main="Nt+1=41Nt-2Nt^2", sub="melissa runfeldt.Ch2.#1b")

##################################################
##Name: Melissa Runfeldt
##Assignment: Ch2 lab exercise
##Problem 2a:  F(x)=x(5-4x)
##################################################

cobProb2a<-function(x){
return(x * (5 - 4 * x))
}
max_iter<-50;
y_vector<-x_vector<-numeric(
max_iter*2);
y_vector[1]<--0.5;
x_vector[1]<-0.1;
for(ii in 1:50){
  y_vector[2*ii]<-cobProb2a(
  x_vector[2*ii-1]);
  y_vector[2*ii+1]<-
  y_vector[2*ii];
  x_vector[2*ii]<-
  x_vector[2*ii-1];
  x_vector[2*ii+1]<-
  y_vector[2*ii+1];
}

plot(-10:50/10,
cobProb2a(-10:50/10),
xlab='x',
ylab='y',
main='F(x)=x(5-4x)',
sub="melissa runfeldt.Ch2.#2a",
type='l',
xlim=c(-1,2),
ylim=c(-1,2),
xaxs='i',
yaxs='i');

lines(-2:6, -2:6);
lines(x_vector,y_vector)

##################################################
##Name: Melissa Runfeldt
##Assignment: Ch2 lab exercise
##Problem 2b:F(x)=2x(1-3x/2)
##################################################

cobProb2b<-function(x){
return(2 * x * (1 - 3 * x / 2))
}
max_iter<-50;
y_vector<-x_vector<-numeric(
max_iter*2);
y_vector[1]<-0;
x_vector[1]<-0.5;
for(ii in 1:50){
  y_vector[2*ii]<-cobProb2b(
  x_vector[2*ii-1]);
  y_vector[2*ii+1]<-
  y_vector[2*ii];
  x_vector[2*ii]<-
  x_vector[2*ii-1];
  x_vector[2*ii+1]<-
  y_vector[2*ii+1];
}


plot(-10:50/10,
cobProb2b(-10:50/10),
xlab='x',
ylab='y',
main='F(x)=2x(1-3x/2)',
sub="melissa runfeldt.Ch2.#2b",
type='l',
xlim=c(-2,2),
ylim=c(-2,2),
xaxs='i',
yaxs='i');

lines(-2:6, -2:6);
lines(x_vector,y_vector)

##################################################
##Name: Melissa Runfeldt
##Assignment: Ch2 lab exercise
##Problem 2c:F(x)=F(x)=x/2-(x^2)/5
##################################################

cobProb2c<-function(x){
return(x / 2 - x^2 / 5)
}
max_iter<-50;
y_vector<-x_vector<-numeric(
max_iter*2);
y_vector[1]<-0;
x_vector[1]<-0.5;
for(ii in 1:50){
  y_vector[2*ii]<-cobProb2c(
  x_vector[2*ii-1]);
  y_vector[2*ii+1]<-
  y_vector[2*ii];
  x_vector[2*ii]<-
  x_vector[2*ii-1];
  x_vector[2*ii+1]<-
  y_vector[2*ii+1];
}


plot(-10:50/10,
cobProb2c(-10:50/10),
xlab='x',
ylab='y',
main='F(x)=x/2-(x^2)/5',
sub="melissa runfeldt.Ch2.#2c",
type='l',
xlim=c(0,2),
ylim=c(0,2),
xaxs='i',
yaxs='i');

lines(-2:6, -2:6);
lines(x_vector,y_vector)


##################################################
##Name: Melissa Runfeldt
##Assignment: Ch2 lab exercise
##Problem 2d:F(x)=x(5/2-7x)
##################################################

cobProb2d<-function(x){
return(x * (5/2 - 7 * x))
}
max_iter<-50;
y_vector<-x_vector<-numeric(
max_iter*2);
y_vector[1]<-0;
x_vector[1]<-0.1;
for(ii in 1:50){
  y_vector[2*ii]<-cobProb2d(x_vector[2*ii-1]);
  y_vector[2*ii+1]<-y_vector[2*ii];
  x_vector[2*ii]<-x_vector[2*ii-1];
  x_vector[2*ii+1]<-y_vector[2*ii+1];
}

plot(-10:50/10,
cobProb2d(-10:50/10),
xlab='x',
ylab='y',
main='F(x)=x(5/2-7x)',
sub="melissa runfeldt.Ch2.#2d",
type='l',
xlim=c(0,1),
ylim=c(0, 0.5),
xaxs='i',
yaxs='i');

lines(-2:6, -2:6);
lines(x_vector,y_vector)

##################################################
##Name: Melissa Runfeldt
##Assignment: Ch2 lab exercise
##Problem 3
##################################################

fun<-function(x){
  return(x * (5/2 - 7 * x))
}

cobPlot<-function(fun, max_iter=50,init_val=0.1){

x_vector<-y_vector<-numeric(length=max_iter*2)

x_vector[1]<-0;
y_vector[1]<-init_val;

for(ii in 1:max_iter){
  y_vector[2*ii]<-fun(x_vector[2*ii-1]);
  y_vector[2*ii+1]<-y_vector[2*ii];
  x_vector[2*ii]<-x_vector[2*ii-1];
  x_vector[2*ii+1]<-y_vector[2*ii+1];
}
curve(fun,0,max_iter,main="F(x)=x(5/2-7x)",
sub="melissa runfeldt.Ch2.#3", bty='n');
abline(h=0);
abline(v=0);
lines(x_vector,y_vector, col='red');
lines(c(-1,51),c(-1,51));
}


##################################################
##Name: Melissa Runfeldt
##Assignment: Ch2 lab exercise
##Problem 4a
##################################################

#1st order, autonomous, homogenous ODE

#dG/dt = -k(t)G

##################################################
##Name: Melissa Runfeldt
##Assignment: Ch2 lab exercise
##Problem 4b
##################################################

#G(t)=G(0)*e^(-kt)

##################################################
##Name: Melissa Runfeldt
##Assignment: Ch2 lab exercise
##Problem 4c
##################################################

curve(100*exp(-0.01*x),from=0, to=300,
main='Concentration of Glucose in bloodstream:G(t)=100e^(-0.01t)', sub="melissa runfeldt.Ch2.#4c",
xlab='t(min)',ylab='G(t) (mg/dl)');

##################################################
##Name: Melissa Runfeldt
##Assignment: Ch2 lab exercise
##Problem 4d
##################################################

#fixed point = 0
#G'(t*)=-0.01 < 0
#Stable fixed point

##################################################
##Name: Melissa Runfeldt
##Assignment: Ch2 lab exercise
##Problem 5a
##################################################

#dG/dt = -k(t)G + a(t)
#1st order, autonomous, inhomogenous ODE

#dG/dt = -0.01G + 4

##################################################
##Name: Melissa Runfeldt
##Assignment: Ch2 lab exercise
##Problem 5b
##################################################

# "\int"   =  Integral

#G(t) = exp^(\int(k(t)dt)) * \int(a(t)*exp^(-1*\int(k(t)dt)) dt + C*exp^(\int(k(t)dt))


##################################################
##Name: Melissa Runfeldt
##Assignment: Ch2 lab exercise
##Problem 5c
##################################################

#k(t)=0.01
#G0=100
#a=4

#G(t)= e^(-t/100)*4*100*e^(t/100)+C*e^(-t/100)
#G(t)=400+C*e^(-t/100)

#G(t)=400-(go-400)*exp(-t/100)

#G(0)=400+C*e^0=400+C
#C=G0-400
#if G0=100, C=300


#G(t)=400-300*exp(-t/100)

curve(400-300*exp(-x/100), from=-30, to=700, main='Glucose in bloodstream with infusion',sub="melissa runfeldt.Ch2.#5c",xlab='time(min)',ylab='G(t)');

##################################################
##Name: Melissa Runfeldt
##Assignment: Ch2 lab exercise
##Problem 5d
##################################################

##dG/dt = -0.01G + 4
##fixed point (equilibrium concentration) = 400mg/dl

##f'(G)=-0.01    < 0  :: Stable fixed point


curve(400-300*exp(-x/100), from=-30, to=700, main='Glucose in bloodstream with infusion',sub="melissa runfeldt.Ch2.#5d",xlab='time(min)',ylab='G(t)');
