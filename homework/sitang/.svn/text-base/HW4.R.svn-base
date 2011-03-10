#!/usr/bin/env r
# encoding: utf-8
# 
# Name: Si Tang
# Assignment: 4 (Chapter 3 Exercises)
# Question: 1(b)
# Date: Oct. 30
# File Name: HW4-1b_SiTang.R
# Task: Plot the functions f(N) and g(N) of two population models.
#############################################################################
f <- function(x)
{
    return (10 * x * (x / 20 - 1) * (1-x/200) )
}

g <- function(x)
{
    return ( 10 * x * x * (1-x/200))
}


par(mfrow=c(2,1));
curve(f,from=0, to=210, col='red', xlab='N', ylab='f(N)',fg=gray(0.6) )
abline(h=0);
curve(g,from=0, to=210, col='blue', xlab='N', ylab='g(N)',fg=gray(0.6))
abline(h=0);


#!/usr/bin/env r
# encoding: utf-8
# 
# Name: Si Tang
# Assignment: 4 (Chapter 3 Exercises)
# Question: 2(c)
# Date: Oct. 30
# File Name: HW4-2c_SiTang.R
# Task: Plot the direction field of the ODE for the bacterial growth model
#############################################################################

f <- function(N, t)
{
    return( (N - sin(2*t/pi) ) )
}



tRange <- c(0,20);
dt <- 1;
h <- 0.7;
xRange <- c(0,5);
dx <- 0.25; 
k <- 0.5; # Aspect correction.
tSteps <- diff(range(tRange))/dt;
xSteps <- diff(range(xRange))/dx;

plot(tRange, xRange, type='n', ylab='N(population)', xlab='t', fg=gray(0.8));

for(ii in 0:(tSteps-1)) {
    for(jj in 0:(xSteps-1)){
        t<-ii*dt + min(tRange);
        x<-jj*dx + min(xRange);
        m<-f(x, t);
        u<-t+(h*dt);
        v<-x+h*m*dt*k;
        arrows(t, x, u, v, code=2, col='blue', length=0.05);
    }
}

#!/usr/bin/env r
# encoding: utf-8
# 
# Name: Si Tang
# Assignment: 4 (Chapter 3 Exercises)
# Question: 3(c)
# Date: Oct. 30
# File Name: HW4-3c_SiTang.R
# Task: Plot the solution of Voltage as a function of time, starting with 
#       three different initial conditions.
#############################################################################

Nt <- function(t, N0)
{
    y= 1/(4/pi/pi + 1) * (2/pi * cos(2*t/pi) + sin (2*t/pi) ) + N0 - 2*pi/(4 + pi*pi)
    return (y)
}

checkNt <- function (N)
{
    for(i in 1: length(N)){
        if(N[i]<=0) {
            for (j in i : length(N)) {
                N[j] <- 0;
            }
            break;
        }
    }

    return (N);
}

t <- seq(0, 20, length=200);
Nt_1 <- Nt(t,1); Nt_1 <- checkNt(Nt_1);
Nt_2 <- Nt(t,2); Nt_2 <- checkNt(Nt_2);
Nt_3 <- Nt(t,3); Nt_3 <- checkNt(Nt_3);

plot(range(t), c(0, max(Nt_1,Nt_2,Nt_3)), type='n', ylab='N (Population)', xlab='t(Days)', xaxs='i', fg=gray(0.6), yaxs='i' );

lines(t, Nt_1, col=2);
lines(t, Nt_2, col=3);
lines(t, Nt_3, col=4);

legend(15,3.3, paste("N(0)=", c(1, 2, 3)), col=2:4,  pch='', ncol = 1, lwd = 2,bty='n')

#!/usr/bin/env r
# encoding: utf-8
# 
# Name: Si Tang
# Assignment: 4 (Chapter 3 Exercises)
# Question: 3(b)
# Date: Oct. 30
# File Name: HW4-3b_SiTang.R
# Task: Plot the direction field of the model for the two voltage-dependent 
#       ion channels.
#############################################################################

f <- function(V, t)
{
   return ( ((V-58.1) *(-1) - (V+69.6)* 100 )/0.15 )

}



tRange <- c(0,10);
dt <- 0.5;
h <- 0.7;
xRange <- c(-70,-65);
dx <- 0.25; 
k <- 0.001; # Aspect correction.
tSteps <- diff(range(tRange))/dt;
xSteps <- diff(range(xRange))/dx;

plot(tRange, xRange, type='n', ylab='V / mV', xlab='t', xaxs='i', fg=gray(0.6), yaxs='i' );

for(ii in 0:(tSteps-1)) {
    for(jj in 0:(xSteps-1)){
        t<-ii*dt + min(tRange);
        x<-jj*dx + min(xRange);
        m<-f(x, t);
        u<-t+(h*dt);
        v<-x+h*m*dt*k;
        arrows(t, x, u, v, code=2, col='blue', length=0.05);
    }
}


#!/usr/bin/env r
# encoding: utf-8
# 
# Name: Si Tang
# Assignment: 4 (Chapter 3 Exercises)
# Question: 3(c)
# Date: Oct. 30
# File Name: HW4-3c_SiTang.R
# Task: Plot the solution of Voltage as a function of time, starting with 
#       three different initial conditions.
#############################################################################

Vt <- function(V=0, t, V0)
{
    y= -68.336 + (V0+68.336) * exp(-673.33 * t)
    return (y)
}


t <- seq(0, 0.05, length=200);
Vt_1 <- Vt(0,t,(-100));
Vt_2 <- Vt(0,t,(-68.33));
Vt_3 <- Vt(0,t,0);

plot(range(t), range(range(Vt_1),range(Vt_2),range(Vt_3), 0), type='n', ylab='V / mV', xlab='t', xaxs='i', fg=gray(0.6), yaxs='i' );

lines(t, Vt_1, col=2);
lines(t, Vt_2, col=3);
lines(t, Vt_3, col=4);

legend(0.038, -.4, paste("V(0)=", c(-100, -68.33, 0),'mV'), col=2:4,  pch='', ncol = 1, lwd = 2,bty='n')

#!/usr/bin/env r
# encoding: utf-8
# 
# Name: Si Tang
# Assignment: 4 (Chapter 3 Exercises)
# Question: 4(b)
# Date: Oct. 30
# File Name: HW4-4b_SiTang.R
# Task: Plotting flow on the line, logistic model f(x)=x/3(1-x/5)
#############################################################################

f <- function(x) {
    return (x/3 * (1-x/5));
}

flow <- function (fx) {
    root1 <- uniroot(fx,c(0,1))$root;
    root2 <- uniroot(fx,c(1,100))$root;
    
    median <- (root1 + root2) -1 ;
    
    xmin <- root1-10
    xmax <- root2+10
    
    curve(fx, from=xmin, to=xmax, ylab='f(x)', xlab='x', xaxs='i', fg=gray(0.9), yaxs='i',col='blue',ylim=c(-10, 2) ); 

    abline(h=0,lwd=2);
    abline(v=0,lwd=2);
    
    if ( fx(xmin) <=  0 ){
        arrows(root1+(xmin-root1)/10, -0.1, xmin, -0.1, col='red',code=2,length=0.05, lwd=2)
    } else {
        arrows(xmin, 0.1, root1+(xmin-root1)/10, 0.1, col='red',code=2,length=0.05, lwd=2)
    }
    
    if ( fx(median) <= 0 ) {
        arrows(root2-(root2-root1)/10, -0.1, root1+(root2-root1)/10, -0.1, col='red',code=2,length=0.05, lwd=2)
    } else {
        arrows(root1+(root2-root1)/10, 0.1, root2-(root2-root1)/10, 0.1, col='red',code=2,length=0.05, lwd=2)
    }
    if (fx(xmax) <= 0) {
        arrows(xmax, -0.1, root2+(xmax-root2)/10, -0.1, col='red',code=2,length=0.05, lwd=2)
    } else {
        arrows(root2+(xmax-root2)/10, 0.1, xmax, 0.1, col='red',code=2,length=0.05, lwd=2)
    }

}

flow(f);


