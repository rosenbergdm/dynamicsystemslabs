#!/usr/bin/env r
# encoding: utf-8
# 
# Name: Si Tang
# Assignment: Assignment 5, exercise for Chapter 4
# Date: Nov. 13 2009
# Filename: HW5_SiTang.R
#############################################################################

fEuler <- function(f,para1, para2, x0, t0, t_max, step_size) {

    nstep  <- (t_max - t0) / step_size;
    xvals <- tvals <- numeric(nstep+1);
    xvals[1] <- x0;
    tvals[1] <- t0;
    
    for (i in 1 : nstep) {
        xvals[i+1] <- xvals[i] + step_size * f(xvals[i], tvals[i], para1, para2);
        tvals[i+1] <- tvals[i] + step_size;
    }

    return(data.frame(t=tvals, x=xvals));

}


#/--#########################################################################
# Name: Si Tang
# Assignment: Assignment 5, exercise for Chapter 4
# Date: Nov. 13 2009
# Question: 1
##########################################################################/--


epiModel <- function (I, t, beta, gamma) {
    y <- beta * (1-I) * I - gamma * I
    return (y)
}


#------------------------------------------------------------------------------
# Subquestion: (1c)
#------------------------------------------------------------------------------


feuler1c1 <- fEuler(epiModel, 0.3, 0.1, 0.01, 0, 365, 0.1 );
feuler1c2 <- fEuler(epiModel, 0.3, 0.1, 0.01, 0, 365, 1 );
feuler1c3 <- fEuler(epiModel, 0.3, 0.1, 0.01, 0, 365, 10 );

plot(c(0,365), c(0, max(feuler1c1$x,feuler1c2$x, feuler1c3$x)), 
        type='n', fg=gray(0.8), xlab='t / Days', 
        ylab='I / Infected Fraction', 
        main='Numerical Solution For An Epidemiology Model',
        sub='Si Tang Chapter 4 Problem 1c');

lines(feuler1c1$t, feuler1c1$x, col='blue', type='l');
lines(feuler1c2$t, feuler1c2$x, col='red', type='l');
lines(feuler1c3$t, feuler1c3$x, col='green',type='l');

abline(h=0,lwd=2);
abline(v=0,lwd=2);

legend(330, 0.1, paste('dt=', c(0.1, 1.0, 10) ), 
        col=c('blue','red','green'),lwd=2, ncol=1, bty='n')

# dev.print(width=40, height=30, file='Fig5-1c_SiTang.eps');

#------------------------------------------------------------------------------
# Subquestion: (1d)
#------------------------------------------------------------------------------


feuler1d1 <- fEuler(epiModel, 0.1, 0.2, 0.01, 0, 365, 0.1 );
feuler1d2 <- fEuler(epiModel, 0.1, 0.2, 0.01, 0, 365, 10 );
feuler1d3 <- fEuler(epiModel, 0.1, 0.2, 0.01, 0, 365, 20 );

plot(c(0,365), c(min(feuler1d1$x, feuler1d2$x, feuler1d3$x), 
            max(feuler1d1$x,feuler1d2$x, feuler1d3$x)), type='n', 
        fg=gray(0.8), xlab='t / Days', ylab='I / Infected Fraction', 
        main='Numerical Solution For An Epidemiology Model',
        sub='Si Tang Chapter 4 Problem 1d');

lines(feuler1d1$t, feuler1d1$x, col='blue', type='l');
lines(feuler1d2$t, feuler1d2$x, col='red', type='l');
lines(feuler1d3$t, feuler1d3$x, col='green',type='l');

abline(h=0,lwd=2);
abline(v=0,lwd=2);


legend(345, -0.0085, paste('dt=', c(0.1, 10, 20) ), 
        col=c('blue','red','green'),lwd=2, ncol=1,bty='n')

# dev.print(width=40, height=30, file='Fig5-1d_SiTang.eps');

#/--#########################################################################
# Name: Si Tang
# Assignment: Assignment 5, exercise for Chapter 4
# Date: Nov. 13 2009
# Question: 2
##########################################################################/--

# using the following parameters and initial conditions

k1 <- 1e-2
k2 <- 1e-5
S <- 1e-2
R0 <- 1e-2
tmax <- 1e6

receptorModel <- function (R, t, k1, k2, R0 = 1e-2, S=1e-2) {
    y <- k2 * ( R0 - R ) - k1 * S * R
    return (y)
}

receptorSol <- function (t, k1=1e-2, k2=1e-5, S=1e-2, R0 = 1e-2) {
    y <- k2 * R0 / (k1 * S + k2) + 
       R0 *  (1 - k2 / (k1 * S + k2)) * exp( - (k2 + k1 * S ) * t );
 
    return (y)
}

#------------------------------------------------------------------------------
# Subquestion: (2d)
#------------------------------------------------------------------------------


feuler2d1 <- fEuler(receptorModel, para1=k1, para2=k2, 
        x0=R0, t0=0, t_max=tmax, step_size=100 );

feuler2d2 <- fEuler(receptorModel, para1=k1, para2=k2, 
        x0=R0, t0=0, t_max=tmax, step_size=10000 );

feuler2d3 <- fEuler(receptorModel, para1=k1, para2=k2, 
        x0=R0, t0=0, t_max=tmax, step_size=15000 );

plot(c(0,tmax), c(min(feuler2d3$x), R0), type='n', fg=gray(0.8), 
        xlab='t / s', ylab='[R]', main='Numerical Solution For An Receptor Model(Forward Euler)', 
        sub='Si Tang Chapter 4 Problem 2d');

curve(receptorSol, from = 0, to =tmax, add=TRUE, col='red')

lines(feuler2d1$t, feuler2d1$x, col='blue', type='l');
lines(feuler2d2$t, feuler2d2$x, col='green', type='l');
lines(feuler2d3$t, feuler2d3$x, col='purple', type='l');

abline(h=0,lwd=2);
abline(v=0,lwd=2);



legend(3 * 10^5, R0, c('Analytical Solution',
            'Numerical Solution, dt=100', 
            'Numerical Solution, dt=10000',
            'Numerical Solution, dt=15000'), 
        col=c('red','blue','green','purple'),
        lwd=2, ncol=1,bty='n')

# dev.print(width=40, height=30, file='Fig5-2d_SiTang.eps');


#------------------------------------------------------------------------------
# Subquestion: (2e)
#------------------------------------------------------------------------------

tStep <- c(5,10,20,50,100,200,500,1000,2000, 5000);

tmax <- 1e6;

feuler2e1 <- numeric();

for (i in 1: length(tStep)) {

    result2e <- numeric();
    
    result2e <- fEuler (receptorModel, para1=k1, para2=k2, x0=R0, t0=0, 
            t_max = tmax, step_size=tStep[i] );

    feuler2e1[i] <- sum(   
                        abs(
                            result2e$x - receptorSol( result2e$t ) 
                            ) 
                       ) / length(result2e$t) 

}


plot(tStep, feuler2e1, log='xy', type='b', pch=19, col='purple', 
        main = 'log(error) vs log(dt)', xlab = 'log(dt)', 
        ylab='log(error)',
        sub='Si Tang Chapter 4 Problem 2e');

slope1 = mean (diff(log(feuler2e1)) / diff( log(tStep) ) );

text(50, 5e-6, paste('Slope m =', slope1));


# dev.print(width=40, height=30, file='Fig5-2e_SiTang.eps');

#------------------------------------------------------------------------------
# Subquestion: (2f)
#------------------------------------------------------------------------------

bEuler <- function (f, x0, t0, t_max, step_size) {

    nstep  <- (t_max - t0) / step_size;
    xvals <- tvals <- numeric(nstep+1);
    xvals[1] <- x0;
    tvals[1] <- t0;
    
    for (i in 1 : nstep) {
        xvals[i+1] <- f(xvals[i], step_size);
        tvals[i+1] <- tvals[i] + step_size;
    }

    return(data.frame(t=tvals, x=xvals));

}


backwardscheme <- function(R, dt, k1=10^-2, k2=10^-5, R0=10^-2, S=10^-2) {
    y <- (R0 * dt * k2 + R) / (1 + dt *(k1 * S + k2));
    return (y);
}


result2f1 <- bEuler(backwardscheme, R0, 0, tmax, 100);
result2f2 <- bEuler(backwardscheme, R0, 0, tmax, 1000);
result2f3 <- bEuler(backwardscheme, R0, 0, tmax, 10000);
result2f4 <- bEuler(backwardscheme, R0, 0, tmax, 15000);


plot(result2f1$t, result2f1$x, col=1, fg=gray(0.6), type='l', 
        xlab='t/s', ylab='[R]', ylim=c(0,R0),
        main='Numerical Solution For An Receptor Model (Backward Euler)',
        sub='Si Tang Chapter 4 Problem 2f');

lines(result2f2$t, result2f2$x, col=2, fg=gray(0.6), 
        type='l', xlab='t/s', ylab='[R]', ylim=c(0,R0));

lines(result2f3$t, result2f3$x, col=3, fg=gray(0.6), 
        type='l', xlab='t/s', ylab='[R]', ylim=c(0,R0));

lines(result2f4$t, result2f4$x, col=4, fg=gray(0.6), 
        type='l', xlab='t/s', ylab='[R]', ylim=c(0,R0));

abline(v=0, lwd=2);
abline(h=0, lwd=2);


legend(1.3e5, R0, paste('Numerical Solution, dt =', c(100, 1000, 10000, 15000) ), 
        col=1:4,lwd=2, ncol=1,bty='n')

# dev.print(width=40, height=30, file='Fig5-2f_SiTang.eps');

