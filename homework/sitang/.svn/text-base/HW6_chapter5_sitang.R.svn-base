#!/usr/bin/env r
# encoding: utf-8
# 
# Name: Si Tang
# Assignment: Assignment 6, exercises for Chapter 5
# Date: Nov. 28 2009
# Filename: HW6_SiTang.R
#############################################################################


#/--#########################################################################
# Name: Si Tang
# Assignment: Assignment 6, exercise for Chapter 5
# Date: Nov. 28 2009
# Question: 1
##########################################################################/--

plotCobweb <- function(f, max_iter) {

    iVal <-1;
    iiVal <- iVal / 2;

    while (f(iVal) <= 0 | f(iiVal)==iiVal) {
        iVal <- iVal / 10;
        iiVal <- iVal/2;
    }

    xRange <- numeric (length=2);
    xRange[1] <- 0;

    xMax <- iiVal;
    while(f(xMax) >=0) {
        xMax <- 1.1 * xMax;
    }

    xRange[2] <- xMax;

    yArr <- numeric(length = max_iter * 2);
    xArr <- yArr;

    xArr[1] <- 0;
    yArr[1] <- iiVal;

    for (ii in 1: max_iter) {
        yArr[2 * ii] <- f(yArr[2 * ii - 1]);
        yArr[2 * ii+1] <- yArr[2 * ii];
        xArr[2 * ii] <- xArr[2 * ii -1];
        xArr[2 * ii + 1] <- yArr[2 * ii];
    }

    curve(f, xlim = xRange, 
            bty='n', 
            fg=grey(0.6), 
            xlab=expression(X[t]),
            ylab=expression(X[t+1]));

    abline(h=0);
    abline(v=0);

    lines(c(xRange[1] - 100, xRange[2] + 100), 
            c(xRange[1]-100, xRange[2]+100), 
            col='red');
    lines(xArr, yArr, col='green');

    return(xRange);
}

plotNumSol <- function(f, x0, r=2, nDiscard=0, max_iter=500) {

    xArr <- numeric(length=max_iter + 1);
    xArr[1] <- x0 ;
    for(i in 1:max_iter) {
        xArr[i+1] = f(xArr[i], r);
    }

    plot(0,0, type='n', bty='n', fg=grey(0.6),
            xlim=range((nDiscard+1):max_iter+1), 
            ylim=range(xArr), 
            xlab=expression(X[t]), 
            ylab=expression(X[t+1]));

    lines((nDiscard+1):(max_iter+1), xArr[(nDiscard+1):(max_iter+1)], 
            col='blue',
            type='b', pch=1);
}

NumSol <- function(f, x0, r=2, max_iter) {

    xArr <- numeric(length=max_iter + 1);
    xArr[1] <- x0 ;
    for(i in 1:max_iter) {
        xArr[i+1] = f(xArr[i], r);
    }
    return (xArr);
}

#------------------------------------------------------------------------------
# Subquestion: 1(a)
#------------------------------------------------------------------------------

logisticModel <- function (x,r=2) {
    return (r*x*(1-x));
}

par(mfrow=c(1,2));
plotCobweb(logisticModel, 50);
title(main='Cobweb Plot', sub='Si Tang-Problem 1(a)-1');
plotNumSol(logisticModel, x0=0.01, r=2, nDiscard=0, max_iter=50);
title(main='Numerical Solution', sub='Si Tang-Problem 1(a)-2');

#------------------------------------------------------------------------------
# Subquestion: 1(b)
#------------------------------------------------------------------------------

solver <- function(f, x0, r_min, r_max, dr, max_iter) {
    r <- seq(r_min, r_max, by=dr);
    x <- array(NA, c(length(r), max_iter+1 ) );
    for ( i in 1:length(r) ) {
        x[i,] <- NumSol(f, x0, r[i], max_iter);
    }
    

    return (list(r=r, x=x));
}



#------------------------------------------------------------------------------
# Subquestion: 1(c)
#------------------------------------------------------------------------------
max_iter <- 500;
rmin <- 0;
rmax <- 4;
dr <- 0.01;
x0 <- 0.01;
nDiscard <- 200;

result <- solver(logisticModel,x0, rmin, rmax, dr, max_iter);

r <- result$r;
x <- result$x;
plot(0, 0, type='n',bty='n', fg=grey(0.6), 
    xlab='r', ylab=expression(X[t]),
    xlim=range(r), ylim=range(x));

for (i in 1:length(r)) {
    points( rep(r[i], max_iter-nDiscard+1), x[i,(nDiscard+1):(max_iter+1)], 
            col='blue',pch='.');
}
 

#------------------------------------------------------------------------------
# Subquestion: 1(d)
#------------------------------------------------------------------------------
par(mfrow=c(4,1));

r <- c(2, 3.3, 3.5, 4);

for(i in 1:length(r)){
    plotNumSol(logisticModel, x0=0.01, r=r[i], nDiscard=200, max_iter=500);
    title(main=paste('Numerical Solution: r=', r[i]) , 
            sub=paste('Si Tang-Problem 1(d) -', i) );
}

#/--#########################################################################
# Name: Si Tang
# Assignment: Assignment 6, exercise for Chapter 5
# Date: Nov. 28 2009
# Question: 2
##########################################################################/--

#------------------------------------------------------------------------------
# Subquestion: 2(b)
#------------------------------------------------------------------------------
leslieModel <- function(x, y) {
    x1 <- 0.5*x + 2* y;
    y1 <- 0.1*x;
    return (c(x1, y1));
}

solveLeslie <- function (f, x0, y0, t_max) {
    pop <- array(NA, c((t_max+1),2));
    pop[1, 1] <- x0;
    pop[1, 2] <- y0;
    for (i in 2:(t_max+1)) {
        pop[i, ] <- leslieModel(pop[i-1, 1], pop[i-1, 2] );
    }

    return(pop);
}

tmax2b <- 50;

pop <- solveLeslie(leslieModel, 10, 10, tmax2b);


plot( 1:(tmax2b+1), pop[1:(tmax2b+1),1] + pop[1:(tmax2b+1),2], 
        bty='n',type='b',fg=grey(0.6), col=4, pch=19,
         xlab='t', ylab='Population', main='Population vector Plot', 
        sub='Si Tang-Problem 2(b)-1');

lines( 1:(tmax2b+1), pop[1:(tmax2b+1),1], pch=19,
        bty='n',type='b',fg=grey(0.6), col=2);

lines( 1:(tmax2b+1), pop[1:(tmax2b+1),2], pch=19,
        bty='n',type='b',fg=grey(0.6), col=3);

legend(30, 8, paste(c(expression(x[t]), expression(y[t]), expression(N[t])), '~t'), 
        col=c(2:4), lwd=1, bty='n');
abline(h=0);
abline(v=0);



#------------------------------------------------------------------------------
# Subquestion: 2(c)-Fig. 5 
#------------------------------------------------------------------------------

tmax2c <- 50;
x0 <- c(1,10, 10);
y0 <- c(5,10, 1);

pop <- array(NA, c(length(x0), (tmax2c+1), 2) );

for (i in 1: length(x0)) {
    pop[i, 1:(tmax2c+1), 1:2] <- solveLeslie(leslieModel, x0[i], y0[i], tmax2c);
}

plot(0, 0, type='n', bty='n', fg=grey(0.6),
        xlim=c(0, tmax2c), ylim=c(0, 10),
        xlab='t', ylab='Population', 
        main='Population vector Plot', 
        sub = 'Si Tang-Problem 2(c)-1');

color=2;

for (i in 1:length(x0)) {
    lines( 1:(tmax2c+1), 
            pop[i, 1:(tmax2c+1),1],lty=1,cex=0.7, 
            type='p', fg=grey(0.6), col=color,pch=16);
    lines( 1:(tmax2c+1), 
            pop[i, 1:(tmax2c+1),1],lty=1,cex=0.7, 
            type='l', fg=grey(0.6), col=color,pch=16);
    color <- color+1;

}

legend(30, 8, paste('X~t  , ','x0=', x0, ', y0=', y0), col=2:4, 
        ncol=1,lwd=1, bty='n', pch=16);
abline(h=0);
abline(v=0);

for (i in 1:length(y0)) {

    lines( 1:(tmax2c+1), 
            pop[i, 1:(tmax2c+1),2], lty=1, cex=0.7,
            type='p', fg=grey(0.6), col=color, pch=17);
    lines( 1:(tmax2c+1), 
            pop[i, 1:(tmax2c+1),2], lty=1, cex=0.7,
            type='l', fg=grey(0.6), col=color, pch=17);
    color <- color+1;

}

legend(30, 4, paste('Y~t  , ', 'x0=', x0, ', y0=', y0), col=5:7, 
        ncol=1,lwd=1, bty='n', pch=17);

#------------------------------------------------------------------------------
# Subquestion: 2(c)-Fig. 6.
#------------------------------------------------------------------------------

tmax2c <- 50;
x0 <- c(1,10, 10);
y0 <- c(5,10, 1);

pop <- array(NA, c(length(x0), (tmax2c+1), 2) );

for (i in 1: length(x0)) {
    pop[i, 1:(tmax2c+1), 1:2] <- solveLeslie(leslieModel, x0[i], y0[i], tmax2c);
}


plot(0, 0, type='n', bty='n', fg=grey(0.6),
        xlim=c(0, tmax2c), ylim=c(0, 2),
        xlab='t', ylab='Population', 
        main='', 
        sub = 'Si Tang-Problem 2(c)-2');

color=2;


for (i in 1:length(x0)) {
    lines( 1:(tmax2c), 
            pop[i, 2:(tmax2c+1),1]/pop[i, 1:(tmax2c),1],
            lty=1,cex=0.7, 
            type='p', fg=grey(0.6), col=color,pch=16);
    lines( 1:(tmax2c), 
            pop[i, 2:(tmax2c+1),1]/pop[i, 1:(tmax2c),1],
            lty=1,cex=0.7, 
            type='l', fg=grey(0.6), col=color,pch=16);
    color <- color+1;

}

legend(20, 2, paste('X[t+1]/X[t] ~ t, ','x0=', x0, ', y0=', y0), col=2:4, 
        ncol=1,lwd=1, bty='n', pch=16);

abline(h=0);
abline(v=0);

for (i in 1:length(y0)) {
    lines( 1:(tmax2c), 
            pop[i, 2:(tmax2c+1),2]/pop[i, 1:(tmax2c),2],
            lty=1,cex=0.7, 
            type='p', fg=grey(0.6), col=color,pch=17);
    lines( 1:(tmax2c), 
            pop[i, 2:(tmax2c+1),2]/pop[i, 1:(tmax2c),2],
            lty=1,cex=0.7, 
            type='l', fg=grey(0.6), col=color,pch=17);
    color <- color+1;

}

legend(20, 1.5, paste('Y[t+1]/Y[t] ~ t, ','x0=', x0, ', y0=', y0), col=5:7, 
        ncol=1,lwd=1, bty='n', pch=17);



#/--#########################################################################
# Name: Si Tang
# Assignment: Assignment 6, exercise for Chapter 5
# Date: Nov. 28 2009
# Question: 3
##########################################################################/--

#------------------------------------------------------------------------------
# Subquestion: 3(b)
#------------------------------------------------------------------------------

UsherModel <- function(x, y, a) {
    x1 <- 0.5*x + 2* y;
    y1 <- 0.1*x + a * y;
    return (c(x1, y1));
}

solveUsher <- function (f, x0, y0, a_min, a_max, da, t_max) {
    a <- seq(a_min, a_max, by=da);
    pop <- array(NA, c(length(a),(t_max+1),2));
    for(k in 1: length(a)) {

        pop[k, 1, 1] <- x0;
        pop[k, 1, 2] <- y0;
        for (i in 2:(t_max+1)) {
            pop[k, i, ] <- f(pop[k, i-1, 1], pop[k, i-1, 2],a[k] );
        }
    }
    return(pop);
}

# Initialization
tmax <- 500;
amin <- 0;
amax <- 0.8;
da <- 0.01;
x0 <- 10;
y0 <- 8;
#

a = seq(amin, amax, by=da);
total <- numeric(length=length(a));
total2 <- numeric(length=length(a));

result <- solveUsher(UsherModel, x0, y0, amin, amax, da, tmax);
for (i in 1: length(a)) {
    total[i] <- result[i, tmax+1, 1] + result[i, tmax+1, 2];
    total2[i] <- result[i, tmax, 1] + result[i, tmax, 2];
}


par(mfrow=c(1,3));

# Plot the population behavior, a (0, 0.8). 
plot(a, total, type='l', xlim=c(0,1), bty='n', xlab='a',
        ylab=expression(x[500] + y[500]), fg=grey(0.6),
        col='blue', main='Convergence Behavior of the Usher Population',
        sub='Si Tang-Problem 3(b)-1'); 
abline(v=0);
abline(h=0);

# Plot a Scaled figure of the population behavior, the range of a is [0, 0.6]
a2 <- a[a<=0.6];
plot(a2, total[1: length(a2)], type='l', xlim=c(0,1),bty='n', xlab='a',
        ylab=expression(x[500] + y[500]), fg=grey(0.6),
        col='blue', main='Convergence Behavior of the Usher Population\n (Scaled Figure)',
        sub='Si Tang-Problem 3(b)-2'); 
abline(v=0);
abline(h=0);

# Plot the change of the eigenvalue versus a, ranging from 0 to 0.8. 
plot(a, total/total2, type='l', xlim=c(0,1), bty='n', xlab='a',
        ylab=expression(lambda), fg=grey(0.6),
        col='blue', main='Eigenvalue versus a',
        sub='Si Tang-Problem 3(b)-3'); 
abline(v=0);
abline(h=0);


#------------------------------------------------------------------------------
# Subquestion: 3(c)
#------------------------------------------------------------------------------
par(mfrow=c(3,1));
aVal <- c(0.55, 0.6, 0.62);
for (ii in 1: length(aVal)) {
    plot(0:tmax,
        result[which(a==aVal[ii]), 1:(tmax+1), 1 ] + result[which(a==aVal[ii]), 1:(tmax+1), 2],
        xlab='t', ylab='Population', type='l', col='blue',
        ylim=c(0, max(result[which(a==aVal[ii]), , 1 ] + result[which(a==aVal[ii]), , 2])),
        bty='n', fg=grey(0.6),
        main='Population Behavior of the Usher Model',
        sub=paste('Si Tang-Problem 3(b)-', ii)
        );
    
    lines(0:tmax, 
        result[which(a==aVal[ii]), 1:(tmax+1), 1 ],
        xlab='t', ylab='Population', type='l', col='red',
        bty='n', fg=grey(0.6)
        );
    lines(0:tmax, 
        result[which(a==aVal[ii]), 1:(tmax+1), 2 ],
        xlab='t', ylab='Population', type='l', col='green',
        bty='n', fg=grey(0.6)
        );

    abline(v=0);
    abline(h=0);

    legend(350, max (
        result[which(a==aVal[ii]), 1:(tmax+1), 1 ] + result[which(a==aVal[ii]), 1:(tmax+1), 2]),
        paste(c('N', 'X', 'Y'), '~ t, a= ', aVal[ii]), 
        col=c('blue', 'red', 'green'), lwd=1, bty='n'
            )

}
#/--#########################################################################
# Name: Si Tang
# Assignment: Assignment 6, exercise for Chapter 5
# Date: Nov. 28 2009
# Question: 4
##########################################################################/--

#------------------------------------------------------------------------------
# Subquestion: 4(a)
# Write a function which takes an (unsorted) vector of type numeric 
# and returns a linked list containing those values.
# Note: The linked list returned is in ascending order.
#------------------------------------------------------------------------------

Vector2lList <- function ( x ) {
    if(is.numeric(x)==FALSE){
        cat('The Input is not a numeric vector!');
    } else {
        i <- length(x);
        dataPart <- numeric();
        pointPart <- numeric();
    
        if (i<=0) {
            cat('No numeric values in the vector.');
            return();
        } else if (i==1) {
            dataPart <- x;
            pointPart <- -1;
        } else {
            dataPart <- sort (x);
            for(ii in 1:(i-1) ) {
                index = which( x == dataPart[ii] );
                pointPart[index] = which(x == dataPart[ii+1]);
            }
            index = which (x==dataPart[i]);
            pointPart[index] <- -1;
            dataPart <- x;
        }
        lList <- list ( dataPart=dataPart , pointPart=pointPart ) ;
        return (lList);
    }
}

#------------------------------------------------------------------------------
# Subquestion: 4(b)
# Write a function which takes a linked list and 
# adds a single element to it (preserving ordering).
# Note: The new element is added to the last position of the linked list.
#------------------------------------------------------------------------------

lList.add <- function (lList, element){
    if(is.numeric(lList$dataPart) & is.numeric(element)){
        if( length ( which(lList$dataPart == element) ) == 0 ) {
            i <- length(lList$dataPart);
            minValue <- min(lList$dataPart);
            currentValue <- minValue;
            currentPoint <- lList$pointPart[which(lList$dataPart == currentValue)];
            
            while(currentValue <= element) {
                    previousValue = currentValue;
                    previousPoint = currentPoint;
                    currentValue = lList$dataPart[currentPoint];
                    currentPoint = lList$pointPart[currentPoint];    
            }
        
            tmp <- lList$pointPart [lList$dataPart == previousValue]; 
            lList$pointPart [lList$dataPart == previousValue] <- i+1;
            lList$pointPart <- c(lList$pointPart, tmp);
            lList$dataPart <- c(lList$dataPart, element);    
            return(lList);
        } else {
            cat('The element already exists in the linked list!');
        }

    } else {
        cat ('The element or the linked list is not numeric! ');
    }
}

#------------------------------------------------------------------------------
# Subquestion: 4(c)
# Write a function which takes a linked list and removes a single element.
#------------------------------------------------------------------------------
lList.delete <- function (lList, element) {
    if(is.numeric(lList$dataPart) & is.numeric(element)) {
        minValue <- min(lList$dataPart);
        maxValue <- max(lList$dataPart);
        if( length ( which(lList$dataPart == element) ) == 1) {
            if ( element == minValue) {
                index <- which(lList$dataPart==minValue);
                for( ii in 1 : length(lList$pointPart) ) {
                    if(lList$pointPart[ii] > index ) {
                        lList$pointPart[ii] <- lList$pointPart[ii] - 1;
                    }
                }
            }else if (element == maxValue) {
                index <- which(lList$dataPart==maxValue);
                lList$pointPart[lList$pointPart==index] <- -1;
            } else {
                index <- which(lList$dataPart==element);
                currentPoint <- lList$pointPart[lList$dataPart==element];
                lList$pointPart[lList$pointPart==index] <- currentPoint;
                for (ii in 1: length(lList$pointPart)) {
                    if(lList$pointPart[ii]> index ) {
                        lList$pointPart[ii] <- lList$pointPart[ii] - 1;
                    }
                }
            }

        lList$dataPart <- lList$dataPart[-1 * index];
        lList$pointPart <- lList$pointPart[-1 * index];
        return (lList);
        
        } else if (length ( which(lList$dataPart == element) ) == 0){
            cat('The element is not in the linked list! ');
        } else {
            cat('There are more than one element having the value provided!');
        }
    }else {
        cat('The element or the linked list is not numeric! ');
    }
}

