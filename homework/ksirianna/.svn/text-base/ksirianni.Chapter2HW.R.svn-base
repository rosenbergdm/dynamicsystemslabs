#!/usr/bin/env r
# encoding: utf-8
# 
# name: STUDENT_NAME
# assignment: 3
# 10/22/09
# filename: Sirianni Homework 3 Final
#############################################################################
#######NOTE: SOME QUESTIONS DONE ON PAPER, turned in seperately!!!#######
#/--#########################################################################
# name: Katie Sirianni
# assignment: 3
# date: 10/22/09
# question: 2
# subquestion: a
# other files: NAMES_OF_OTHER_RELATED_FILES
##########################################################################/--

# to adjust the range of the function plotted by x and y, change rangeX and rangeY.  Keep in mind these numbers are divided by a specified number in the function. 
rangeX<- -50:500 /50
rangeY<- -50:500 /50

# to adjust the range of the plot, change xLim and yLim
xLim<- c(-.5, 5)
yLim<- c(-.5,5)

cobImapFun<- function(x) {
	return(3*x-(3*x**2)/4 +1);
}
max_iter<-50
y_vector<- x_vector<- numeric(
   max_iter *2);
y_vector[1] <-0;
x_vector[1] <- 0.5;
for(ii in 1:50) {
   y_vector[2*ii] <- cobImapFun(
     x_vector[2*ii -1]);
   y_vector[2*ii+1] <-
     y_vector[2*ii];
   x_vector[2*ii] <-
     x_vector[2*ii-1]
   x_vector[2*ii+1] <-
     y_vector[2*ii+1];
}
plot(rangeX, 
       cobImapFun(rangeY), 
       type= "l",
       main= "Cobweb plot", 
        xlim= xLim, ylim= yLim,
       xaxs="i",
      yaxs="i")

abline(0,1, col="red");
lines(x_vector, y_vector,  col= "blue")

lines(3.097,3.097, type<- "p")

## A fixed point occurs at 3.097.  It is not stable, as shown by the fact that the blue line swirls outward.  

#/--#########################################################################
# name: Katie Sirianni
# assignment: 3
# date: 10/22/09
# question: 2
# subquestion: b
# other files: NAMES_OF_OTHER_RELATED_FILES
##########################################################################/--

# to adjust the range of the function plotted by x and y, change rangeX and rangeY.  Keep in mind these numbers are divided by a specified number in the function. 
rangeX<- -10:500 /10
rangeY<- -10:500 /10

# to adjust the range of the plot, change xLim and yLim
xLim<- c(-.5, 50)
yLim<- c(-.5,1300)

cobImapFun<- function(x) {
	return(100*x-(2*x**2));
}
max_iter<-50
y_vector<- x_vector<- numeric(
   max_iter *2);
y_vector[1] <-0;
x_vector[1] <- 0.5;
for(ii in 1:50) {
   y_vector[2*ii] <- cobImapFun(
     x_vector[2*ii -1]);
   y_vector[2*ii+1] <-
     y_vector[2*ii];
   x_vector[2*ii] <-
     x_vector[2*ii-1]
   x_vector[2*ii+1] <-
     y_vector[2*ii+1];
}
plot(rangeX, 
       cobImapFun(rangeY), 
       type= "l",
       main= "Cobweb plot", 
        xlim= xLim, ylim= yLim,
       xaxs="i",
      yaxs="i")

abline(0,1, col="red");
lines(x_vector, y_vector,  col= "blue")

lines(49.5,49.5, type<- "p")

## A fixed point occurs at 49.5.  It is stable, because the blue line goes right to it. Another fixed point occurs at 0, but this is unstable.

#/--#########################################################################
# name: Katie Sirianni
# assignment: 3
# date: 10/22/09
# question: 2
# subquestion: c
# other files: NAMES_OF_OTHER_RELATED_FILES
##########################################################################/--


cobImapFun <-function(x) {
   return(-100*x + (x**2)/2);
}
max_iter<-50
y_vector<- x_vector<- numeric(
   max_iter *2);
y_vector[1] <-0;
x_vector[1] <- 0.5;
for(ii in 1:50) {
   y_vector[2*ii] <- cobImapFun(
     x_vector[2*ii -1]);
   y_vector[2*ii+1] <-
     y_vector[2*ii];
   x_vector[2*ii] <-
     x_vector[2*ii-1]
   x_vector[2*ii+1] <-
     y_vector[2*ii+1];
}
plot(-1000:50000 / 10,
        cobImapFun(-1000:50000 / 10),
        xlab="Nt", ylab="N(t+1)",
        main= "Cobweb plot", type = "l",
        xlim= c(-150,300), ylim= c(-4000,1500),
       xaxs="i",
      yaxs="i");
abline(0,1, col="red");
lines(x_vector, y_vector,  col= "blue")
lines(202,202, type= "p")

# An equilibrium point occurs at 202.  However, the blue line doesn't go anywhere near it, so it is an unstable equilibrium.



#/--#########################################################################
# name: Katie Sirianni
# assignment: 3
# date: 10/22/09
# question: 3
# subquestion: SUBQUESTION_LETTER_IF_PRESENT_OTHERWISE_EMPTY
# other files: NAMES_OF_OTHER_RELATED_FILES
##########################################################################/--

# to adjust the range of the function plotted by x and y, change rangeX and rangeY.  Keep in mind these numbers are divided by a specified number in the function. 
rangeX<- -500:500 /50
rangeY<- -500:500 /50

# to adjust the range of the plot, change xLim and yLim
xLim<- c(-10, 10)
yLim<- c(-10,10)

#
plotEquation<- function(x) {
	return(8*x*(3+x));
}

cobwebPlotGenerator<- function(plotEquation, rangeX, rangeY, xLim, yLim) {

max_iter<-50
y_vector<- x_vector<- numeric(
   max_iter *2);
y_vector[1] <-0;
x_vector[1] <- 0.5;
for(ii in 1:50) {
   y_vector[2*ii] <- plotEquation(
     x_vector[2*ii -1]);
   y_vector[2*ii+1] <-
     y_vector[2*ii];
   x_vector[2*ii] <-
     x_vector[2*ii-1]
   x_vector[2*ii+1] <-
     y_vector[2*ii+1];
}
plot(rangeX, 
       plotEquation(rangeY), 
       type= "l",
       main= "Cobweb plot", 
       xlab= "Nt", ylab= "N(t+1)"
        xlim= xLim, ylim= yLim,
       xaxs="i",
      yaxs="i")

abline(0,1, col="red");
lines(x_vector, y_vector,  col= "blue")
}

#/--#########################################################################
# name: Katie Sirianni
# assignment: 3
# date: 10/22/09
# question: 4
# subquestion: c
# other files: Hard Copy of other questions
##########################################################################/--

initialGlucose<- 100        # initial glucose conc. in mg/dl
k<- -.01                    # removal rate of glucose FROM bloodstream
maxT<-1000                   # transitions
t<- 1:maxT                  # t is time in minutes, this creates the          
                           # time vector
glucoseConc<- numeric()  
glucoseConc[1]<- initialGlucose 

# to find glucose conc at each time until the max t:               
for(g in 2:maxT) {          
	glucoseConc<- c(glucoseConc, 100*exp(k*g))
	}

plot(t, glucoseConc, 
	main= "Concentration of Glucose in Bloodstream (mg/dl)",
	xlab="Time(min)", 
	ylab= "Glucose Concentration(mg/dl)", 
	type = "l", 
	col= "seagreen")

#/--#########################################################################
# name: Katie Sirianni
# assignment: 3
# date: 10/22/09
# question: 5
# subquestion: c
# other files: Hard copy of other questions
##########################################################################/--

initialGlucose<- 100
k<- -.01
a<- 4
maxT<- 800
t<- 1:maxT
glucoseConc<- numeric()
glucoseConc[1]<- initialGlucose

for(g in 2:maxT) {
	glucoseConc<- c(glucoseConc, 
	(a/k*exp(k*g)+initialGlucose*exp(k*g)-a/k))
	}
	
plot(t, glucoseConc,
   main= "Concentration of Glucose in Bloodstream (mg/dl)",
   xlab= "Time(min)",
   ylab= "Glucose Concentration (mg/dl)",
   type= "l",
   col= "seagreen")

   plotEquation<- function(x) {
   	return(5*x*(4-x))
   	}

#/--#########################################################################
