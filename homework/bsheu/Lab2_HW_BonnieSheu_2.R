# name: Sheu, Bonnie
# assignment: Chapter 2 Lab Exercise 
# date: November 15, 2009
# filename: Lab2_HW_BonnieSheu
##################################################################

##################################################################
# name: Sheu, Bonnie
# assignment: Chapter 2 Lab Exercise
# date: 
# question: 1
# subquestion: a
# other files: 
##################################################################
--

# FIND EQUILIBRIA (FIXED POINTS)
#
# N(t+1) = 41*N(t) - 10*N(t)^2 
#        = (41 - 10*N(t))*N(t)
# 
# If x = N(t+1) = N(t), then
# 
#    x = (41 - 10*x)*x
#      = 41*x - 10*x^2
#
#    0 = 40*x - 10*x^2
#      = 10*x*(4 - x)
#
# Solve for x, we get fixed points x = 0 and x = 4.
#
#
# DETERMINE EQUILIBRIA STABILITY
#
# Find first derivative:
#
# x' = 41 - 20*x
#
#
# Plug in fixed point values into first derivative and analyze
# the absolute value:
#
# abs(x'(0)) = abs(41 - 20*0) = 41 > 1     (unstable at x = 0)
# abs(x'(4)) = abs(41 - 20*4) = 39 > 1     (unstable at x = 4)
#
# 
# There are no stable nonzero equilibria that this particular
# population will approach in the long run.


#/--
##################################################################
# name: Sheu, Bonnie
# assignment: Chapter 2 Lab Exercise
# date: 
# question: 1
# subquestion: b
# other files: 
##################################################################
--

# FIND EQUILIBRIA (FIXED POINTS)
#
# N(t+1) = 41*N(t) + 2*N(t)^2 
#        = (41 + 2*N(t))*N(t)
# 
# If x = N(t+1) = N(t), then
# 
#    x = (41 + 2*x)*x
#      = 41*x + 2*x^2
#
#    0 = 40*x + 2*x^2
#      = 2*x*(20 + x)
#
# Solve for x, we get fixed points x = 0 and x = -20.
#
#
# DETERMINE EQUILIBRIA STABILITY
#
# Find first derivative:
#
# x' = 41 + 4*x
#
#
# Plug in fixed point values into first derivative and analyze
# the absolute value:
#
# abs(x'(0)) = abs(41 + 4*0) = 41 > 1     (unstable at x = 0)
# abs(x'(-20)) = abs(41 + 4*-20) = 39 > 1 (unstable at x = 4)
#
# 
# Again, there are no stable nonzero equilibria that this
# particular population will approach in the long run.


#/--
##################################################################
# name: Sheu, Bonnie
# assignment: Chapter 2 Lab Exercise
# date: 
# question: 2
# subquestion: a
# other files: Lab2_2a_CobwebPlot_BonnieSheu.pdf
##################################################################
--

fun <- function (x) {
	return(x*(5 - 4*x))
}

plotGenericImap <- function(fun, nIter=20) {
 
  iVal <- 0.02;                              
  iiVal <- iVal / 2;
  
  while (fun(iVal) <= 0 | fun(iiVal) == iiVal) {
    iVal <- iVal / 10;
    iiVal <- iVal / 2;
  }
  
  xRange <- numeric(length=2);
  xRange[1] <- 0;
  xMax <- iiVal;                          
  
  while (fun(xMax) >= 0) {
    xMax <- 1.25 * xMax;
  }
  
  xRange[2] <- xMax;
  
  yArr <- numeric(length=nIter * 2);
  xArr <- yArr;
  xArr[1] <- 0;
  yArr[1] <- iiVal;
  
  for (ii in 1:nIter) {
    yArr[2 * ii] <- fun(yArr[2 * ii - 1]);
    yArr[2 * ii + 1] <- yArr[2 * ii];
    xArr[2 * ii] <- xArr[2 * ii - 1];
    xArr[2 * ii + 1] <- yArr[2 * ii];
  }
  
  curve(fun, xRange[1], xRange[2], 
  col='blue', bty='n', fg=grey(0.6));
  abline(h=0);
  abline(v=0);
  lines(c(xRange[1]-100, xRange[2]+100), 
  c(xRange[1]-100, xRange[2]+100),col='red');
  lines(xArr, yArr, col='green');
  
  return();
}

plotGenericImap(fun)


# GRAPHICALLY IDENTIFY FIXED POINTS AND DETERMINE THEIR STABILITY
#
# According to the generated plot, there are two fixed points at
# x = 0 and x = 1.0 where the blue parabola (graph of logistic
# model) intersects the red identity line.
#
# The fixed point at x = 0 is unstable because it has a steeper
# slope than the identity line (hence the absolute value 
# of the slope at x = 0 must be > 1). 
#
# The fixed point at x = 1.0 is also unstable because solution 
# bounces around the fixed point. 


#/--
##################################################################
# name: Sheu, Bonnie
# assignment: Chapter 2 Lab Exercise
# date: 
# question: 2
# subquestion: b
# other files: Lab2_2b_CobwebPlot_BonnieSheu.pdf
##################################################################
--

fun <- function (x) {
	return(2*x*(1 - 3*x/2))
}
	
plotGenericImap <- function(fun, nIter=20) {
 
  iVal <- 0.02;                              
  iiVal <- iVal / 2;
  
  while (fun(iVal) <= 0 | fun(iiVal) == iiVal) {
    iVal <- iVal / 10;
    iiVal <- iVal / 2;
  }
  
  xRange <- numeric(length=2);
  xRange[1] <- 0;
  xMax <- iiVal;                          
  
  while (fun(xMax) >= 0) {
    xMax <- 1.25 * xMax;
  }
  
  xRange[2] <- xMax;
  
  yArr <- numeric(length=nIter * 2);
  xArr <- yArr;
  xArr[1] <- 0;
  yArr[1] <- iiVal;
  
  for (ii in 1:nIter) {
    yArr[2 * ii] <- fun(yArr[2 * ii - 1]);
    yArr[2 * ii + 1] <- yArr[2 * ii];
    xArr[2 * ii] <- xArr[2 * ii - 1];
    xArr[2 * ii + 1] <- yArr[2 * ii];
  }
  
  curve(fun, xRange[1], xRange[2], 
  col='blue', bty='n', fg=grey(0.6));
  abline(h=0);
  abline(v=0);
  lines(c(xRange[1]-100, xRange[2]+100), 
  c(xRange[1]-100, xRange[2]+100),col='red');
  lines(xArr, yArr, col='green');
  
  return();
}

plotGenericImap(fun)


# GRAPHICALLY IDENTIFY FIXED POINTS AND DETERMINE THEIR STABILITY
#
# According to the generated plot, there are two fixed points at
# x = 0 and x = 1/3.
#
# The fixed point at x = 0 is unstable because it has a steeper
# slope than the identity line (which means the absolute value 
# of the slope must be > 1). 
#
# The fixed point at x = 1/3 is stable because the solution
# converges to the fixed point at the intersection of the graph of
# the function (blue parabola) and red identity line. We also see
# that the slope at x = 1/3 is almost horizontal (< 1).


#/--
##################################################################
# name: Sheu, Bonnie
# assignment: Chapter 2 Lab Exercise
# date: 
# question: 2
# subquestion: c
# other files: Lab2_2c_CobwebPlot_BonnieSheu.pdf
##################################################################
--

fun <- function (x) {
	return(x/2 - x^2/5)
}
	
plotGenericImap <- function(fun, nIter=20) {
 
  iVal <- 1.0;                              
  iiVal <- iVal / 2;
  
  while (fun(iVal) <= 0 | fun(iiVal) == iiVal) {
    iVal <- iVal / 10;
    iiVal <- iVal / 2;
  }
  
  xRange <- numeric(length=2);
  xRange[1] <- 0;
  xMax <- iiVal;                          
  
  while (fun(xMax) >= 0) {
    xMax <- 1.25 * xMax;
  }
  
  xRange[2] <- xMax;
  
  yArr <- numeric(length=nIter * 2);
  xArr <- yArr;
  xArr[1] <- 0;
  yArr[1] <- iiVal;
  
  for (ii in 1:nIter) {
    yArr[2 * ii] <- fun(yArr[2 * ii - 1]);
    yArr[2 * ii + 1] <- yArr[2 * ii];
    xArr[2 * ii] <- xArr[2 * ii - 1];
    xArr[2 * ii + 1] <- yArr[2 * ii];
  }
  
  curve(fun, xRange[1], xRange[2], 
  col='blue', bty='n', fg=grey(0.6));
  abline(h=0);
  abline(v=0);
  lines(c(xRange[1]-100, xRange[2]+100), 
  c(xRange[1]-100, xRange[2]+100),col='red');
  lines(xArr, yArr, col='green');
  
  return();
}

plotGenericImap(fun)

# GRAPHICALLY IDENTIFY FIXED POINTS AND DETERMINE THEIR STABILITY
#
# Algebraically, we determine there are two fixed points at
# x = 0 and x = -5/2.
#
# The fixed point at x = 0 is stable because it the solution
# converges to the fixed point at the intersection of the graph
# of the function (blue parabola) and red identity line. 
#
# The fixed point at x = -5/2 is unstable because the solution
# bounces around the fixed point. 


#/--
##################################################################
# name: Sheu, Bonnie
# assignment: Chapter 2 Lab Exercise
# date: 
# question: 2
# subquestion: d
# other files: Lab2_2d_CobwebPlot_BonnieSheu.pdf
##################################################################
--

fun <- function (x) {
	return(x*(5/2 - 7*x))
}
	
plotGenericImap <- function(fun, nIter=20) {
 
  iVal <- 1.0;                              
  iiVal <- iVal / 2;
  
  while (fun(iVal) <= 0 | fun(iiVal) == iiVal) {
    iVal <- iVal / 10;
    iiVal <- iVal / 2;
  }
  
  xRange <- numeric(length=2);
  xRange[1] <- 0;
  xMax <- iiVal;                          
  
  while (fun(xMax) >= 0) {
    xMax <- 1.25 * xMax;
  }
  
  xRange[2] <- xMax;
  
  yArr <- numeric(length=nIter * 2);
  xArr <- yArr;
  xArr[1] <- 0;
  yArr[1] <- iiVal;
  
  for (ii in 1:nIter) {
    yArr[2 * ii] <- fun(yArr[2 * ii - 1]);
    yArr[2 * ii + 1] <- yArr[2 * ii];
    xArr[2 * ii] <- xArr[2 * ii - 1];
    xArr[2 * ii + 1] <- yArr[2 * ii];
  }
  
  curve(fun, xRange[1], xRange[2], 
  col='blue', bty='n', fg=grey(0.6));
  abline(h=0);
  abline(v=0);
  lines(c(xRange[1]-100, xRange[2]+100), 
  c(xRange[1]-100, xRange[2]+100),col='red');
  lines(xArr, yArr, col='green');
  
  return();
}

plotGenericImap(fun)

# GRAPHICALLY IDENTIFY FIXED POINTS AND DETERMINE THEIR STABILITY
#
# Algebraically, we determine there are two fixed points at
# x = 0 and x = 3/14.
#
# The fixed point at x = 0 is unstable because it has a steeper
# slope than the identity line (which means the absolute value 
# of the slope must be > 1). 
#
# The fixed point at x = 3/14 is stable because the solution
# converges to the fixed point at the intersection of the graph
# of the function (blue parabola) and red identity line. 


#/--
##################################################################
# name: Sheu, Bonnie
# assignment: Chapter 2 Lab Exercise
# date: 
# question: 3
# subquestion: 
# other files: 
##################################################################
--

plotGenericImap2 <- function(fun, nIter=20) {
  iVal <- 1;
  iiVal <- iVal / 2;
  while (fun(iVal) <= 0 | iiVal == fun(iiVal)) {
    iVal <- iVal / 10;
    iiVal <- iVal / 2;
  }
  
  xArr <- yArr <- numeric(length=2 * nIter);
  xArr[1] <- 0;
  yArr[1] <- iiVal;
  
  xRange <- numeric(length=2);
  xRange[1] <- 0;
  xRange[2] <- iiVal;
  
  while ( fun(xRange[2]) >= 0 ) {
    xRange[2] <- 1.25 * xRange[2];
  }
  
  for (ii in 1:nIter) {
    yArr[2 * ii] <- fun(yArr[2 * ii - 1]);
    yArr[2 * ii + 1] <- yArr[2 * ii];
    xArr[2 * ii] <- xArr[2 * ii - 1];
    xArr[2 * ii + 1] <- yArr[2 * ii];
  }
  
  curve(fun, xRange[1], xRange[2], 
  col='blue', bty='n', fg=grey(0.6));
  abline(h=0);
  abline(v=0);
  lines(c(xRange[1]-100, xRange[2]+100), 
  c(xRange[1]-100, xRange[2]+100),col='red');
  lines(xArr, yArr, col='green');
  
  return();
}

plotGenericImap2(fun)


#/--
##################################################################
# name: Sheu, Bonnie
# assignment: Chapter 2 Lab Exercise
# date: 
# question: 4
# subquestion: a
# other files: 
##################################################################
--

# Differential equation:   Gdot = k*G(t)
#
# This is a homogenous linear differential equation.


#/--
##################################################################
# name: Sheu, Bonnie
# assignment: Chapter 2 Lab Exercise
# date: 
# question: 4
# subquestion: b
# other files: 
##################################################################
--

# ANALYTICAL SOLUTION OF ODE
#
# Let "integral()" represent the mathematical symbol for integral.
#
#                           dG/dt = k*G(t)
#                  dG/dt * 1/G(t) = k
#    integral( dG/dt * 1/G(t) dt) = integral( k dt )
#           integral( 1/G(t) dG ) = integral( k dt )
#                        log G(t) = k*t + C
#                            G(t) = e^(k*t + C)
# 
# Use initial condition G(0) to determine C.
#
#      G(0) = e^(t*0 + C) = e^C
#         C = ln G(0)
#
# Hence, the complete solution is:
#
#      G(t) = e^(k*t + ln G(0))

#/--
##################################################################
# name: Sheu, Bonnie
# assignment: Chapter 2 Lab Exercise
# date: 
# question: 4
# subquestion: c
# other files: Lab2_4c_Graph_BonnieSheu.pdf
##################################################################
--

#      G(t) = e^(k*t + ln G(0))
#           = e^(-0.01*t + ln 100)

glucose <- function(t) {
  return(exp(-0.01*t)*100)
}

xlab <- "time (t) (minutes)"
ylab <- "Blood glucose concentration G(t) (mg / dl)"
plot(glucose, from=0, to=800, xlab=xlab, ylab=ylab)


#/--
##################################################################
# name: Sheu, Bonnie
# assignment: Chapter 2 Lab Exercise
# date: 
# question: 4
# subquestion: d
# other files: 
##################################################################
--

# The equilibrium concentration of blood sugar in this model
# is 0 mg/dl. The equilibrium is stable.


#/--
##################################################################
# name: Sheu, Bonnie
# assignment: Chapter 2 Lab Exercise
# date: 
# question: 5
# subquestion: a
# other files: 
##################################################################
--


# Differential equation:   Gdot = k*G(t) + a
#
# This is a non-homogenous linear differential equation.



#/--
##################################################################
# name: Sheu, Bonnie
# assignment: Chapter 2 Lab Exercise
# date: 
# question: 5
# subquestion: b
# other files: 
##################################################################
--

#           Gdot = dG/dt = G'(t) = k*G(t) + a
#                 G'(t) - k*G(t) = a
#
# Find integration factor:
# (Let "integral()" represent mathematical symbol for integral.)
#
# Let        mu = e^(integral(-k dt))
#               = e^(-k*t)
#
#      d(mu)/dt = -k * e^(-k*t)
#
#          mu*a = mu*(G'(t) - k*G(t))
#               = e^(-k*t) * G'(t) - k * e^(-k*t) * G(t)
#               = a * e^(integral(-k dt))
#               = mu * G'(t) + mu' * G(t)
#               = (G(t) * mu)'
#
# integral((G(t) * mu)' dt) = integral(a * e^(-k*t) dt)
#             G(t) * mu + C = integral(a * e^(-k*t) dt)
#                 G(t) * mu = -(a/k) * e^(-k*t) + C
#                      G(t) = (-a/k * e^(-k*t) + C) / e^(-k*t)
#                           = -a/k + C / e^(-k*t)
#
# Use initial condition G(0) to determine C.
#
#      G(0) = -a/k + C / e^(-k*0)
#           = -a/k + C 
#         C = G(0) + a/k
#
# Hence, the complete solution is:
#
#      G(t) = -a/k + (G(0) + a/k) / e^(-k*t)



#/--
##################################################################
# name: Sheu, Bonnie
# assignment: Chapter 2 Lab Exercise
# date: 
# question: 5
# subquestion: c
# other files: Lab2_5c_Graph_BonnieSheu.pdf
##################################################################
--

# G(t) = -a/k + (G(0) + a/k) / e^(-k*t)
#      = -4/-0.01 + (100 + 4/-0.01)/e^(-(-0.01)*t)

glucose <- function(t) {
  return(4/0.01 + (100 - 4/0.01)/exp(0.01*t))
}

xlab <- "time (t) (minutes)"
ylab <- "Blood glucose concentration G(t) (mg / dl)"
plot(glucose, from=0, to=800, xlab=xlab, ylab=ylab)



#/--
##################################################################
# name: Sheu, Bonnie
# assignment: Chapter 2 Lab Exercise
# date: 
# question: 5
# subquestion: d
# other files: 
##################################################################
--

# The equilibrium concentration of blood sugar in the model with 
# glucose infusion is 400 mg/dl. The equilibrium is stable.


#/--
##################################################################
