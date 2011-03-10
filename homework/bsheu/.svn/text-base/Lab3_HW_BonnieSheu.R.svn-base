# name: Sheu, Bonnie
# assignment: Chapter 3 Lab Exercise 
# date: November 15, 2009
# filename: Lab3_HW_BonnieSheu
##################################################################

##################################################################
# name: Sheu, Bonnie
# assignment: Chapter 3 Lab Exercise
# date: 
# question: 1
# subquestion: a
# other files: Chapter 3 Lab Exercise (hand-written)
##################################################################
--

# Please see hand-written solutions. 


#/--
##################################################################
# name: Sheu, Bonnie
# assignment: Chapter 3 Lab Exercise
# date: 
# question: 1
# subquestion: b
# other files: Lab3_1b_Graph1_BonnieSheu.pdf,
#              Lab3_1b_Graph2_BonnieSheu.pdf
##################################################################
--

# Plot of first model, f(N)

modelF <- function (r, N, K) {
	return(r * N * (N/20 - 1) * (1 - N/K))
}

curveF <- function (x, r=10, K=200) {
	return(modelF(N=x, r=r, K=K))
}

plot(curveF(N),
     main='f(N) model of population growth', 
     xlab='N', 
     ylab='f(N) = 10*N*(N/20 - 1)*(1 - N/200)',
     type='l',
     fg=grey(0.6),
     bty='n'
)


# According to the generated plot, in the f(N) model the
# population decays at approximately N < 20 and N > 200
# (when the graph is below the x-axis), and grows at 
# 20 < N < 200 (when the graph is above the x-axis).


# Plot of second model, g(N)

modelG <- function (r, N, K) {
	return(r * N^2 * (1 - N/K))
}

curveG <- function (x, r=10, K=200) {
	return(modelG(N=x, r=r, K=K))
}

plot(curveG(N), 
     main='g(N) model of population growth', 
     xlab='N', 
     ylab='g(N) = 10*N^2*(1 - N/200)',
     type='l', 
     fg=grey(0.6), 
     bty='n'
)


# According to the generated plot, in the g(N) model the
# population decays when N > 200 and grows otherwise 
# (when N < 200).


#/--
##################################################################
# name: Sheu, Bonnie
# assignment: Chapter 3 Lab Exercise
# date: 
# question: 1
# subquestion: c
# other files: 
##################################################################
--

# The principal difference in the dynamics of the two population
# models occurs at the point N = 20. 
#
# In the f(N) model, only nonzero populations with size
# N > 20 grow and approach the carrying capacity K = 200. Those
# with N < 20 decay.
# 
# In the g(N) model, all nonzero populations approach carrying
# capacity K = 200.
#
# A possible biological interpretation of the intervals of
# growth and decay of the first f(N) model could be a colony of
# penguins. Since penguins live together and rely on a high
# level of social interaction to survive in the cold environment,
# a population size of N < 20 might be too small to survive.
#
# A possible biological interpretation of the second g(N) model
# could be an E. colo bacteria colony that grows under optimal
# conditions.


#/--
##################################################################
# name: Sheu, Bonnie
# assignment: Chapter 3 Lab Exercise
# date: 
# question: 2
# subquestion: a
# other files: Chapter 3 Lab Exercise (hand-written)
##################################################################
--

# Please see hand-written solutions. 


#/--
##################################################################
# name: Sheu, Bonnie
# assignment: Chapter 3 Lab Exercise
# date: 
# question: 2
# subquestion: b
# other files: Chapter 3 Lab Exercise (hand-written)
##################################################################
--

# Please see hand-written solutions. 


#/--
##################################################################
# name: Sheu, Bonnie
# assignment: Chapter 3 Lab Exercise
# date: 
# question: 2
# subquestion: c
# other files: Lab3_2c_DirectionField_BonnieSheu.pdf
##################################################################
--

fun <- function (N, t, r=1) {
	return (r * N - 4 * sin(4*t / 2*pi))
}

tRange <- c(0, 10); 
tStep <- 0.5;
nRange <- c(-5, 20); 
nStep <- 2.5;
k <- 5;
h <- 0.5;
otherPoints <- data.frame(x=numeric(), y=numeric());

plot(c(0, 10), c(-5, 20), 
     type='n', 
     bty='n', 
     fg=grey(0.6),
     ylab='N', 
     xlab='t');
     
for(tt in seq(tRange[1], tRange[2], by=tStep)) {
	for(nn in seq(nRange[1], nRange[2], by=nStep)) {
		t0 <- tt; n0 <- nn;
		t1 <- tt + tStep * h
		n1 <- nn + fun(N=nn, t=tt) * tStep * h
		arrows(t0, n0, t1, n1, length=0.1);
		}
	nZero <- 4 * sin(2 * tt / pi);
	otherPoints <- rbind(otherPoints, data.frame(x=tt, y=nZero));
	points(tt, nZero, col='blue', fg='blue', pch=15);
}

lines(otherPoints, col='blue');

# The generated direction field shows the fixed points as blue
# squares connected on a blue curve. The stability of the fixed
# points are reflected in the plot by direction field arrows
# that are in the close vicinity of the fixed points.
#
# To determine the whether a fixed point is stable, observe
# whether or not the direction of the nearby direction field
# arrows are pointing in the same direction as the direction of
# the blue curve or away from it (unstable). 


#/--
##################################################################
# name: Sheu, Bonnie
# assignment: Chapter 3 Lab Exercise
# date: 
# question: 2
# subquestion: d
# other files: Lab3_2d_Graph_BonnieSheu.pdf
##################################################################
--

# Assign initial conditions. Let subscripts 1, 2, and 3 denote the
# three different initial conditions.

t0_1 <- 0
t0_2 <- 0
t0_3 <- 0

N0_1 <- 0
N0_2 <- 1      # Assign arbitrary, different initial value
N0_3 <- 1.5    # Assign arbitrary, different initial value

constant <- function (t0, N0, r=1) {
	return((4*pi^2*r*sin(2*t0/pi) - 8*pi*cos(2*t0/pi) - 
	      (pi^2*r^2 + 4)*N0)/(pi^2*r^2 + 4))
}

general_soln <- function (t, C, r=1) {
	return ((4*pi^2*r*sin(2*t/pi) - 8*pi*cos(2*t/pi) - 
	       exp(r*t)*(pi^2*r^2 + 4)*C)/(pi^2*r^2 + 4))
}

C_1 <- constant(t0_1, N0_1)
C_2 <- constant(t0_2, N0_2)
C_3 <- constant(t0_3, N0_3)

plot(c(0, 10), c(-5, 25), 
     type='n', 
     bty='n',
     fg=grey(0.6),
     xlab='t',
     ylab='N'
)

abline(v=0)
abline(h=0)

x <- (1:1000)/50

lines(x, general_soln(t=x, C=C_1), col='black')
lines(x, general_soln(t=x, C=C_2), col='blue')
lines(x, general_soln(t=x, C=C_3), col='red')


# The generated graph is related to the direction field from
# part c) in that the three solution curves are vertical shifts of
# the blue curve (with fixed points as blue squares) in part c). 
# The solution curves are shifted along the vertical axis due to
# the three different intial values.


#/--
##################################################################
# name: Sheu, Bonnie
# assignment: Chapter 3 Lab Exercise
# date: 
# question: 3
# subquestion: a
# other files: Chapter 3 Lab Exercise (hand-written)
##################################################################
--

# Please see hand-written solutions. 

#/--
##################################################################
# name: Sheu, Bonnie
# assignment: Chapter 3 Lab Exercise
# date: 
# question: 3
# subquestion: b
# other files: Lab3_3b_DirectionField_BonnieSheu.pdf
##################################################################
--

# Define function f(v)

voltage <- function (v) {
	return(-gNa*(v - vNa) - gK*(v - vK))
}


# Set plotting variables and parameters

capac <- 0.15e-6
gNa <- 1e-3
gK <- 100e-3
vNa <- 58.1e-3
vK <- -69.6e-3

vRange <- c(-100e-3, 100e-3)
vStep <- 10e-3
tRange <- c(0, 10)
tStep <- 0.5
h <- 0.5     # delta t
k <- 1       # aspect correction


# Initialize plot

plot(tRange, vRange, 
     type='n', 
     bty='n', 
     fg=grey(0.6),
     xlab='t',
     ylab='V',
)

abline(v=0)
abline(h=0)


# Main loop

for (ii in seq(vRange[1], vRange[2], by=vStep)) {
	for (jj in seq(tRange[1], tRange[2], by=tStep)) {
		v0 <- ii;
		v1 <- ii + voltage(ii) * h * k;
		t0 <- jj;
		t1 <- jj + tStep * h;
		arrows(t0, v0, t1, v1, code=2, length=0.05)
	}
}


# The generated direction field plot agrees with the fixed point
# stability analysis performed in part a), because the direction
# vectors at the fixed point V = -68.3 mV = -0.0683 V are 
# horizontal (indicating slope value of 0).


#/--
##################################################################
# name: Sheu, Bonnie
# assignment: Chapter 3 Lab Exercise
# date: 
# question: 3
# subquestion: c
# other files: Lab3_3c_Graph_BonnieSheu.pdf
##################################################################
--

# Assign initial conditions. Let subscripts 1, 2, and 3 denote the
# three different initial conditions.

t0_1 <- 0
t0_2 <- 0
t0_3 <- 0

v0_1 <- -50
v0_2 <- 1
v0_3 <- 50


general_soln2 <- function (C, t) {
	return((100 * -69.6 + 58.1)/101 - C * exp(-101*t))
}

constant2 <- function (v0, t0) {
	return(-v0*exp(101*t0) - 68.34*exp(101*t0))
}

C_1 <- constant2(v0=v0_1, t0=t0_1)
C_2 <- constant2(v0=v0_2, t0=t0_2)
C_3 <- constant2(v0=v0_3, t0=t0_3)

plot(c(0, 0.25), c(-100, 100), type='n', bty='n', fg=grey(0.6), xlab='t', ylab='v')

abline(v=0)
abline(h=0)
abline(h=-68.33, lty=3)

lines((0:1000)/1000, general_soln2(C=C_1, t=(0:1000)/1000),
       col='blue')
lines((0:1000)/1000, general_soln2(C=C_2, t=(0:1000)/1000),
       col='black')
lines((0:1000)/1000, general_soln2(C=C_3, t=(0:1000)/1000),
       col='red')

# According to the plot, the model predicts that the long-term
# behavior of the membrane potential should approach equilibrium
# value V = -68.34. If we just looked at this plot we would not
# have needed an analytic solution to come to this conclusion.


#/--
##################################################################
# name: Sheu, Bonnie
# assignment: Chapter 3 Lab Exercise
# date: 
# question: 4
# subquestion: a
# other files: 
##################################################################
--

# The following pseudocode algorithm for plotting flow lines is
# structured based on the example pseudocode listing provided on
# page 2 of the Chapter 3 Lab Exercise handout.

# define function F(x: REAL, t:REAL)
# var ARRAY z: REAL;                   # Critical points
# var t_min, t_max: INT;               # Plot limits
# var sign: INT;
# var t0, t_mid, t1, y: REAL;          # Temporary placeholders
# init plot
# plot F(x: REAL, t: REAL);
# z <- solve for F(x: REAL, t: REAL) = 0
# define function G(x: REAL, t: REAL)  # Derivative of F(x,t)
# for ii : INT in 0 to length z
# begin
#     t0 <- z[ii-1]
#     t1 <- z[ii]
#     t_mid <- (t0 + t1)/2
#     sign <_ (t_mid/abs(t_mid)) * (G(0, t_mid)/abs(G(0, t_mid)))
#     if (sign = 1)
#     begin
#         draw arrow (t0, y) - (t1, y)
#     end
#     else
#     begin
#         draw arrow (t1, -y) - (t0, -y)
#     end
# end


#/--
##################################################################
# name: Sheu, Bonnie
# assignment: Chapter 3 Lab Exercise
# date: 
# question: 4
# subquestion: b
# other files: Lab3_4b_FlowLines_BonnieSheu.pdf
##################################################################
--

fun <- function (x, t) {
	return(x/3 * (1 - x/5))
}

curve(fun, from=-0.5, to=5.5, 
      col='black', 
      xlab='t',
      ylab='f(x,t) = x/3 * (1 - x/5)',
      fg=gray(0.6),
      bty='n'
)

abline(h=0, col='blue')
abline(v=0, col='blue')

arrows(c(0.125, -0.125, 5.5), 
       c(0.025, -0.025, -0.025), 
       c(4.875, -0.5, 5.125), 
       c(0.025, -0.025, -0.025), 
       col='red'
)


#/--
##################################################################