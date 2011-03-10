# name: Sheu, Bonnie
# assignment: Chapter 4 Lab Exercise 
# date: November 21, 2009
# filename: Lab4_HW_BonnieSheu
##################################################################

##################################################################
# name: Sheu, Bonnie
# assignment: Chapter 4 Lab Exercise
# date: 
# question: 1
# subquestion: a
# other files: 
##################################################################
--

# Let   I_dot = beta * (N - I) * I - gamma * I.
#
# To find equilibria (fixed points), set I_dot equal to 0:
#
#         I_dot = beta * (N - I) * I - gamma * I
#               = 0
#
#     gamma * I = beta * (N - I) * I    # divide I from both sides
#         
#         gamma = beta * (N - I)
#               = beta * N - beta * I
#
#      beta * I = beta * N - gamma
#
#             I = (beta * N - gamma) / (beta)
#
#
# Thus, the fixed points are I = 0 and 
#                            I = (beta * N - gamma)/ (beta).
#
# This means that the rate of # infected individuals (I_dot) = 0
# (is constant) when:
# 
#      1) there are no infected individuals, and 
#      2) the recovery rate of infected individuals is equal to
#         the overall infection rate (infection rate per
#         encounter * current # infected individuals).
#
# Thus, predictions for the course of an epidemic depends on two
# parameters: beta (infection rate) and gamma (recovery rate).


#/--
##################################################################
# name: Sheu, Bonnie
# assignment: Chapter 4 Lab Exercise
# date: 
# question: 1
# subquestion: b
# other files: 
##################################################################
--

# To determine stability of fixed points,
# 
# Let   I_dot = f(I)
#             = beta * (N - I) * I - gamma * I 
#             = (beta * N * I) - (beta * I^2) - (gamma * I)
# 
# Determine and solve for f'(I) and fixed point values:
#
#       f'(I) = (beta * N) - (2 * beta * I) - (gamma)
#       
#       Fixed point is stable if:      f'(I) < 0.
#       Fixed point is unstable if:    f'(I) > 0.
#      
# 
# At fixed point I = 0:
#      
#       f'(0) = (beta * N) - (2 * beta * 0) - (gamma)
#             = beta * N - gamma
#
#                  f'(0) < 0
#       beta * N - gamma < 0
#               beta * N < gamma
#
# Therefore, fixed point I = 0...
#  
#       is stable        if beta * N < gamma, and
#       is unstable      if beta * N > gamma. 
#
#
# At fixed point I = (beta * N - gamma)/ (beta):
#
#      f'((beta * N - gamma) / (beta))
#   = (beta * N) - (2 * beta * (beta * N - gamma) / beta) - gamma
#   = (beta * N) - (2 * beta * N - 2 * gamma) - gamma
#   = (beta * N) - (2 * beta * N) + (2 * gamma) - gamma
#   = - (beta * N) + gamma
#
#      f'((beta * N - gamma) / (beta)) < 0   
#                 - (beta * N) + gamma < 0
#                                gamma < beta * N
# 
# Therefore, fixed point I = (beta * N - gamma)/ (beta)...
#
#       is stable        if gamma < beta * N, and 
#       is unstable      if gamma > beta * N.
#
#
# The implications for the epidemiological prediction 


#/--
##################################################################
# name: Sheu, Bonnie
# assignment: Chapter 4 Lab Exercise
# date: 
# question: 1
# subquestion: c
# other files: Lab4_1c_Graph_BonnieSheu.pdf
##################################################################
--

# Solve equation for I (fraction of infected) over 365 days
# using Forward Euler method:
#
# Let  N = 1 = I + S
#      beta = 0.3 / day
#      gamma = 0.1 / day
#     
#      Start with initial value I_0 = 0.01
#      Vary time step (delta_t) from 0.1 days to 10 days


I_dot <- function (I, N=1, beta=0.3, gamma=0.1) {
	return(beta * I * (N - I) - gamma * I)
}

Forward <- function(I_dot, delta_t, tRange=c(0,365), I_0=0.01) {
	t_vals <- seq(from=tRange[1], to=tRange[2], by=delta_t);
	I_vals <- t_vals;
	I_vals[1] <- I_0;
	for (ii in 2:length(t_vals)) {
		I_vals[ii] <- I_vals[ii-1] + delta_t * I_dot(I=I_vals[ii-1])
		}
	result <- list(t=t_vals, I=I_vals);
}

tSteps <- c(0.1, 0.2, 0.4, 0.6, 0.8, 1, 2, 4, 6, 8, 10);
colors <- rainbow(n=length(tSteps));

plot(c(0, 100), c(-0.45, 1), 
     type='n', 
     bty='n',
     fg=grey(0.6),
     main='Fraction of infected individuals over 1 year',
     xlab='time (t) in days',
     ylab='fraction of infected individuals in population (I)'
);
     
abline(v=0);
abline(h=0);

for (tt in 1:length(tSteps)) {
	res <- Forward(I_dot, delta_t=tSteps[tt]);
	lines(res$t, res$I, col=colors[tt]);
}

legend(c('0.1', '0.2', '0.4', '0.6', '0.8', '1', '2', '4', '6', '8', '10'),
       x=70,
       y=0.45,
       lwd=2,
       col=colors,
       title='time step (delta_t)',
       bg='white'
)


# According to the generated graph, increasing the time step 
# (delta_t) from 0.1 to 10 has the effect of increasing the
# instability of the numerical solution.
#
# Up until about time steps delta_t = 4 or 6 the behavior of the
# graph is consistent with the behavior predicted in the 
# theoretical analysis.


#/--
##################################################################
# name: Sheu, Bonnie
# assignment: Chapter 4 Lab Exercise
# date: 
# question: 1
# subquestion: d
# other files: Lab4_1d_Graph_BonnieSheu.pdf
##################################################################
--

# Solve equation for I (fraction of infected) over 365 days
# using Forward Euler method, with different parameter values:
#
# Let  N = 1 = I + S
#      beta = 0.1 / day
#      gamma = 0.2 / day
#     
#      Start with initial value I_0 = 0.01
#      Vary time step (delta_t) from 0.1 days to 10 days


I_dot <- function (I, N=1, beta=0.1, gamma=0.2) {
	return(beta * I * (N - I) - gamma * I)
}

Forward <- function(I_dot, delta_t, tRange=c(0,365), I_0=0.01) {
	t_vals <- seq(from=tRange[1], to=tRange[2], by=delta_t);
	I_vals <- t_vals;
	I_vals[1] <- I_0;
	for (ii in 2:length(t_vals)) {
		I_vals[ii] <- I_vals[ii-1] + delta_t * I_dot(I=I_vals[ii-1])
		}
	result <- list(t=t_vals, I=I_vals);
}

tSteps <- c(0.1, 0.2, 0.4, 0.6, 0.8, 1, 2, 4, 6, 8, 10);
colors <- rainbow(n=length(tSteps));

plot(c(0, 50), c(-0.02, 0.02), 
     type='n', 
     bty='n',
     fg=grey(0.6),
     main='Fraction of infected individuals over 1 year',
     xlab='time (t) in days',
     ylab='fraction of infected individuals in population (I)'
);
     
abline(v=0);
abline(h=0);

for (tt in 1:length(tSteps)) {
	res <- Forward(I_dot, delta_t=tSteps[tt]);
	lines(res$t, res$I, col=colors[tt]);
}

legend(c('0.1', '0.2', '0.4', '0.6', '0.8', '1', '2', '4', '6', '8', '10'),
       x=70,
       y=0.45,
       lwd=2,
       col=colors,
       title='time step (delta_t)',
       bg='white'
)


# Note: The window (xRange, yRange) for the generated graph is
# different from the previous graph for part 1c. Color coding
# for the varying time steps is the same.
#
# Again, we see that the instability of the Forward Euler method
# becomes apparent for the graphs after the dark blue line. So up 
# until about time step (delta_t) = 4 or 6, the numerical solution
# still behaves similarly to the theoretical prediction, but not 
# at a greater time step.


#/--
##################################################################
# name: Sheu, Bonnie
# assignment: Chapter 4 Lab Exercise
# date: 
# question: 2
# subquestion: a
# other files: 
##################################################################
--

#     S + R ---_k+_---> C ---_k-_---> S + R
#
# An ODE model for the receptor concentration (R) is:
#
# R_dot = -(k+ * S * R) + (k- * (R_0 - R))
#       = (k- * R_0) - (R * (k+ * S + k-))
#
# where k+ = binding rate,
#       k- = dissociation rate, 
#       C = complex concentration,
#       R = recepter concentration,
#       S = signaling molecule concentration (a constant)


#/--
##################################################################
# name: Sheu, Bonnie
# assignment: Chapter 4 Lab Exercise
# date: 
# question: 2
# subquestion: b
# other files: 
##################################################################
--

# Analyze ODE model:
# 
#  1) Find equilibria (fixed points)
#
#              Let R_dot = (k- * R_0) - (R * (k+ * S + k-))
#                        = f(R, t)
#                        = 0
#     
#      R * (k+ * S + k-) = k- * R_0
#                      R = (k- * R_0) / (k+ * S + k-)
#   
#     There is one fixed point R* = (k- * R_0) / (k+ * S + k-).
#
#  2) Fixed point stability 
#
#               f(R, t) = (k- * R_0) - (R * (k+ * S + k-))
#
#              f'(R, t) = - (k+ * S + k-)
#
#             f'(R*, t) = - (k+ * S + k-)
#                       = - (R+ * S) - R-
#                       < 0
#                    k- < - (k+ * S)
#
#     Fixed point R* = (k- * R_0) / (k+ * S + k-)...
# 
#            is stable       if k- < - (k+ * S), and
#            is unstable     if k- > - (k+ * S).


#/--
##################################################################
# name: Sheu, Bonnie
# assignment: Chapter 4 Lab Exercise
# date: 
# question: 2
# subquestion: c
# other files: Lab4_2c_BonnieSheu.pdf
##################################################################
--

# See attached file.


#/--
##################################################################
# name: Sheu, Bonnie
# assignment: Chapter 4 Lab Exercise
# date: 
# question: 2
# subquestion: d
# other files: Lab4_2d_Graph_BonnieSheu.pdf
##################################################################
--

# Let S = 1e-2
#     k+ = k1 = 1e-2    ## To prevent syntax error 
#     k- = k2 = 1e-4    ## To prevent syntax error
#     R_0 = 1e-2
#     C_0 = 0


tSteps <- c(1, 5, 10, 50, 100, 500, 1000, 5000, 10000)
R_dot <- function(R, S=1e-2, R_0=1e-2, k1=1e-2, k2=1e-4) {
	return((-k1 * S * R) + k2 * (R_0 - R));
}

Forward <- function(R_dot, tRange=c(0, 1e6), timeStep=100, R_0=1e-2) {
	t_vals <- seq(from=tRange[1], to=tRange[2], by=timeStep);
	R_vals <- numeric(length=length(t_vals));
	R_vals[1] <- R_0;
	for (ii in 1:length(t_vals)) {
		R_vals[ii+1] <- R_vals[ii] + R_dot(R=R_vals[ii] * timeStep);
		}
	return(list(t=t_vals, R=R_vals));
}

result <- list();
colors <- rainbow(n=length(tSteps), start=0.1, end=0.7);

plot(c(0, 2e5), c(-0.01, 0.01), 
     fg=grey(0.6), 
     bty='n', 
     type='n', 
     xlab='t', 
     ylab='R');

abline(v=0);
abline(h=0);

for (ii in 1:length(tSteps)) {
	result[[ii]] <- Forward(R_dot, timeStep=tSteps[ii]);
	lines(result[[ii]]$t, result[[ii]]$R[-1], col=colors[ii]);
}

legend(12e4, y=0.001, 
       bg='white', 
       lwd=2, 
       title='step size', 
       col=colors, 
       legend=paste('delta t =', as.character(tSteps))
)

#/--
##################################################################
# name: Sheu, Bonnie
# assignment: Chapter 4 Lab Exercise
# date: 
# question: 2
# subquestion: e
# other files: Lab4_2e_Graph_BonnieSheu.pdf
##################################################################
--

Analytic <- function (t, S=1e-2, R_0=1e-2, k1=1e-2, k2=1e-4) {
	Cons <- (R_0 * k2 - R_0 * (k1 * S + k2)) / (k2 + k1 * S);
	R_vals <- (R_0 * k2) / (k1 * S + k2) - Cons * exp(-t * (k2 + k1 * S));
	return (R_vals);
}

tSteps <- c(5, 10, 50, 100, 500, 1000, 5000);

errors <- numeric(length=length(tSteps));

for (ii in 1:length(tSteps)) {
	est <- Forward(R_dot, timeStep=tSteps[ii]);
	analyticVals <- Analytic(t=est$t);
	totalError <- abs(est$R[-1] - analyticVals)[length(analyticVals)] / length(analyticVals);
	errors[ii] <- totalError[length(totalError)];
}

plot(tSteps, errors, 
	type='l', 
	log=c('xy'), 
	bty='n', 
	fg=grey(0.6), 
	xlab='delta t', 
	ylab='mean total log error'
)

points(tSteps, errors, pch=15, col='blue');

slope <- mean(diff(log(errors)) / diff(log(tSteps)));

text(x=500, y=5e-8,
     paste('Slope m=', as.character(slope), sep='')
)


#/--
##################################################################
# name: Sheu, Bonnie
# assignment: Chapter 4 Lab Exercise
# date: 
# question: 2
# subquestion: f
# other files: Lab4_2f_Graph_BonnieSheu.pdf
##################################################################
--

# Numerical scheme of Backward Euler:
#
#    R(n+1) = R(n) + delta_t*f(R(n+1), t(n+1))
#           = R(n) + delta_t*(-R(n+1)*(k1*S + k2) + k2*R_0)
#  
#    R(n+1)*(1 + delta_t*(k1*S + k2)) = R(n) + delta_t*k2*R_0
# 
# Thus, 
#
#    R(n+1) = (R(n) + delta_t*k2*R_0) / (1 + delta_t*(k1*S + k2))


BackwardStep <- function (R, stepSize, S=1e-2, R_0=1e-2, k1=1e-2, k2=1e-4) {
	return ((R_0*stepSize*k2 + R) / 
	(S*stepSize*k1 + stepSize*k2 + 1))
}

BackwardEuler <- function (var, tRange=c(0, 1e6), timeStep=100, R_0=1e-2) {
	t_vals <- seq(from=tRange[1], to=tRange[2], by=timeStep);
	R_vals <- numeric(length=length(t_vals));
	R_vals[1] <- R_0;
	for (ii in 1:length(t_vals)) {
		R_vals[ii+1] <- var(R=R_vals[ii], stepSize=timeStep);
		}
	return(list(t=t_vals, R=R_vals));
}

result <- list();

plot (c(0, 1e5), c(0, 0.02), 
      type='n',
      xlim=c(0, 2e5),
      bty='n',
      fg=grey(0.6),
      xlab='t',
      ylab='R'
)

abline(v=0);
abline(h=0);

tSteps <- c(5, 10, 50, 100, 500, 1000, 2000, 5000);

colors <- rainbow(n=length(tSteps), start=0.1, end=0.7);

for (ii in 1:length(tSteps)) {
	result[[ii]] <- BackwardEuler(BackwardStep, timeStep=tSteps[ii]);
	lines(result[[ii]]$t, result[[ii]]$R[-1], col=colors[ii]);
}

legend(x=12e4, y=0.018,
       bg='white',
       lwd=2,
       title='step size',
       col=colors,
       legend=paste('delta t =', as.character(tSteps))
)


#/--
##################################################################
# name: Sheu, Bonnie
# assignment: Chapter 4 Lab Exercise
# date: 
# question: 2
# subquestion: g
# other files: Lab4_2g_Graph_BonnieSheu.pdf
##################################################################
--

Analytic <- function (t, S=1e-2, R_0=1e-2, k1=1e-2, k2=1e-4) {
	Cons <- (R_0 * k2 - R_0 * (k1 * S + k2)) / (k2 + k1 * S);
	R_vals <- (R_0 * k2) / (k1 * S + k2) - Cons * exp(-t * (k2 + k1 * S));
	return (R_vals);
}

tSteps <- c(10, 50, 500, 1000, 5000);

errors <- numeric(length=length(tSteps));

for (ii in 1:length(tSteps)) {
	est <- BackwardEuler(BackwardStep, timeStep=tSteps[ii]);
	analyticVals <- Analytic(t=est$t);
	totalError <- abs(est$R[-1] - analyticVals)[length(analyticVals)] / length(analyticVals);
	errors[ii] <- totalError[length(totalError)];
}

plot(tSteps, errors, 
	type='l', 
	log=c('xy'), 
	bty='n', 
	fg=grey(0.6), 
	xlab='delta t', 
	ylab='mean total log error'
)

points(tSteps, errors, pch=15, col='blue');

slope <- mean(diff(log(errors)) / diff(log(tSteps)));

text(x=500, y=5e-8,
     paste('Slope m=', as.character(slope), sep='')
)


#/--
##################################################################