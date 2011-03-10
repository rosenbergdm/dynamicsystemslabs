# name: Christina Baer
# assignment: Ch 3
# date: 10//09
# filename: Bios Lab Ch 3.R
#############################################################################

#/--#########################################################################
# name: Christina Baer
# assignment: Ch 3
# date: 10//09
# question: 1
# subquestion: a
# other files: 
##########################################################################/--
# f(N) = r * N * ((N / U) - 1) * (1 - (N / K))
# f(N) = ((r * N^2 / U) - r * N) * (1 - (N / K))
# f(N) = (r * N^2 / U) - (r * N^3 / U / K) - r * N + (r * N^2 / K)

# f'(N)= [(2 * r * N / U) - (3 * r * N^2 / U / K) - r + (2 * r * N / K)] * dx 
# f'(N) = [(-3 * r * N^2 / U / K) + 2 * r * (1 / U + 1 / K) * N - r] * dx

# Fixed points when f(N) = 0.
# 0 = r * N * ((N / U) -1) * (1 - (N / K))
# Set each factor equal to 0:
	# 0 = r * N
	# N* = 0

	# 0 = N / U - 1
	# 1 = N / U
	# N* = U

	# 0 = 1 - N / K
	# N / K = 1
	# N* = K

# The fixed points are N = 0, U, K
# f'(0) = -r, which is less than 0 and therefore stable, if r, K, and U > 0.

# f'(U) = (2 * r / U) * U - (3 * r / U / K) * U^2 - r + (2 * r / K) * U
# f'(U) = 2 * r - (3 * r / K) * U - r + (2 * r / K) * U
# f'(U) = r - (r / K) U
# To determine stablity, set this expression to be less than 0.
# r - r / K * U < 0
# r < r / K * U
# 1 < 1 / K * U
# K < U
# If K < U, then fixed point f(U) is stable.

# f'(K) = (2 * r / U) * K - (3 * r / U / K) * K^2 - r + (2 * r / K) * K
# f'(K) = (2 * r / U) * K - (3 * r / U) * K - r + 2 * r
# f'(K) = -r / U * K + r
# To determine stablity, set this expression to be less than 0.
# r - r / U * K < 0
# r < r / U * K
# U < K
# If U < K, then the fixed point f(K) is stable.

# For r = 10, K = 200, and U = 20, therefore, N* = 0 and N* = K are stable, but 
# N* = U is not.

# g(N)= r * N^2 - r * N^3 / K
# g'(N)= [2 * r * N - (3 * r / K) * N^2] * dx

# Fixed points when g(N) equals 0.
# 0 = r * N^2 - r * N^3 / K
# r / K * N^3 = r * N^2
# N = K

# g'(0) = 2 * r * 0 - (3 * r / K) * 0^2
# g'(0) = 0, so stability is uncertain.

# g'(K) = 2 * r * K - (3 * r / K) * K^2
# g'(K) = 2 * r * K - 3 * r * K
# g'(K) = -K, which is less than 0 and stable, since K > 0.

# For r = 10 and K = 200, N* = 0 and N* = -200.


#/--#########################################################################
# name: Christina Baer
# assignment: Ch 3
# date: 10//09
# question: 1
# subquestion: b
# other files: 
##########################################################################/--
r<- 10
K<- 200
U<- 20

# Plot for f(N)
f<- function(N) 
{
return(10 * N * ((N / 20) - 1) * (1 - (N / 200)))
}

model2<-function(logdiff, title='', plotname='', initial, max) 
{
modelb<-function(N_at_t) 
{
N_at_t1<-logdiff(N_at_t)
return(N_at_t1)
}

init_value <-initial
max_iter <-max
x_array<-numeric(length=max_iter);
x_array[1]<-init_value;
for (ii in 2:max_iter) 
{
new_value<-modelb(x_array[ii-1]);
x_array[ii]<-new_value
}
plotname<-plot(1:max_iter, x_array[1:max_iter], 
main=title, xlab='t', type='l',
ylab='x(t)')
return<-plotname
}

plotf<-model2(f, "Question 1b, f(N)", plotf, 199.9999999999999, 16)



# f(N) decays exponentially at N = 200.000000001, N = 20.00000001, 
# and N = 0.0000001.  Even the "stable" fixed points do not seem to be 
# very attractive.

# Plot for g(N)

g<- function(N) 
{
return(10 * N^2 - (r * N^3 / K))
}

plotg<-model2(g, "Question 1b, g(N)", plotg, 0.9, 100)

# g(N) decays exponentially at N = -199.9999999 and goes to 0 at 
# N = -0.0000000001.

#/--#########################################################################
# name: Christina Baer
# assignment: Ch 3
# date: 10//09
# question: 1
# subquestion: c
# other files: 
##########################################################################/--

# f(N) grows exponentially at values near the fixed points,
# while g(N) either decays or stabilizes at zero. The difference between the 
# two must be explained by the (N / U - 1) term in f(N).  The effect of the 
# term is that as N approaches 0, N is converted to a negative term with a 
# larger absolute value, instead of N^2, in g(N), which would continue to 
# decrease, sending g(N) towards 0.  Biologically, this difference could 
# represent some sort of increased population growth rate at extremely low 
# densities.

#/--#########################################################################
# name: Christina Baer
# assignment: Ch 3
# date: 10//09
# question: 2
# subquestion: a
# other files: 
##########################################################################/--
# dN / dt = r * N - sin(4 * t / (2 * pi))

#/--#########################################################################
# name: Christina Baer
# assignment: Ch 3
# date: 10//09
# question: 2
# subquestion: b
# other files: 
##########################################################################/--
# dN / dt = (1 + r) * N - sin(2 * t / pi) = f(N)
# f'(N) = 1 + r

# f(N*) = 0
# 0 = (1 + r) * N - sin(2 * t / pi)
# sin(2 * t / pi) = (1 + r) * N
# N = sin(2 * t / pi) / (1 + r)
# 

# There are fixed points for each t, the first two being t = 0, N = 0 and
# t = 1, N = sin(2 / pi) / (1 + r).  Only the fixed point at t = 0, N = 0
# is stable; all the others go to exponential growth.

#/--#########################################################################
# name: Christina Baer
# assignment: Ch 3
# date: 10//09
# question: 2
# subquestion: c
# other files: BiosLabCh3-CSB-2c.pdf
##########################################################################/--
# An ODE for bacteria population with variables for population size (N) and 
# time (t).

bacpop<-function (N, t) 
{
return( (1 + 1) * N - sin((4 * t) / (2 * pi) ) )
}

tRange<-c(0, 10)
dt<- 0.5
NRange<-c(-20, 1000)
dN<- 50
h<- 0.7
k<- 0.5
tSteps<- diff(range(tRange)) / dt
NSteps<- diff(range(NRange)) / dN

plot(c(0, 9), c(-10, 900), type='n', main="Bacterial Growth Directional Field",
sub="Christina Baer, Ch 3, 2c", xlab="t", xaxs='i', ylab= "N", yaxs='i')

for (ii in 0:(tSteps-1)) 
{
	for (jj in 0:(NSteps-1)) 
{
		t<- ii * dt
		N<- jj * dN
		m<- bacpop(N, t)
		u<- t + (h * dt)
		v<- N + (k* h* dt) * m
		arrows ( t, N, u, v, code=2, length=0.05)
}
}

# PDF copy of plot
pdf(file="BiosLabCh3-CSB-2c.pdf")
tRange<-c(0, 10)
dt<- 0.5
NRange<-c(-20, 1000)
dN<- 50
h<- 0.7
k<- 0.5
tSteps<- diff(range(tRange)) / dt
NSteps<- diff(range(NRange)) / dN

plot(c(0, 9), c(-10, 900), type='n', main="Bacterial Growth Directional Field",
sub="Christina Baer, Ch 3, 2c", xlab="t", xaxs='i', ylab= "N", yaxs='i')

for (ii in 0:(tSteps-1)) 
{
	for (jj in 0:(NSteps-1)) 
{
		t<- ii * dt
		N<- jj * dN
		m<- bacpop(N, t)
		u<- t + (h * dt)
		v<- N + (k* h* dt) * m
		arrows ( t, N, u, v, code=2, length=0.05)
}
}
dev.off()

# The fixed point at 0 is indicated by the flat arrows along the x-axis.
# The exponential growth is indicated by the arrows approaching vertical 
# alignment.

#/--#########################################################################
# name: Christina Baer
# assignment: Ch 3
# date: 10//09
# question: 2
# subquestion: d
# other files: BiosLabCh3-CSB-2d.pdf
##########################################################################/--
# dN / dt = (1 + r) * N - sin(4 * t / (2 * pi))
# Used SAGE workbook:
#     var('N t r') evaluate
#         (N, t)(N, t)
# 
#     f(N, t) = (1 + r) * N - sin(2 * t / pi)  evaluate
#
#     latex(f.integrate(N).integrate(N)(N,t)) evaluate
#           \frac{1}{6} \, {(r + 1)} N^{3} - \frac{1}{2} \, N^{2} 
#             \sin\left(2 \, \frac{t}{\pi}\right) 
#
#     f.integrate(N) evaluate
#         (N, t) |--> 1/2*(r + 1)*N^2 - N*sin(2*t/pi)

# N = 1 / 2 * (r + 1) * N^2 - N * sin(2 * t / pi)
# 1 = (r + 1) / 2 * N - sin(2 * t / pi)
# 1 + sin(2 * t / pi) = (r + 1) / 2 * N
# N = 2 / (r + 1) (1 + sin(2 * t / pi)) + N_0

ode2<- function(t, r, N_0) 
{
return (2 / (r + 1) * (1 + sin(2 * t / pi)) + N_0)
}

ode2(0, 1, 1)

plot(ode2(1:50, 1, 0), main="Solution Curve 1 for 
Bacteria Populations", sub="Christina Baer, Ch 3, 2d", 
xlab="t", ylab="Number of Bacteria", type='l')

plot(ode2(1:50, 1, 1), main="Solution Curve 1 for 
Bacteria Populations", sub="Christina Baer, Ch 3, 2d", 
xlab="t", ylab="Number of Bacteria", type='l')

plot(ode2(1:50, 1, 10), main="Solution Curve 1 for 
Bacteria Populations", sub="Christina Baer, Ch 3, 2d", 
xlab="t", ylab="Number of Bacteria", type='l')

# PDF copy of plots
pdf(file="BiosLabCh3-CSB-2d.pdf")
plot(ode2(1:50, 1, 0), main="Solution Curve 1 for 
Bacteria Populations", sub="Christina Baer, Ch 3, 2d", 
xlab="t", ylab="Number of Bacteria", type='l')

plot(ode2(1:50, 1, 1), main="Solution Curve 1 for 
Bacteria Populations", sub="Christina Baer, Ch 3, 2d", 
xlab="t", ylab="Number of Bacteria", type='l')

plot(ode2(1:50, 1, 10), main="Solution Curve 1 for 
Bacteria Populations", sub="Christina Baer, Ch 3, 2d", 
xlab="t", ylab="Number of Bacteria", type='l')
dev.off()


#/--#########################################################################
# name: Christina Baer
# assignment: Ch 3
# date: 10//09
# question: 3
# subquestion: a
# other files: 
##########################################################################/--
# f(V) = -(20 / 3) * (V - 58.1) - (2000 / 3) * (V + 69.6)
# f(V) = -(2020 / 3) * V - (138038 / 3)
# Fixed points are when f(V) = 0.  There is one fixed point when 
#  -(2020 / 3) * V = 138038 / 3, in other words, when V = -68.3356.
# f'(V) = [-(2020 / 3)] dx
# Since f'(V) is a negative constant, V* is stable.

#/--#########################################################################
# name: Christina Baer
# assignment: Ch 3
# date: 10//09
# question: 3
# subquestion: b
# other files: BiosLabCh3-CSB-3b.pdf
##########################################################################/--
V<-c()
membrane<- function(V, t) 
{
return( -(2020 / 3) * V - (138038 / 3))
}

plotmem<-model2(membrane, "Question 3b", plotmem, -68.33560000000001, 10)


tRange<-c(0, 40)
dt<- 1
VRange<-c(-1000, 1000)
dV<- 100
h<- 0.7
k<- 0.5
tSteps<- diff(range(tRange)) / dt
VSteps<- diff(range(VRange)) / dV

plot(c(0, 9), c(-200, 100), type='n', main="Membrane Voltage Equilibration",
sub="Christina Baer, Ch 3, 3b", xlab="t", xaxs='i', ylab= "V", yaxs='i')

for (ii in 0:(tSteps-1)) 
{	
	for (jj in 0:(VSteps-1)) 
{
		t<- ii * dt
		V<- jj * dV
		m<- membrane(V)
		u<- t + (h * dt)
		v<- V + (k * h * dt) * m
		arrows ( t, V, u, v, code=2, length=0.01)
}
}

# PDF copy of plots
pdf(file="BiosLabCh3-CSB-3b.pdf")
tRange<-c(0, 40)
dt<- 1
VRange<-c(-1000, 1000)
dV<- 100
h<- 0.7
k<- 0.5
tSteps<- diff(range(tRange)) / dt
VSteps<- diff(range(VRange)) / dV

plot(c(0, 9), c(-200, 100), type='n', main="Membrane Voltage Equilibration",
sub="Christina Baer, Ch 3, 3b", xlab="t", xaxs='i', ylab= "V", yaxs='i')

for (ii in 0:(tSteps-1)) 
{	
	for (jj in 0:(VSteps-1)) 
{
		t<- ii * dt
		V<- jj * dV
		m<- membrane(V)
		u<- t + (h * dt)
		v<- V + (k * h * dt) * m
		arrows ( t, V, u, v, code=2, length=0.01)
}
}
dev.off()

# I'm not quite sure what's wrong with the arrows, the slope field should be 
# showing slopes quickly heading towards -68 (the fixed point) and stabilizing
# there.

#/--#########################################################################
# name: Christina Baer
# assignment: Ch 3
# date: 10//09
# question: 3
# subquestion: c
# other files: BiosLabCh3-CSB-3c.pdf
##########################################################################/--
# dV / dt = -2020 / 3 * V - 138038 / 3
# To solve, begin by solving dV / dt = -2020 / 3 * V
	# S dV / V = S -2020 / 3 * dt
	# ln |V| = -2020 / 3 * t + C
	# V = e^(-2020 / 3 * t + C)
	# V = A * e^(-2020 / 3 * t)
	
	# V(0) = V_(0)
	# V(0) = A * e^0
	# V(t) = V_(0) * e^(-2020 / 3 * t)

# Next, solve the inhomogeneous part, dV / dt = -138038 / 3
	# Integration factor: e^(2020 / 3 * t)
	# Sd[e^(2020 / 3 * t) * V] / dt =S[-138038 / 3 * e^(2020 / 3 * t)] * dt
	# e^(2020 / 3 * t) * V = -69019 / 1010 * e^(2020 / 3 * t) + C
	# V(t) = -69019 / 1010 + C * e^(-2020 / 3 * t) 
	
	# V(0) = V_(0)
	# V_(0) = -69019 / 1010 + C * e^0
	# C = V_(0) - 69019 / 1010
	# V(t) = -69019 / 1010 + (V_(0) - 69019 / 1010) * e^(-2020 / 3 * t) 

voltage<- function(t, V_0) 
{
return(-69019 / 1010 + (V_0 - 69019 / 1010) * exp(-2020 / 3 * t))
}

plot(voltage(0:4, 0), type='l', main="Solution Curve 1 for 
Membrane Reversal Equilibria", sub="Christina Baer, Ch 3, 3c", 
xlab="t", ylab="Voltage")

plot(voltage(0:4, -500), type='l', main="Solution Curve 2 for 
Membrane Reversal Equilibria", sub="Christina Baer, Ch 3, 3c", 
xlab="t", ylab="Voltage")

plot(voltage(0:4, 500), type='l', main="Solution Curve 3 for 
Membrane Reversal Equilibria", sub="Christina Baer, Ch 3, 3c", 
xlab="t", ylab="Voltage")

# PDF copy of plots
pdf(file="BiosLabCh3-CSB-3c.pdf")
plot(voltage(0:4, 0), type='l', main="Solution Curve 1 for 
Membrane Reversal Equilibria", sub="Christina Baer, Ch 3, 3c", 
xlab="t", ylab="Voltage")

plot(voltage(0:4, -500), type='l', main="Solution Curve 2 for 
Membrane Reversal Equilibria", sub="Christina Baer, Ch 3, 3c", 
xlab="t", ylab="Voltage")

plot(voltage(0:4, 500), type='l', main="Solution Curve 3 for 
Membrane Reversal Equilibria", sub="Christina Baer, Ch 3, 3c", 
xlab="t", ylab="Voltage")
dev.off()

# All the solution curves stablize at the fixed point at approximately -68.

#/--#########################################################################
# name: Christina Baer
# assignment: Ch 3
# date: 10//09
# question: 4
# subquestion: a
# other files: 
##########################################################################/--
# Pseudocode for plotting function flow:

# 1. Define a function that represents dx/dt(t).
# f <-function(x) {
# return(FUNCTION)
# }

# 2. Find points where f(x) = 0 in a given range.
# minx<- ##
# maxx<- ##
# xset<-c(minx:maxx)
# fx<-c(f(xset))
# fixed<-xset[fx=0]

# 3. Plot f.
# plot(xset, fx,...)

# 4. Determine flow between each pair of fixed points and draw arrows.
# l<-c()
# r<-c()
# for(ii 1:length(fixed)) 
# {
# l[ii]<-f(fixed[ii] - 1)
# r[ii]<-f(fixed[ii] + 1)
# 	if (r[ii] > 0) 
# {
#	draw right-pointing arrow from ii to ii + 1
# }
#	else if (r[ii] < 0)
# {
#	draw left-pointing arrow from ii + 1 to ii
# }
#     if first or last point 
# {
#     draw arrow from min/max to/from first/last point
# }
# }

#/--#########################################################################
# name: Christina Baer
# assignment: 4
# date: 10//09
# question: 4
# subquestion: b
# other files: BiosLabCh3-CSB-4b.pdf
##########################################################################/--
# PDF copy of plot
pdf(file="BiosLabCh3-CSB-4b.pdf")

# 1. Define a function that represents dx/dt(t).
f <-function(x) 
{
return(x / 90 * (90 - x))
}

# 2. Find points where f(x) = 0 in a given range.
minx<- -10
maxx<- 100
xset<-c(minx:maxx)
fx<-c(f(xset))
fp<- fx == 0
fixed<-c(xset[fp[TRUE]])
fixed

# 3. Plot f.
plot(xset, fx, type='l', main="Flow Line Diagram for f(x)", 
sub="Christina Baer, Ch 3, 4b", xlab="t", ylab="f(x)")
abline(h=0, v=0)

# 4. Determine flow between each pair of fixed points and draw arrows.
l<-c()
r<-c()

# This finds whether f(x) is above or below the x-axis on either side of each
# fixed point.

for(ii in 1:length(fixed)) 
{
l[ii]<-f(fixed[ii] - 1)
r[ii]<-f(fixed[ii] + 1)
}

# This draws the arrow between the left margin and the first fixed point.

if (l[1] > 0) 
{
arrows(minx, 3, (fixed[1] - 1), 3)
} else if (l[1] < 0) 
{
arrows((fixed[ii] - 1), 3, minx, 3)
}

# This draws the arrow between the last fixed point and the right margin.

if(r[length(fixed)] > 0) 
{
arrows((fixed[length(fixed)] + 1), 3, maxx, 3)
} else if (r[length(fixed)] < 0) 
{
arrows(maxx, 3, (fixed[length(fixed)] + 1), 3)
}

# If there is more than one fixed point, this draws arrows between each pair
# of fixed points.

if (length(fixed) > 1) 
{
	for (ii in 2:(length(fixed) - 1)) 
{
		if (r[ii] > 0) 
{
		arrows((fixed[ii] + 5), 3, (fixed[ii + 1] - 5), 3) 
} 		else if (r[ii] < 0) 
{
		arrows((fixed[ii + 1] - 5), 3, (fixed[ii] + 5), 3) 		
}
}
}
