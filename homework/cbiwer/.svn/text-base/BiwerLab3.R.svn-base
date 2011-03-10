# name: Craig Biwer
# assignment: 3
# date: 10/23/2009
# filename: BiwerLab3.R
#############################################################################







#/--#########################################################################
# name: Craig Biwer
# assignment: Lab 3
# date: 10/23/2009
# question: 1
# subquestion: a
# other files:
##########################################################################/--

#Equilibrium values: N(t) = 0 and N(t) = 4
#Neither 0 nor 4 are stable fixed points. That means that there is no
#stable nonzero carrying capacity.


#/--#########################################################################
# name: Craig Biwer
# assignment: Lab 3
# date: 10/23/2009
# question: 1
# subquestion: b
# other files:
##########################################################################/--

#Equilibrium values: N(t) = 0 and N(t) = -20
#Neither 0 nor -20 are stable fixed points. Additionally, since -20 is
#negative, it doesn't make sense biologically.



#/--#########################################################################
# name: Craig Biwer
# assignment: Lab 3
# date: 10/23/2009
# question: 2
# subquestion: a
# other files:
##########################################################################/--

twoa <- function(x) {
	return (x*(5-4*x))
}

#cobwebPlot(twoa)
#Fixed points at 0 (unstable) and 1 (unstable) (intersection of plot and red line)


#/--#########################################################################
# name: Craig Biwer
# assignment: Lab 3
# date: 10/23/2009
# question: 2
# subquestion: b
# other files:
##########################################################################/--

twob <- function(x) {
	return (2*x*(1-3*x/2))
}

#cobwebPlot(twob)
#Fixed points at 0 (unstable) and 1/3 (stable) (intersection of plot and red line)


#/--#########################################################################
# name: Craig Biwer
# assignment: Lab 3
# date: 10/23/2009
# question: 2
# subquestion: c
# other files:
##########################################################################/--

twoc <- function(x) {
	return (x/2-x^2/5)
}

#cobwebPlot(twoc)
#Fixed points at 0 (stable) and -5/2 (unstable) (intersection of plot and red line)


#/--#########################################################################
# name: Craig Biwer
# assignment: Lab 3
# date: 10/23/2009
# question: 2
# subquestion: d
# other files:
##########################################################################/--

twod <- function(x) {
	return (x*(5/2 - 7*x))
}

#cobwebPlot(twod)
#Fixed points at 0 (unstable) and 3/14 (stable) (intersection of plot and red line)


#/--#########################################################################
# name: Craig Biwer
# assignment: Lab 3
# date: 10/23/2009
# question: 3
# subquestion: 
# other files:
##########################################################################/--

plotThis <- function(x) {
#	return (3 * x - 3 * x^2 / 4 + 1)
#	return (100 * x - 2 * x^2 + 0)
	return (0 - 100 * x + x^2 / 2)
}

cobwebPlot <- function(plotMe) {
	max_iter <- 50
	y_vector <- x_vector <- numeric(max_iter * 2)
	y_vector[1] <- 0
	x_vector[1] <- 0.2
	for(ii in 1:50) {
		y_vector[2 * ii] <- do.call(plotMe, list(x=x_vector[2 * ii - 1]))
		y_vector[2 * ii + 1] <- y_vector[2 * ii]
		x_vector[2 * ii] <- x_vector[2 * ii - 1]
		x_vector[2 * ii + 1] <- y_vector[2 * ii + 1]
	}

	curve(
		plotMe,
		0,
		1,
		xlab = 'x',
		ylab = 'f(x)',
		main = 'Lab 3 Exercise 3',
		type = 'l'
	)
	lines(-10:10, -10:10, col='red')
	lines(x_vector, y_vector, col='blue')
}




#/--#########################################################################
# name: Craig Biwer
# assignment: Lab 3
# date: 10/23/2009
# question: 4
# subquestion: a
# other files:
##########################################################################/--

#G' = -kG
#Type: Linear


#/--#########################################################################
# name: Craig Biwer
# assignment: Lab 3
# date: 10/23/2009
# question: 4
# subquestion: b
# other files:
##########################################################################/--

#G(t) = G(0) * exp^(-kt)


#/--#########################################################################
# name: Craig Biwer
# assignment: Lab 3
# date: 10/23/2009
# question: 4
# subquestion: c
# other files:
##########################################################################/--

#curve(100*exp(-0.01*x), 0, 400, xlab = 't', ylab = 'G(0) * exp^(-kt)', main = 'Lab 3 Exercise 4c', type = 'l')


#/--#########################################################################
# name: Craig Biwer
# assignment: Lab 3
# date: 10/23/2009
# question: 4
# subquestion: d
# other files:
##########################################################################/--

#The equilibrium concentration occurs for a glucose level of 0. At this
#point, there is no more glucose in the blood so no more can be absorbed.
#This equilibrium point is stable.


#/--#########################################################################
# name: Craig Biwer
# assignment: Lab 3
# date: 10/23/2009
# question: 5
# subquestion: a
# other files:
##########################################################################/--

#G' = -kG + a
#Type: Linear


#/--#########################################################################
# name: Craig Biwer
# assignment: Lab 3
# date: 10/23/2009
# question: 5
# subquestion: b
# other files:
##########################################################################/--

#G(t) = G(0) * exp^(-kt) + a/k


#/--#########################################################################
# name: Craig Biwer
# assignment: Lab 3
# date: 10/23/2009
# question: 5
# subquestion: c
# other files:
##########################################################################/--

#curve(-300*exp(-0.01*x) + 4/.01, 0, 400, xlab = 't', ylab = 'A * exp^(-kt) + a/k', main = 'Lab 3 Exercise 5c', type = 'l')


#/--#########################################################################
# name: Craig Biwer
# assignment: Lab 3
# date: 10/23/2009
# question: 5
# subquestion: d
# other files:
##########################################################################/--

#The equilibrium concentration occurs for a glucose level of 400. At this
#point, there is no more glucose in the blood so no more can be absorbed.
#This equilibrium point is stable.