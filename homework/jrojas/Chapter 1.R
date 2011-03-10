##############################
# Chapter 1 Homework
# Jose Rojas
##############################

########## Excercise 1 ############

 # Takes a string representing a polynomial and returns a string
 # representing its derrivative.
 # 
 # source parsePolynomial.R to run

source('parsePolynomial.R')

derrivative <- function(polynomialString) {
	 # Converst list elements to vectors for easy manipulation
	exp <- parsePolynomial (polynomialString)$exponents
	coe <- parsePolynomial (polynomialString)$coefficients


	for( i in 1:length(exp) ){
		coe[i]	<- coe[i] * exp[i]

		 # Shortens vectors if a coefficient is equal to 0
		if (coe[i] == 0){
			coe <- coe[1:length(coe)!=i]
			exp <- exp[1:length(exp)!=i]
			break
		}

		exp[i]	<- exp[i] - 1
	}
	deparsePolynomial(coe,exp)
}

 # Example #

derrivative( "3x^9+4x^2+3x+1" )
 # returns
 # [1] "27x^8 + 8x + 3"


########## Excercise 2 ############


source('http://rosenbergdm.uchicago.edu/maxima_utilities.R')

 # Simple function to make a particular logistic growth function
 # ARGUMENTS desired model parmeters
 # RETURTNS  a function describing a log model with those parameters
logModelMap <- function (r, k) {
	Map <- function(x)
		x * (r - k*x)
	return( Map )
}

 # Function to make an array of the numerical solution to a map
 # ARGUMENTS Map to be 'solved', starting conditions x0
 #           number of iterations 
 # RETURTNS  The solution array an its plot
numSol <- function (Map, x0, iterations) {
	Map.x0	<- numeric(iterations)
	Map.x0[1]	<- x0
	
	for (i in 2:iterations)
		Map.x0[i] <- Map(Map.x0[i-1])
	
	plot(Map.x0)	
	Map.x0
}

 # Example #

exampleMap <- logModelMap(4,1)
numSol(exampleMap,1,100)
 # [1] 1 3 3 3 3 3 3 3 3 3
 #
 # The numSol return shows that the value stays constant at 3 which is
 # in fact a fixed point for the map:
 #	f(x) = 2x(2-x/2) = x(4-x)
 #
 #	f(3) = 3(4-3)
 #	     = 3
 #
 # I should mention that this particular model is very temperamental if
 # x0 is not set to exactly one the graph looks weird.



########## Excercise 3 ############


# My implementation of the quicksort pseudo code
qsort <- function (x){
	n = length(x)
	if (n <= 1) {
	x
	} else {
		pivot	<- x[1]
	
		less	<- x[2:n][ x[2:n] < pivot ]
		less	<- qsort(less)

		more	<- x[2:n][ x[2:n] >= pivot ]
		more	<- qsort(more)

		x	<- c(less, pivot, more)
	}
	x
}




########## Excercise 4 ############

# The efficiency of quicksort depends upon the pivots that are picked.
# If pivots happen to be chosen so that they always divide the data
# into nearly equal parts, then quicksort will be faster. The function
# will have to be called and evaluated until the halves are all shorter
# than two. So for n elements:
# Num of calls	 	1   +	2   +	4   +	...  +	n
# Data to be divided	n	n/2	n/4	...	1
#
# If, however, the pivot happens to consistently be chosen so that the
# data is divided very unevenly, the algorithm will be much slower.
# The function will have to be called and evaluated twice for every
# element in the data:
# Num of calls	 	1   +	2   +	3   +	...  +	n
# Data to be divided	n	n-1	n-2	...	1



