# Vivian Choi
# Chapter 1
# vchoi.chapter1.R

# 1.)
# parsePolynomial - a function for parsing a polynomial (in x) into a
#            vector of coefficients and a vector of exponents.
# argument - inputExpression - a polynomial in x, expressed as a string.
#            For example,   "x^3 + 2x^2 - x - 1"
# returns  - a list with two components, coefficients and exponents,
#            representing the coefficients and exponents of the polynomial,
#            respectively.
#
# Example:
#       > inputEx <- "x^3 + 2x^2 - x - 1"
#       > result <- parsePolynomial(inputEx)
#       > result[['exponents']]
#       [1] 3 2 1 0
#       > result[['coefficients']]
#       [1] 1 2 -1 -1
#
parsePolynomial <- function(inputExpression) {
  numberTerms <- nchar(gsub("[^x]", "", inputExpression));
  coefficients <- numeric(length=(numberTerms)+1);
  exponents <- numeric((length=numberTerms)+1);
  splitTerms <- strsplit(gsub('-', '+ -', inputExpression), '\\+')[[1]]
  for (ii in seq(along=splitTerms)) {
    term <- gsub(' ', '', splitTerms[ii]);
    if(gsub("[^x]", "", term) == '') {
      term <- paste(term, 'x^0', sep='');
    }
    if (gsub("([\\+|-]*)(.*)x.*", "\\2", term) == '') {
      term <- gsub('x', '1x', term);
    }
    if (gsub(".*x", "", term) == '') {
      term <- paste(term, '^1', sep='');
    }
    coef <- eval(as.integer(gsub('x.*', '', term)));
    coefficients[ii] <- coef;
    exponent <- eval(as.integer(gsub('.*\\^(.*)', '\\1', term)));
    exponents[ii] <- exponent;
  }
  o <- -(order(exponents)) + length(exponents) + 1;
  exponents <- exponents[o];
  coefficients <- coefficients[o];
  result <- list(exponents=exponents, coefficients=coefficients);
  return(result);
}


# deparsePolynomial - a function which takes a vector of coefficients and a 
#            vector of exponents and constructs a polynomial.
# arguments - coefficients - a vector of integer coefficients
#             exponents - a vector of exponents
#
# Example:
#       > exponents <- c(3, 2, 1, 0)
#       > coefficients <- c(1, 2, -1, -1)
#       > deparsePolynomial(coefficients, exponents)
#       [1] "x^3 + 2x^2 - x - 1"
#
deparsePolynomial <- function(coefficients, exponents) {
  if (length(exponents) != length(coefficients)) {
    stop('Exponent vector and coefficient vector must be of equal length.\n')
  }
  out_string <- '';
  for (ii in seq(along=exponents)) {
    out_string <- paste(out_string, ' + ', as.character(coefficients[ii]),
                        'x^', as.character(exponents[ii]), sep='');
  }
  out_string <- gsub('\\+ -', '- ', out_string);
  out_string <- gsub('1x', 'x', out_string);
  out_string <- gsub(' x\\^0', ' 1', out_string);
  out_string <- gsub('x\\^0', '', out_string);
  out_string <- gsub('^ [\\+|-] ', '', out_string);
  out_string <- gsub('x\\^1', 'x', out_string);
  return(out_string);
}

############################################################
#polyDerivative takes an input string that represents a polynomial
#and returns that polynomial's first derivative as a string using
#the funcitons parsePolynomial() and deparsePolynomial()

polyDerivative <- function(polynomial) {
	parsed = parsePolynomial(polynomial)
	#extract the two vectors from the list returned by parsePolynomial()
	exponents = parsed[['exponents']]
	coefficients = parsed[['coefficients']]
	ii = 1
	newExp <- c()
	newCoeff <- c()
	#loop over the length of the exponents vector
	#to find the new exponent, subtract one unless the exponent is 0
	#in which case keep the 0
	#to find the new coefficient, multiply the exponent and coefficient
	while(ii <= length(exponents)) {
		if(exponents[ii] == 0) {
			newExp = c(newExp, 0)
		} else {
			newExp = c(newExp, exponents[ii]-1)
		}
		newCoeff = c(newCoeff, exponents[ii]*coefficients[ii])
		ii=ii+1
	}
	return (deparsePolynomial(newCoeff, newExp))
}

# 2.)
#plots f(x) = x(2-x/2) given initial condition "init" for "k" iterations
#THE FIXED POINTS OF THIS EQUATION ARE 0 AND 2
myPlot <- function(init, k){
	x = numeric(k)
	x[1] = init
	ii = 2
	while(ii <= k) {
		x[ii] = 2*x[ii-1]-((x[ii-1]^2)*0.5)
		ii = ii+1
	}
	t = c(1:k)
	plot(t, x, type="l")
}

######################################################
#functor: myPlot2 
#arguments: 
#	fun: a function which evaluates an expression at point x
#	init: initial condition
#	k: number of iterations
#returns nothing but gives an iterated plot of the expression in
#fun() starting at init for k iterations
myPlot2 <- function(fun, init, k){
	x = numeric(k)
	x[1] = init
	ii = 2
	while(ii <= k) {
		x[ii] = fun(x[ii-1])
		ii = ii+1
	}
	t = c(1:k)
	plot(t, x, type="l")
}

myFunA <- function(x)
{
	return (-(x^2)+2*x)
}

myFunB <- function(x)
{
	return (-4*(x^2)+4*x)
}

#excercise3:
#a. the calculated fixed points are 0 and 1
#   the computationally found solution approaches 1 but not 0
#b. the calculated fixed points are 0 and 0.75
#   the solution approaches neither of them for the initial values I used
#   except in the case of init=1, in which case the solutions went to 0
