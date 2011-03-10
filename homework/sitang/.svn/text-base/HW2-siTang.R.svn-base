# Exercise 1
#
# Si Tang, 396904
# Email: sugar@uchicago.edu
#
# -----------------------------------------------------------------
#
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
#   Change "as.integer()" to "as.numeric()", so that the function can handle polynomial with non-integer coefficients
#   coef <- eval(as.integer(gsub('x.*', '', term)));
    coef <- eval(as.numeric(gsub('x.*', '', term)));
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

# Not all of the '1x's need to be replaced by 'x'.
# out_string <- gsub('1x', 'x', out_string);
  out_string <- gsub(' x\\^0', ' 1', out_string);
  out_string <- gsub('x\\^0', '', out_string);

# "-" at the beginning of the polynomial should not be removed.
# out_string <- gsub('^ [\\+|-] ', '', out_string);
  out_string <- gsub('^ [\\+] ', '', out_string);
  out_string <- gsub('x\\^1', 'x', out_string);

  cat("The 1st derivative is:", out_string, "\n");

  return(out_string);
}

#
#----------------------------------------------------------------------------------
# Exercise 1

# "sortExponents" is a function to re-arrange the Polynomial with descending order of x
# For example if the input polynomial is "x^5 - x^7 + x^2 - 4x^4"
# Then after parsePolynomial("x^5-x^7+x^2-4x^4"), we have the result:
#
# > parsePolynomial("x^5-x^7+x^2-4x^4") 
# $exponents
# [1] 5 2 7 0 4
#
# $coefficients
# [1]  1  1 -1  0 -4
#
# After we sort the input Polynomial, we got: 
# > result <- sortExponents(parsePolynomial("x^5-x^7+x^2-4x^4"))
# $exponents
# [1] 7 5 4 2 0
# $coefficients
# [1] -1  1 -4  1  0
#
# Then we can check the Polynomial after re-arranging.
# (Note that there is a tiny modification of function "deparsePolynomial", 
# see the line being commented out in function "deparsePolynomial" above )
#
# > result <- sortExponents(parsePolynomial("x^5-x^7+x^2-4x^4"))
# > deparsePolynomial(result[["coefficients"]], result[["exponents"]]);
# The 1st derivative is:  - x^7 + x^5 - 4x^4 + x^2 + 0 


sortExponents <- function (result) {
  x <- result[["coefficients"]];
  y <- result[["exponents"]];

  for (i in c(1: (length(y))  ) ){
     for (j in c(i: (length(y)) ) ) {
         if ( y[i] < y[j] ) {
		tmp <- y[j];
		y[j] <- y[i];
		y[i] <- tmp;


		tmp <- x[j];
		x[j] <- x[i];
		x[i] <- tmp;
	 } else{}
     }
  }
     
  result <- list(exponents=y, coefficients=x);  
  return (result);
}



# Funtion Name: firstDerivative
# Argument of the function: An expression of polynomial
# Use: Calculate the first derivative of the input polynomial

# Declare the function to calculate the 1st Derivative
firstDerivative <- function (input_Polynomial) {
  result1 <- parsePolynomial(input_Polynomial);

  result2 <- sortExponents(result1);

  a<-result2[["coefficients"]]; # coefficients of input Polynomial
  a1 <- c(1: (length(a)-1) ); # coefficients of output Polynomial
  b <- result2[["exponents"]]; # exponents of input Polynomial
  b1 <- c(1: (length(b)-1) ); # exponents of output Polynomial 

  # Calculate the first derivative
  for( i in c(1:(length(a) - 1) )  ) {
    a1[i] <- a[i] * b[i];
    b1[i] <- b[i]-1;
  }
  # Display the first derivative
  return (deparsePolynomial(a1, b1)) ;
 
}
#
# An interactive scripts for calculating first derivative.
cat("\n","Enter a Polynomial like \"x^3 + 2x^2 - x - 1\":","\n") # prompt
input_Poly <- readline();

# Call the function "firstDerivative" to calculate the first derivative of the input polynomial.
output_Poly <- firstDerivative(input_Poly);



# Exercise 2(a)
#
# Si Tang, 396904
# Email: sugar@uchicago.edu
#
# Define the fuction of the logistic model in Exercise 2(a)
logiModel_2a <- function (x) {
	y <- 2 * x * ( 2 - x / 2 );
	return(y);
}


# Get the initial value for iteration
cat ("Please input an initial value: \n");
init_value <- scan (n=1);

# Get the maximum times for iteration
cat ("Please specify a maximum times of iteration : \n");
max_iter <- scan(n=1);

# x_array is a variable to store the X value in each iteration
x_array <- numeric (length = (max_iter+1));
x_array[1] <- init_value;

# Trying to find the Fixed Points, by solving the difference equation in R
# You can also read the fixed point from the figures, or by solving the difference equation on paper
#
# First, declare fucntion of the difference equation based on the 
# function of the logistic model
diff_2a <-function (x) {

	y<-logiModel_2a(x)-x;
	return (y);
}

# It is applicable, as long as you know there are only two fixed points and one of the them is zero.
root1 <- uniroot(diff_2a, c(0, 0.0001), tol=0.0001 )
root2 <- uniroot(diff_2a, c(0.0001,1000), tol=0.0001)

root <- numeric();
root[1] <- sprintf("%.3f", root1[1]);
root[2] <- sprintf("%.3f", root2[1]);

cat ("the fixed points are x1=", root[1], "and x2=", root[2])

# Set the legend characters
legend <- sprintf('f(x)=2x(2-x/2)\nInitial Value= %.2f \nIteration=%.0f \n Fix Points: \n x1=%.3f \n x2=%.3f', init_value, max_iter, as.numeric(root[1]), as.numeric(root[2]));

#Prepare the plot area
par(mfcol=c(1,2)) ;
curve (logiModel_2a, xlim=c(0, 4), ylim=c(0, 4), ylab='x(t)', main='x(t)~x') ;
lines (c(0,4), c(0, 4), type='l', col='red');
# Set the legend of the figure
legend(-0.25,4, legend, bty='n')

# A 'for' loop for the realization of iteration.
for (i in 2:(max_iter+1)) {
	x_array[i] <- logiModel_2a (x_array[i-1])

	# Draw a line from y=x to y=f(x)= 2 * x * (2-x/2)
	lines(c(x_array[i-1], x_array[i-1]), c(x_array[i-1], x_array[i]), col='purple');
	# Draw a line from y=f(x)= 2 * x * (2-x/2) to y=x
	lines(c(x_array[i-1], x_array[i]), c(x_array[i], x_array[i]), col='blue');

}

# Plot the x value against time t, the numeric solution. 
plot(1:(max_iter+1), x_array, type='b', main='x(t) ~ t', col=3, xlab='t', ylab='x(t)')
#
#
# Save the figure as an 'eps' file
# If you would like to use it you can uncomment it.
dev.print(device=postscript, file='fig2-1.eps', horizontal=TRUE)

# Exercise 2(b)
#
# Si Tang, 396904
# Email: sugar@uchicago.edu
# 
# The body of MyFunctor is almost the same as 'HW2-2-1_SiTang.R'
# But in order to find the root of the difference equation automatically by R,
# the argument 'fx' here should be a 'difference equation function', 
#
# 	Example: x(n+1) - x(n) = f(x(n)) - x(n)
#
# not an 'iteration function'.
#
# 	Example: x(n+1) = f(x(n))
#

MyFunctor <- function(fx) {
	# Get the initial value for iteration
	cat ("Please input an initial value: \n");
	init_value <- scan (n=1);
	
	# Get the maximum times for iteration
	cat ("Please specify a maximum times of iteration : \n");
	max_iter <- scan(n=1);
	
	# x_array is a variable to store the X value in each iteration
	x_array <- numeric (length = (max_iter+1));
	x_array[1] <- init_value;
	
        # Trying to find the fixed points of the difference equation.
	# It is applicable given that one of the fixed points is 0
	# fx is the difference equation, as stated above.
        root1 <- uniroot(fx, c(0, 100), tol=0.001 )
        root2 <- uniroot(fx, c(0.0001,1000), tol=0.001)
        root <- numeric();
        root[1] <- sprintf("%.3f", root1[1]);
        root[2] <- sprintf("%.3f", root2[1]);
        cat ("the fixed points are x1=", root[1], "and x2=", root[2],"\n")
	# End of finding the fixed points of difference equation.
	
	x <- seq(from=0, to=( as.numeric(root[1])+ as.numeric(root[2])+ 1 ),length=100)
	y <- fx(x) + x;

	legend <- sprintf('Initial Value= %.2f \nIteration=%.0f \n Fixed Points: \n x1=%.3f \n x2=%.3f', init_value, max_iter, as.numeric(root[1]), as.numeric(root[2]));
	par(mfcol=c(1,2)) ;
	plot(x, y, ylab='x(t)',xlim=c(0, 2*max(as.numeric(root))), ylim=c(0,max(y)), main='x(t)~x', type='l') ;
	lines (c(0,4), c(0, 4), type='l', col='red');
	legend(0,max(y), legend, bty='n')
	

	# A 'for' loop for generating the iterated map.
	for (i in 2:(max_iter+1)) {
		x_array[i] <- fx(x_array[i-1]) + x_array[i-1]
	
		# Draw a line from y=x to y=f(x)= 2 * x * (2-x/2)
		lines(c(x_array[i-1], x_array[i-1]), c(x_array[i-1], x_array[i]), col='purple');
		# Draw a line from y=f(x)= 2 * x * (2-x/2) to y=x
		lines(c(x_array[i-1], x_array[i]), c(x_array[i], x_array[i]), col='blue');
	
	}
	
	# Plot the x value against time t, the numeric solution. 
	plot(1:(max_iter+1), x_array, type='b', main='x(t) ~ t', col=3, xlab='t', ylab='x(t)')

	# Save the figure as an 'eps' file
	# If you would like to use it you can uncomment it.
	 dev.print(device=postscript, file='fig.eps', horizontal=TRUE)
	
}

# Exercise 3
#
# Si Tang, 396904
# Email: sugar@uchicago.edu
#

qsort <- function (x)
{

	if (length(x)==0) {
		return;
	}else if (length(x)==1) { 
		return (x);
	} else {

	qhead <- numeric();
	qtail <- numeric();

	qpivot <- x[1];
	j<-1;
	k<-1;
	for ( i in c(2:(length(x))) ) {
		if (x[i] < qpivot) {
			qhead[j] = x[i];	
			j=j+1;
		} else {
			qtail[k] = x[i]
			k=k+1;
		}
	}
	
	if (length(qhead)!=0) qhead<-qsort(qhead);
	if (length(qtail)!=0) qtail<-qsort(qtail);

	x <-c(qhead, qpivot, qtail);
	return (x);
	}
	
}


# Exercise 3 Revised
#
# Si Tang, 396904
# Email: sugar@uchicago.edu
#

# 3a.
# Function of Difference equation of logistic model f(x)=2x(1-x)
# f(x) - x = x - 2* x* x
myfun1 <- function (x) {

	return (x- 2*x*x)
}

# 3b.
# Function of Difference equation of logistic model f(x) = 4x(1-x)
# f(x) - x = 3*x - 4*x*x
myfun2 <- function (x) {

	return (3*x - 4*x*x)
}


# Functor pasted from Exercise 2(b), see file, HW2-2-2_SiTang.R
MyFunctor <- function(fx) {
        # Get the initial value for iteration
        cat ("Please input an initial value: \n");
        init_value <- scan (n=1);

        # Get the maximum times for iteration        cat ("Please specify a maximum times of iteration : \n");
        max_iter <- scan(n=1);

        # x_array is a variable to store the X value in each iteration
        x_array <- numeric (length = (max_iter+1));
        x_array[1] <- init_value;

        # Trying to find the fixed points of the difference equation.
        # It is applicable given that one of the fixed points is 0
        # fx is the difference equation, as stated above.
        root1 <- uniroot(fx, c(0, 100), tol=0.001 )
        root2 <- uniroot(fx, c(0.0001,1000), tol=0.001)
        root <- numeric();
        root[1] <- sprintf("%.3f", root1[1]);
        root[2] <- sprintf("%.3f", root2[1]);
        cat ("the fixed points are x1=", root[1], "and x2=", root[2],"\n")
        # End of finding the fixed points of difference equation.

        x <- seq(from=0, to=( as.numeric(root[1])+ as.numeric(root[2])+ 1 ),length=100)
        y <- fx(x) + x;

        legend <- sprintf('Initial Value= %.2f \nIteration=%.0f \n Fixed Points: \n x1=%.3f \n x2=%.3f', init_value, max_iter, as.numeric(root[1]), as.numeric(r
oot[2]));
        par(mfcol=c(1,2)) ;
        plot(x, y, ylab='x(t)',xlim=c(0, 2*max(as.numeric(root))), ylim=c(0,max(y)), main='x(t)~x', type='l') ;
        lines (c(0,4), c(0, 4), type='l', col='red');
        legend(0,max(y), legend, bty='n')

        # A 'for' loop for generating the iterated map.
        for (i in 2:(max_iter+1)) {
                x_array[i] <- fx(x_array[i-1]) + x_array[i-1]

                # Draw a line from y=x to y=f(x)= 2 * x * (2-x/2)
                lines(c(x_array[i-1], x_array[i-1]), c(x_array[i-1], x_array[i]), col='purple');
                # Draw a line from y=f(x)= 2 * x * (2-x/2) to y=x
                lines(c(x_array[i-1], x_array[i]), c(x_array[i], x_array[i]), col='blue');

        }

        # Plot the x value against time t, the numeric solution. 
        plot(1:(max_iter+1), x_array, type='b', main='x(t) ~ t', col=3, xlab='t', ylab='x(t)')

        # Save the figure as an 'eps' file
        # If you would like to use it you can uncomment it.
         dev.print(device=postscript, file='fig.eps', horizontal=TRUE)

}

