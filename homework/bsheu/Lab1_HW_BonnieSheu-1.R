# name: Sheu, Bonnie
# assignment: Chapter 1 Lab Exercise 
# date:
# filename: Lab1_HW_BonnieSheu
##################################################################

##################################################################
# name: Sheu, Bonnie
# assignment: Chapter 1 Lab Exercise
# date: 
# question: 1
# subquestion: 
# other files: 
##################################################################
--

# First run provided functions parsePolynomial() and deparsePolynomial()

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


# Assign an input polynomial

inputEx <- "x^4 + 2x^3 - x^2 - x + 1"


# Run deriv() function

deriv <- function(x) {
  result <- parsePolynomial(inputEx)
  coef <- c(result[['exponents']]*result[['coefficients']])
  exp <- c(result[['exponents']] - 1) 
  inputEx1 <- deparsePolynomial(coef, exp)
  return(inputEx1)
}

deriv(inputEx)

#/--
##################################################################
# name: Sheu, Bonnie
# assignment: Chapter 1 Lab Exercise
# date: 
# question: 2
# subquestion: a
# other files: Lab1_2a_Graph_BonnieSheu.pdf
##################################################################
--

logMod <- function(x_at_t) {
  x_at_t1 <- x_at_t*(2 - x_at_t/2)
  return(x_at_t1)
}

init_value <- 1
max_iter <- 20
x_array <- numeric(length=max_iter)
x_array[1] <- init_value

for (ii in 2:max_iter) {
  new_value <- logMod(x_array[ii-1])
  x_array[ii] <- new_value
}

plot(1:20, x_array, type='l', main='Logistic Model', xlab='t', ylab='x(t)')

#/--
##################################################################
# name: Sheu, Bonnie
# assignment: Chapter 1 Lab Exercise
# date: 
# question: 2
# subquestion: b
# other files: 
##################################################################
--

# Assign input logistic difference equation

logDiffEq <- "x_at_t*(2 - x_at_t/2)"


# Functor

functor <- function(logDiffEq) {
  diffEqImap <- function(x_at_t) {
  x_at_t1 <- x_at_t*(2 - x_at_t/2)        ## Input logDiffEq here
  return(x_at_t1)
}

  init_value <- 1
  max_iter <- 20
  x_array <- numeric(length=max_iter)
  x_array[1] <- init_value

  for (ii in 2:max_iter) {
  new_value <- diffEqImap(x_array[ii-1])
  x_array[ii] <- new_value
}

  output <- plot(1:20, x_array, type='l', main='Logistic Model', xlab='t', ylab='x(t)')
  return(output)
}

functor(logDiffEq)

#/--
##################################################################
# name: Sheu, Bonnie
# assignment: Chapter 1 Lab Exercise
# date: 
# question: 3
# subquestion: a
# other files: Lab1_3a_Graph_BonnieSheu.pdf
##################################################################
--

# Assign given logistic difference equation

logDiffEq <- "2*x_at_t*(1 - x_at_t)"


# Functor

diffEqImap <- function(logDiffEq) {
  logDiffEq <- function(x_at_t) {
  x_at_t1 <- 2*x_at_t*(1 - x_at_t)        ## Input logDiffEq here
  return(x_at_t1)
}

  init_value <- 1
  max_iter <- 20
  x_array <- numeric(length=max_iter)
  x_array[1] <- init_value

  for (ii in 2:max_iter) {
  new_value <- logDiffEq(x_array[ii-1])
  x_array[ii] <- new_value
}

  output <- plot(1:20, x_array, type='l', main='Logistic Model', xlab='t', ylab='x(t)')
  return(output)
}

diffEqImap(logDiffEq)


# We can analytically calculate the fixed points by rewriting the
# given model F(x) = 2x(1-x) as F(x) = (2-2x)x = (r-kN*)N*, where
# the fixed point(s) N* = (r-1)/k.
# 
# By plugging in r=2 and k=2, we get N* = (2-1)/2 = 0.5.
#
# The plot of the numerical solution does not appear to approach 
# the calculated fixed point N* = 0.5.

#/--
##################################################################
# name: Sheu, Bonnie
# assignment: Chapter 1 Lab Exercise
# date: 
# question: 3
# subquestion: b
# other files: Lab1_3b_Graph_BonnieSheu.pdf
##################################################################
--

# Assign given logistic difference equation

logDiffEq <- "4*x_at_t*(1 - x_at_t)"


# Functor

diffEqImap <- function(logDiffEq) {
  logDiffEq <- function(x_at_t) {
  x_at_t1 <- 4*x_at_t*(1 - x_at_t)        ## Input logDiffEq here
  return(x_at_t1)
}

  init_value <- 1
  max_iter <- 20
  x_array <- numeric(length=max_iter)
  x_array[1] <- init_value

  for (ii in 2:max_iter) {
  new_value <- logDiffEq(x_array[ii-1])
  x_array[ii] <- new_value
}

  output <- plot(1:20, x_array, type='l', main='Logistic Model', xlab='t', ylab='x(t)')
  return(output)
}

diffEqImap(logDiffEq)

# F(x) = 4x(1-x) = (4-4x)x = (r-kN*)N*.
# N* = (r-1)/k = (4-1)/4 = 0.75.
# Again, the plot of the numerical solution does not appear to 
# approach the calculated fixed point N* = 0.75.

#/--
##################################################################