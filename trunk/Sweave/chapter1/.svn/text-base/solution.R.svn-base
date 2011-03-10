###################################################
### chunk number 1: aa_SweaveListingsPreparations
###################################################
source('listingPreps.R');


###################################################
### chunk number 2: parse_deparse
###################################################
.calcFirstDerivative <- function(coefs, expons) {
  new_expons <- expons - 1;
  new_expons <- new_expons[new_expons != -1]
  new_coefs <- coefs * expons;
  new_coefs <- new_coefs[new_expons != -1];
  new_coefs <- new_coefs[1:length(new_expons)]
  result <- list(coefs=new_coefs, expons=new_expons);
}


###################################################
### chunk number 3: parse_deparse1
###################################################
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


###################################################
### chunk number 4: parse_deparse2
###################################################
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


###################################################
### chunk number 5: parse_deparse3
###################################################
calcPolyDeriv <- function(input_exp) {
  parsed <- parsePolynomial(input_exp);
  converted <- .calcFirstDerivative(parsed$coefficients, parsed$exponents);
  result <- deparsePolynomial(converted$coefs, converted$expons);
  return(result);
}
calcPolyDeriv('3x^3 + 2x + 1');
calcPolyDeriv('5x^4 - 3x^3 + x^2 - 1');


###################################################
### chunk number 6: functor_logit
###################################################
source('http://rosenbergdm.uchicago.edu/maxima_utilities.R');
plotImap <- function(f) {
  f_zeros <- mSolve(paste(deparse(body(f)), " = 0", sep=''));
  max_iter <- 10;
  init_value <- f_zeros[1] + diff(f_zeros) / 4;
  x_array <- numeric(length=max_iter);
  x_array[1] <- init_value;
  for (ii in 2:max_iter) {
    x_array[ii] <- do.call(f, list(x_array[ii-1]));
  }
  fixed_points <- mSolve(paste(deparse(body(f)),
    " = x", sep=''));
  plot(1:max_iter, x_array, main=paste('Iterated map of f(x)=\n', 
       deparse( body(f) ), sep=''), type='l', xaxs='i',
       xlim=c(0, max_iter), xlab='', ylab=expression(f(t)), 
       sub=paste(c("fixed points: \nx = [", fixed_points[1], ",",
       fixed_points[2], "]"), collapse=" ") );  
}
f <- function(x) 
  x * (2 - x / 2)


###################################################
### chunk number 7: functor_logit_plot
###################################################
plotImap(f)


###################################################
### chunk number 8: qsort_solution
###################################################
options(width=60);    # To make it fit on the page
qSort <- function (x) {
  if (length(x) < 2) {
    return(x);
  } else {
    pivot <- x[1];
    x <- x[-1];
    head <- qSort(x[x < pivot]);
    tail <- qSort(x[x >= pivot]);
    return(c(head, pivot, tail));
  }
}
input_vector <- rnorm(n=20) * 100;
sorted_vector <- qSort(input_vector);
input_vector
sorted_vector


###################################################
### chunk number 9: qsort_efficiency
###################################################
options(expressions=500000);  # Can cause stack overflow
inVec <- rnorm(n=1000);
inVec2 <- qSort(inVec);
system.time(qSort(inVec));
system.time(qSort(inVec2));   # Might crash on some systems


###################################################
### chunk number 10: functor_logit_plot2
###################################################
options(width=78)
layout(matrix(c(1,2), nrow=1));         # Side-by-side plots
plotImap(function(x) 2 * x * (1 - x));  # implicit function definition
plotImap(function(x) 4 * x * (1 - x));


