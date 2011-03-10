# Craig Biwer
# Chapter 1 Exercise

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


#1

polyderive <- function(polynomial) {
	parsed <- parsePolynomial(polynomial);
	newCoeff <- parsed[[1]] * parsed[[2]];
	newExp <- pmax(parsed[[1]]-1, 0);
	if(parsed[[1]][length(parsed[[1]])] == 0) {
		newCoeff <- newCoeff[1:(length(newCoeff)-1)];
		newExp <- newExp[1:(length(newExp)-1)];
	}
	deparsePolynomial(newCoeff, newExp)
}


#2

diffEqImap <- function(xatt) {
	xatt1 <- xatt*(2-xatt/2);
	return(xatt1);
}

initvalue <- 1;
maxiter <- 20;
xarray <- numeric(length=maxiter);
xarray[1] <- initvalue;
for(ii in 2:maxiter) {
	newvalue <- diffEqImap(xarray[ii-1]);
	xarray[ii] <- newvalue
}

plotfunction <- function(inputpoly) {
	initvalue <- .2;
	maxiter <- 20;
	xarray <- numeric(length=maxiter);
	xarray[1] <- initvalue;
	for(ii in 2:maxiter) {
		newvalue <- do.call(inputpoly, list(x=xarray[ii-1]));
		xarray[ii] <- newvalue;
	}
	plot(1:20, xarray, type='l', main='Problem 3b', xlab='t', ylab='x(t)');
}

# The code below generates the plot. I commented it out so it doesn't
# generate a graph each time you load the file.
# plot(1:20, xarray, type='l', main='Problem 2a', xlab='t', ylab='x(t)')
# Fixed points: x = 0 and x = 2


#3a

threea <- function(x) {
	return(2*x*(1-x))
}
#plotfunction(threea)
#Fixed points: x = 0 and x = 1/2
#Plot approaches analytically calculated x = 1/2

 
#3b
 
threeb <- function(x) {
	return(4*x*(1-x))
}
#plotfunction(threeb)
#Fixed points: x = 0 and x = 3/4
#Plot does not approach analytically calculated x = 3/4, but instead varies wildly