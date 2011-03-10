# Ben Umans
# Chapter 1

###################################################
### Ben Umans Week 2 Assignment
###################################################
#require(base)
require(SweaveListingUtils)
require(pgfSweave)
oldRset <- .myRset <- getSweaveListingOption("Rset")
oldRout <- .Rout <- getSweaveListingOption('Rout')
#options(warn=3)
#options(error=recover)
.myRset[['literate']]<-"{<-}{<-}2{<<aa_-}{<<aa_-}2"
.myRset$basicstyle <- "{\\footnotesize\\color{Rcommentcolor}}"
.myRset[['keywordstyle']] <- "{\\footnotesize\\bf\\color{red}}"
.myRset$numbers <- 'left'
.myRset$commentstyle <- "{\\color{black}\\ttfamily\\itshape}"
.myRset$numberstyle="\\tiny"
.Rout$fancyvrb <- 'true'
.Rout$keywordstyle <- "{\\color{Routcolor}}"
.Rout$breaklines <- 'true'
.Rout$linewidth <- "{0.5\\textwidth}"
.myRset$extendedchars <- 'true'
.myRset$breaklines <- 'true'
.myRset$linewidth="{0.5\\textwidth}"
.myRset$otherkeywords <- "{!,!=,~,$,*,\\&,\\%/\\%,\\%*\\%,\\%\\%,<-,<<aa_-,/, \\%in\\%}"
setToBeDefinedPkgs(pkgs = c("base"), keywordstyle="\\bf\\color{red}")
SweaveListingoptions(Rset=.myRset, Rout=.Rout, intermediate = FALSE)
#SweaveListingPreparations()
setCacheDir('cache2')
options(device=quartz);
par(mar=c(2,2,2,2))
#############################################################
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
#############################################################
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
#Exercise 1
#define inputExpression

intermediate <- parsePolynomial('3x^2+2x+1')

derivativeExponents <- 	(intermediate$exponents-1)}
derivativeCoefficients <- (intermediate$coefficients*intermediate$exponents)

deparsePolynomial(derivativeCoefficients, derivativeExponents)

############################################################

#Exercise 2a


#We can plot the evolution of the model by numerical methods as a difference equation:

logisticdiff <- function (x) {
nextx <- x*(2-x/2)
return(nextx)
	}

initial <- 1
iterations <- 20
array <- numeric(length=iterations)
array[1] <- initial

for (ii in 2:iterations) {
	nextvalue <- logisticdiff(array[ii-1])
	array[ii] <- nextvalue
	}
plot(1:20, array, main='Numerical Solution to Logistic Difference Equation', xlab='x', ylab='f(x)')
#fixed points clearly occur at 0 and 2, with 2 being a stable fixed point

##########################################################

#Exercise 2b

logisticdiff <- function (x) {
nextx <- x*(2-x/2)
return(nextx)
	}


solve <- function(logisticdiff, initial) {
array <- c(initial)
nextvalue <- logisticdiff(array[1])
	array <- c(array, nextvalue)

while (array[length(array)] != array[length(array)-1]) {
	array <- c(array, 0)
	nextvalue <- logisticdiff(array[length(array)-1])
	array[length(array)] <- nextvalue
	}
	
print(array[length(array)])

plot(array, main='Numerical Solution to Logistic Difference Equation', xlab='x', ylab='f(x)')

}


#Note that you can specify any logisticdiff and the functor 'solve' will use it to find and plot the steady state.  Note also that, unless you set it as the initial condition, the functor will not find unstable fixed points.



#############################################################################
#Exercise 3a

#We can solve analytically and find that fixed points occur at 0 and 0.5.  The numerical solution will approach the fixed points if we set an initial value close enough to it (in the case of 0.5).

logisticdiff <- function (x) {
nextx <- 2*x*(1-x)
return(nextx)
	}

initial <- 1
solve(logisticdiff, initial)

initial <- 0.7
solve(logisticdiff, initial)

############################################################################
#Exercise 3b

#Fixed points will occur at 0 and 0.75

logisticdiff <- function (x) {
nextx <- 4*x*(1-x)
return(nextx)
	}

	
	
initial <- 1
solve(logisticdiff, initial)

initial <- 0.6
solve(logisticdiff, initial)

#The Model is unstable and, so if the initial condition is not chosen carefully the numerical solution will run infinitely.