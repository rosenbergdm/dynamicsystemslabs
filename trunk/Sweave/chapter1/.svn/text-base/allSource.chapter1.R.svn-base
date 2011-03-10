## exercise.R
###################################################
### chunk number 1: aa_SweaveListingsPreparations
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


###################################################
### chunk number 2: func_syntax eval=FALSE
###################################################
## functionName <- function(argumentList) {
##   codeBlock
##   {return(returnValue)}
## }


###################################################
### chunk number 3: fun_ex_1
###################################################
fibLargerThan <- function(k) {
  x0 <- 0
  x1 <- 1
  while(x1 < k) {
    newX1 <- x0 + x1
    x0 <- x1
    x1 <- newX1
  }
  return(x1);
}
fibLargerThan(100)
fibLargerThan(1000)
fibLargerThan(10000)


###################################################
### chunk number 4: functor_example eval=FALSE
###################################################
## ## Expand on previous plot ...
## parabfun <- function (x) {
##   return(x^2-3)
## }
## curve(parabfun, xlim=c(-2, 2), 
##   ylim=c(-4, 1))


###################################################
### chunk number 5: functor_example_plot
###################################################
parabfun <- function (x) {
  return(x^2-3)
}
curve(parabfun, xlim=c(-2, 2), 
  ylim=c(-4, 1))


###################################################
### chunk number 6: sapply_example
###################################################
myFun <- function (x) {
  return(x^2 - 1)
}

myOtherFun <- function (x, y, z) {
  return(min(c(x/y, x/z, y/z, z/y, z/x, y/x)))
}

x_vector <- 1:10
x <- 1
y <- 2
z <- 3

sapply(x_vector, myFun)
do.call(myOtherFun, list(x=x, y=y, z=z))


###################################################
### chunk number 7: ex1_ch_1_r
###################################################
diffEqImap <- function(x_at_t) {
  x_at_t1 <- 5 * x_at_t;
  return(x_at_t1);
}


###################################################
### chunk number 8: ex1_ch_2_r
###################################################
init_value <- 10;
max_iter <-20;
x_array <- numeric(length=max_iter);  # size of x_array determined by max_iter
x_array[1] <- init_value;             # R indexing begins at 1, not 0


###################################################
### chunk number 9: ex_1_ch_3_r
###################################################
for (ii in 2:max_iter) {
  new_value <- diffEqImap(x_array[ii-1]);
  x_array[ii] <- new_value
}


###################################################
### chunk number 10: sec_plot_first_plot5 eval=FALSE
###################################################
## ## Expand on previous plot ...
## plot(1:20, x_array, type='l',
##      main='Example 1', xlab='t',
##      ylab='x(t)');


###################################################
### chunk number 11: sec_plot_first_plot5_1
###################################################
plot(1:20, x_array, type='l',
     main='Example 1', xlab='t',
     ylab='x(t)');


###################################################
### chunk number 12: hide_prompt
###################################################
options(prompt=' ', continue=' ')


###################################################
### chunk number 13: exam00
###################################################
#!/usr/bin/env rr
# encoding: utf-8
# parsePolynomial.R
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


###################################################
### chunk number 14: unhide_prompt
###################################################
options(prompt='> ', continue='+ ')



## guide.R
###################################################
### chunk number 1: aa_SweaveListingsPreparations
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


###################################################
### chunk number 2: sec_plot_first_plot
###################################################
x <- 1:20
x
y <- (1:20) ^ (0.5)
y
plot(x, y)


###################################################
### chunk number 3: sec_plot_first_plot_1
###################################################
x <- 1:20
y <- (1:20) ^ (0.5)
plot(x, y)


###################################################
### chunk number 4: sec_example_plots
###################################################
lf <- layout(matrix(c(1:4), nrow=2));
par(mar=c(2.0,2.0,2.0,2.0));
#layout.show(lf);
x <- 1:20
y <- (1:20) ^ (0.5)
x0 <- x[1:20 %% 2 == 1]
x1 <- x[1:20 %% 2 == 0]
y0 <- y[1:20 %% 2 == 1]
y1 <- y[1:20 %% 2 == 0]
plot(x, y, main='plot(x, y)')
plot(x,y, type='n', main='lines(x, y)');
lines(x, y);
plot(x,y, type='n', main='segments(x0, y0, x1, y1)');
segments(x0, y0, x1, y1);
plot(x,y, type='n', main='arrows(x0, y0, x1, y1)');
arrows(x0, y0, x1, y1, length=0.1);


###################################################
### chunk number 5: sec_plot_first_plot2
###################################################
x <- seq(from=0, to=2*pi, len=1000)
y <- cos(2*x)
## just provide data; sensible labelling
plot(x,y)
dev.new()


###################################################
### chunk number 6: sec_plot_first_plot2_1
###################################################
x <- seq(from=0, to=2*pi, len=1000)
y <- cos(2*x)
## just provide data; sensible labelling
plot(x,y)


###################################################
### chunk number 7: sec_plot_first_plot3 eval=FALSE
###################################################
## ## Expand on previous plot ...
## plot(x,y, main='cos(2x)', type='l',
##      lty=1, bty='n')
## y2 <- sin(2*x)
## lines(x,y2, main='sin(2x)', type='l',
##       lty=2)
## same <- which( abs(y - y2) < 0.01)
## points(x[same], y[same], pch=19,
##        col='red', cex=3)


###################################################
### chunk number 8: sec_plot_first_plot3_1
###################################################
## Expand on previous plot ...
plot(x,y, main='cos(2x)', type='l', lty=1, bty='n')
y2 <- sin(2*x)
lines(x,y2, main='sin(2x)', type='l', lty=2)
same <- which( abs(y - y2) < 0.01)
points(x[same], y[same], pch=19, col='red', cex=3)



## listingPreps.R
#require(base)
require(SweaveListingUtils)
require(pgfSweave)
oldRset <- .myRset <- getSweaveListingOption("Rset")
options(width=50);
oldRout <- .Rout <- getSweaveListingOption('Rout')
#options(warn=3)
#options(error=recover)
.myRset[['literate']]<-"{<-}{<-}2{<<aa_-}{<<aa_-}2"
.myRset$basicstyle <- "{\\tiny\\color{Rcommentcolor}}"
.myRset[['keywordstyle']] <- "{\\tiny\\bf\\color{red}}"
.myRset$numbers <- 'left'
.myRset$commentstyle <- "{\\color{black}\\ttfamily\\itshape}"
.myRset$numberstyle="\\tiny"
.myRset$escapeinside="{(*@}{@*)}"
.Rout$fancyvrb <- 'true'
.Rout$keywordstyle <- "{\\color{Routcolor}}"
.Rout$breaklines <- 'true'
.Rout$linewidth <- "{0.7\\textwidth}"
.myRset$extendedchars <- 'true'
.myRset$breaklines <- 'true'
.myRset$linewidth="{0.7\\textwidth}"
.myRset$otherkeywords <- "{!,!=,~,$,*,\\&,\\%/\\%,\\%*\\%,\\%\\%,<-,<<aa_-,/, \\%in\\%}"
setToBeDefinedPkgs(pkgs = c("base"), keywordstyle="\\bf\\color{red}")
SweaveListingoptions(Rset=.myRset, Rout=.Rout, intermediate = FALSE)
#SweaveListingPreparations()
setCacheDir('cache2')
#options(device=quartz);
source('maxima_utilities.R')
par(mar=c(2,2,2,2))

## solution.R
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



