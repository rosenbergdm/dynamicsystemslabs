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
### chunk number 2: input_ex eval=FALSE
###################################################
## 3 + 5
## help.start()
## load('myData.RData')


###################################################
### chunk number 3: output_ex
###################################################
3 + 5
randomData <- rnorm(n=100)
summary(randomData)


###################################################
### chunk number 4: hide_prompt
###################################################
options(prompt=' ', continue=' ')


###################################################
### chunk number 5: ex_source eval=FALSE
###################################################
## #!/usr/bin/env rr
## # encoding: utf-8
## # sumDigits.R
## #
## # sumDigits - a function which takes as input a number and returns the
## #            sum of its digits
## #
## # Example:
## #       > sumDigits(15)
## #       [1] 6
## #       > sumDigits(c(10, 122, 134))
## #       [1] 1 5 8
## #
## sumDigits <- function(x) {
##   return(sum(as.integer(strsplit(as.character(x), '')[[1]])))
## }


###################################################
### chunk number 6: unhide_prompt
###################################################
options(prompt='> ', continue='+ ')


###################################################
### chunk number 7: startupmessage
###################################################
startuptext <- "
R version 2.10.0 Under development (unstable) (2009-06-03 r48708)
Copyright (C) 2009 The R Foundation for Statistical Computing
ISBN 3-900051-07-0

R is free software and comes with ABSOLUTELY NO WARRANTY.
You are welcome to redistribute it under certain conditions.
Type 'license()' or 'licence()' for distribution details.

  Natural language support but running in an English locale

R is a collaborative project with many contributors.
Type 'contributors()' for more information and
'citation()' on how to cite R or R packages in publications.

Type 'demo()' for some demos, 'help()' for on-line help, or
'help.start()' for an HTML browser interface to help.
Type 'q()' to quit R.

>
";
cat(startuptext);


###################################################
### chunk number 8: firstsession_0 eval=FALSE
###################################################
## 3
## 3 + 5
## 1:50
## x <- 1:5
## x / 2


###################################################
### chunk number 9: firstsession_1
###################################################
3
3 + 5
1:50
x <- 1:50
x / 2


###################################################
### chunk number 10: firstsession_1_1
###################################################
3


###################################################
### chunk number 11: firstsession_1_2
###################################################
3 + 5


###################################################
### chunk number 12: firstsession_1_3
###################################################
1:50


###################################################
### chunk number 13: firstsession_1_4
###################################################
x <- 1:50


###################################################
### chunk number 14: firstsession_1_4_1
###################################################
x


###################################################
### chunk number 15: firstsession_1_5
###################################################
x / 2


###################################################
### chunk number 16: firstsession
###################################################
x <- rnorm(50, mean=4)
x
mean(x)
range(x)
hist(x)
## check help -- how to change title?


###################################################
### chunk number 17: firstsession_plot_2 eval=FALSE
###################################################
## ?hist


###################################################
### chunk number 18: firstsessionplot
###################################################
hist(x, main='my first plot', )


###################################################
### chunk number 19: tabcompletion_1 eval=FALSE
###################################################
## exp


###################################################
### chunk number 20: tabcompletion_2
###################################################
cat('\nexp\nexp\texpand.grid\texpand.model.frame\texpm1\texpression\n');


###################################################
### chunk number 21: tabcompletion_3
###################################################
cat('> plot(\n')


###################################################
### chunk number 22: tabcompletion_4
###################################################
plotCompletions <- "
...=               ci.lty=            do.points=         log=               panel.last=        x=
absVal=            ci.type=           edge.root=         lty.intervals=     panel=             xaxt=
add.smooth=        ci=                edgePar=           lty.predicted=     par.fit=           xlab=
add=               col.01line=        formula=           lty.separator=     pch=               xlim=
angle=             col.hor=           frame.plot=        lty=               plot.type=         xpd=
ann=               col.intervals=     freq=              lwd=               predicted.values=  xval=
ask=               col.points=        grid=              main2=             qqline=            xy.labels=
asp=               col.predicted=     hang=              main=              range.bars=        xy.lines=
axes=              col.range=         horiz=             mar.multi=         separator=         y=
border=            col.separator=     id.n=              mar=               set.pars=          yax.flip=
caption=           col.vert=          intervals=         max.mfrow=         sub.caption=       yaxt=
center=            col=               label.pos=         mgp=               sub=               ylab=
cex.caption=       conf=              labels.id=         nc=                subset=            ylim=
cex.id=            cook.levels=       labels=            nodePar=           type=              zero.line=
cex.main=          dLeaf=             leaflab=           oma.multi=         verbose=
cex.points=        data=              legend.text=       oma=               verticals=
ci.col=            density=           levels=            panel.first=       which=
";
plotCompletions <- gsub("\n", "\t", gsub(" +", "\t", plotCompletions));
plotCompletions <- strsplit(plotCompletions, '\t')[[1]]
print(plotCompletions);


###################################################
### chunk number 23: comment_ex
###################################################
# This is a comment
'# This is not a comment'


###################################################
### chunk number 24: continuation_prompt
###################################################
3 + 5 +
2

sqrt(
  3.141)



###################################################
### chunk number 25: example_prompt
###################################################
cat('\nHit <Return> to see the next plot: \n')


###################################################
### chunk number 26: ls_example
###################################################
ls()
ls(all.names=TRUE)


###################################################
### chunk number 27: help_ex_1 eval=FALSE
###################################################
## help('plot')


###################################################
### chunk number 28: help_ex1_1
###################################################
plot_help <- help('plot')
plot_help_text <- paste(readLines(plot_help)[1:10], collapse="\n");
plot_help_text <- gsub('_\\\b(.{1})', '\\1', plot_help_text);
cat(plot_help_text);


###################################################
### chunk number 29: help_ex_2 eval=FALSE
###################################################
## ?plot


###################################################
### chunk number 30: help_ex3 eval=FALSE
###################################################
## help.search('plot')
## ??plot


###################################################
### chunk number 31: ex_ex1
###################################################
example('Arithmetic')


###################################################
### chunk number 32: example_2_calculator
###################################################
# Arithmetic
3 / 5
301 + 50000003
0.0005 * 0.0001
-0.0001 ** 9
-0.0001 ^ 9
## exponentiation can be represented with either ** or ^
3 + 5 * 2
(3 + 5) * 2
## special operations are called by name
sin(3)
sqrt(5)
## complex numbers are supported when written as x + yi
-1 + 0i
sqrt(-1 + 0i)
## constants can be called by name or expression (varies)
pi
exp(1)


###################################################
### chunk number 33: vector_ex
###################################################
firstVector <- letters[1:10];
firstVector   # if no index is given, a variable name referes to all members
firstVector[5]    # The fifth member of firstVector
firstVector[5] <- 'E';  # Members can be assigned by indexing as well
firstVector


###################################################
### chunk number 34: vector_ex_2
###################################################
secondVector <- c(1,4,9,16)         # c() is used to make vectors
secondVector                        #
biggerVector <- c(secondVector, 25) # and to add to them
numVector <- 1:5                    # a vector of integers can by made using
numVector                           #       the start:end notation
numVector[c(1,3,5)]                 # You can use vectors to index vectors


###################################################
### chunk number 35: vector_ex_3
###################################################
firstVector <- 1:15
firstVector.1 <- firstVector / pi
firstVector.1
firstVector.2 <- sin(firstVector.1)
firstVector.2
length(firstVector)           # length() is used to find out how many members
                              # a vector has
max(firstVector)              # largest member
sum(firstVector)              # sum of all members
firstVector > pi/2            # a logical vector is returned


###################################################
### chunk number 36: matrix_ex
###################################################
matrix(data=c(1:12), nrow=4, ncol=3)    # Note how the values go DOWN first
matrix(data=c(1:12), nrow=4, ncol=3, byrow=TRUE)   # byrow=TRUE avoids this
matrix(c(1:12), 4, 3, byrow=TRUE)       # The xxx= are optional
matrix(c(1:12), 4, byrow=TRUE)          # only one of nrow, ncol is needed
matrix(c(1:12), ncol=3, byrow=TRUE)
myMatrix <- matrix(c(1:12), 4, byrow=TRUE)
t(myMatrix)                             # t() transposes a matrix
myMatrix
myMatrix[4,3]                   # members are referenced by two indices
myMatrix[4,]             # rows/columns are extracted by omitting an index
myMatrix[,3]
myMatrix[c(1:2), c(1:3)] # vector indexing works as before
rbind(myMatrix, myMatrix)       # rbind() joins matrices vertically
cbind(myMatrix, myMatrix)       # cbind() joins matrices horizontally
myMatrix / 5                    # Operations are always performed element-wise
myMatrix * myMatrix             # (Even multiplication)
myMatrix %*% t(myMatrix)        # unless the %*% notation is used


###################################################
### chunk number 37: hide_prompt_2
###################################################
options(prompt=' ', continue=' ')


###################################################
### chunk number 38: style_example_1 eval=FALSE
###################################################
## # simpleFactor.R
## #
## # Tools for finding the prime factorization of integers using the
## # sieve of Eratosthenes.
## #
## ##STYLE - lines 1-4 serve as a 'commented' header
## #
## # getPrimeFactors
## # Prime factorization function.
## #
## # argument input_number - Integer to find the prime factorization of
## # returns - A numeric vector listing the prime factors of input_number.  The
## #           multiplicity of each number in the return vector represents the
## #           multiplicity of that factor in input_number's prime factorization
## #
## ##STYLE - lines x - y describe the input and output of the getPrimeFactors
## ##STYLE   function
## getPrimeFactors <- function(input_number) {
##   ##STYLE - note that the name getPrimeFactors is explains the function well
##   if (input_number > 100000) {
##     stop(input_number, " is too large to efficiently factor\n");
##   }                             # The sieve of Eratosthenes isn't very fast
## 
##   ##STYLE - Note how we increased indentation inside the function declaration
##   ##        and again inside the if-block
##   prime_factors <- c();
## 
##   max_test_to <- floor(input_number/2);
##   # The largest possible prime factor is the square root of input_number
##   possible_factors <- 2:max_test_to;
## 
##   isFactor <- function(dividend, divisor) {
##     remainder <- dividend %% divisor;
##     # Remeber %% is the modular divisior operator
##     if (remainder == 0) {
##       return(TRUE);
##     } else {
##       return(FALSE);
##     }                                 # end if
##   }                                   # end function isFactor
## 
##   ##STYLE - It is often useful to label closing braces
## 
##   getPrimeMultiplicity <- function(dividend, divisor) {
##     multiplicity_count <- 0;
##     while(dividend %% divisor == 0) {
##       multiplicity_count <- multiplicity_count + 1;
##       dividend <- dividend / divisor;
##     }
##     return(multiplicity_count);
##   }
## 
##   removeMultiples <- function(total_list, divisor) {
##     new_list <- total_list[total_list %% divisor != 0];
##     # logical indexing
##     return(new_list);
##   }                                   # end function removeMultiples
## 
##   while (length(possible_factors) > 0) {
##     divisor <- possible_factors[1];
##     possible_factors <- possible_factors[-1];
##     # possible_factors[-1] refers to all elements of possible_factors except
##     #        the first
##     if (isFactor(input_number, divisor)) {
##       multiplicity_count <- getPrimeMultiplicity(input_number, divisor);
##       prime_factors <- c(prime_factors, rep(divisor,
##                                             multiplicity_count));
## 
##       ##STYLE - the previous line was split to keep it from being too long.
##       ##        note the indentation so that divisor and multiplicity_count
##       ##        line up
##       possible_factors <- removeMultiples(possible_factors, divisor);
##     }                                 # end if
##   }                                   # end while
## 
## 
##   ##STYLE - Note naming consistency.  I use camelCase for function names and
##   ##        All-lowercase-with-underscores for variables.
## 
##   if (length(prime_factors) == 0) {
##     prime_factors <- c(1, input_number);
##   } else {
##     prime_factors <- c(1, prime_factors);
##   }
##   return(prime_factors);
## }


###################################################
### chunk number 39: unhide_prompt_2
###################################################
options(prompt='> ', continue='+ ')


