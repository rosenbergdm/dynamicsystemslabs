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
### chunk number 2: solution1A_1 eval=FALSE
###################################################
## x0 <- 0;
## x1 <- 1;
## while(x1 < k){
##   next_value <- x0 + x1;
##   x0 <- x1;
##   x1 <- next_value;
## }
## if (x1 == k) {
##   cat(k, ' is a fibonnaci number')
## } else {
##   cat(x0, ' is the largest fibonnaci number <= ', k)
## }


###################################################
### chunk number 3: solution1B eval=FALSE
###################################################
## if(abs(x-k) < abs(y-k)) {
##   cat(x, ' is closer to ', k, '\n');
## } else if (abs(x-k) == abs(y-k)) {
##   cat(x, ' and ', y, ' are equidistant from ', k, '\n')
## } else {
##   cat(y, ' is closer to ', k, '\n')
## }


###################################################
### chunk number 4: solution1A_1 eval=FALSE
###################################################
## x0 <- 0;
## x1 <- 1;
## while(x1 < k){
##   next_value <- x0 + x1;
##   x0 <- x1;
##   x1 <- next_value;
## }
## if (x1 == k) {
##   cat(k, ' is a fibonnaci number')
## } else if(abs(x0-k) < abs(x1-k)){
##   cat(x0, ' is the fibonnaci number closest to ', k)
## } else {
##   cat(x1, ' is the fibonnaci number closest to ', k)
## }


###################################################
### chunk number 5: solution2A_1 eval=FALSE
###################################################
## ## Given these two 'inputs'
## sorted <- c(a, b) ## a < b
## to_add <- d       ## new value
## if(d < a) {
##   c(d, a, b)
## } else if (d < b) {
##   c(a, d, b)
## } else {
##   c(a, b, d)
## }


###################################################
### chunk number 6: solution2B_1 eval=FALSE
###################################################
## ## unsorted input
## unsorted <- c(a, b)
## if (a <= b) {
##   c(a, b)
## } else {
##   c(b, a)
## }


###################################################
### chunk number 7: solution2C_1
###################################################
## input
unsorted <- floor(runif(n=10, min=1, max=100))
unsorted
sorted <- numeric(length=length(unsorted))
for (ii in 1:9) {
  smallest <- unsorted[1]
  unsorted <- unsorted[-1]
  for (jj in 1:length(unsorted)) {
    if (unsorted[jj] < smallest) {
      temp <- unsorted[jj]
      unsorted[jj] <- smallest
      smallest <- temp
    }
  }
  sorted[ii] <- smallest;
}
sorted[10] <- unsorted
sorted


###################################################
### chunk number 8: solution3_1
###################################################
## As an example
k <- 15
args <- seq(from=0, to=k-1, by=1) * pi / k
mods <- c(rep(c(1, -1), k))[1:k]
solutions <- complex(modulus=mods, argument=args)
solutions
solutions ^ 15


