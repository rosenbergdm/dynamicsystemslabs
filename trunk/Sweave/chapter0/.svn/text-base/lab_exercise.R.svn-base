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
### chunk number 2: bool_index_0
###################################################
myVec <- letters
myVec
myVec[1:5]          # First five letters


###################################################
### chunk number 3: bool_index_1
###################################################
myVec <- letters
myVec
idx <- 1:26
bool_idx <- idx < 10              # logical vector
bool_idx
myVec[bool_idx]                   # the first 10 letters of the alphabet


###################################################
### chunk number 4: if_usage eval=FALSE
###################################################
## if (CONDITION) {
##   BLOCK_1
## } else if (CONDITION_2) {
##   BLOCK_2
## } else {
##   ELSE_BLOCK
## }


###################################################
### chunk number 5: if_example eval=FALSE
###################################################
## x1 <- 2
## 
## if (x1 < 10) {
##   cat(x1, " has only 1 digit.")
## } else if (x1 < 100) {
##   cat(x1, " has two digits.")
## } else if (x1 < 1000) {
##   cat(x1, " has three digits.")
## } else {
##   cat(x1, " has more than three digits.")
## }
## 
## 
## x1 <- 16
## 
## if (x1 < 10) {
##   cat(x1, " has only 1 digit.")
## } else if (x1 < 100) {
##   cat(x1, " has two digits.")
## } else if (x1 < 1000) {
##   cat(x1, " has three digits.")
## } else {
##   cat(x1, " has more than three digits.")
## }
## 
## 
## x1 <- 128
## if (x1 < 10) {
##   cat(x1, " has only 1 digit.")
## } else if (x1 < 100) {
##   cat(x1, " has two digits.")
## } else if (x1 < 1000) {
##   cat(x1, " has three digits.")
## } else {
##   cat(x1, " has more than three digits.")
## }
## 
## x1 <- 1024
## if (x1 < 10) {
##   cat(x1, " has only 1 digit.")
## } else if (x1 < 100) {
##   cat(x1, " has two digits.")
## } else if (x1 < 1000) {
##   cat(x1, " has three digits.")
## } else {
##   cat(x1, " has more than three digits.")
## }


###################################################
### chunk number 6: if_example_1
###################################################
x1 <- 2

if (x1 < 10) {
  cat(x1, " has only 1 digit.")
} else if (x1 < 100) {
  cat(x1, " has two digits.")
} else if (x1 < 1000) {
  cat(x1, " has three digits.")
} else {
  cat(x1, " has more than three digits.")
}


x1 <- 16

if (x1 < 10) {
  cat(x1, " has only 1 digit.")
} else if (x1 < 100) {
  cat(x1, " has two digits.")
} else if (x1 < 1000) {
  cat(x1, " has three digits.")
} else {
  cat(x1, " has more than three digits.")
}


x1 <- 128
if (x1 < 10) {
  cat(x1, " has only 1 digit.")
} else if (x1 < 100) {
  cat(x1, " has two digits.")
} else if (x1 < 1000) {
  cat(x1, " has three digits.")
} else {
  cat(x1, " has more than three digits.")
}

x1 <- 1024
if (x1 < 10) {
  cat(x1, " has only 1 digit.")
} else if (x1 < 100) {
  cat(x1, " has two digits.")
} else if (x1 < 1000) {
  cat(x1, " has three digits.")
} else {
  cat(x1, " has more than three digits.")
}


###################################################
### chunk number 7: while_syntax eval=FALSE
###################################################
## while(CONDITION) {
##   BLOCK
## }


###################################################
### chunk number 8: while_ex1
###################################################
x0 <- 0
x1 <- 1
while(x1 < 1000) {      # x1 < 1000 is the condition
  newX1 <- x0 + x1      # -\
  x0 <- x1              # --+ -- these three lines make up the BLOCK
  x1 <- newX1           # -/
}
x1


###################################################
### chunk number 9: for_syntax eval=FALSE
###################################################
## for (COUNTER in VECTOR) {
##   BLOCK
## }


###################################################
### chunk number 10: for_ex1
###################################################
xVec <- c(0, 1)
for (ii in 3:20){
  xminus1 <- xVec[ii-1]
  xminus2 <- xVec[ii-2]
  newVal <- xminus1 + xminus2
  xVec <- c(xVec, newVal)
}
xVec


###################################################
### chunk number 11: complex-1
###################################################
myVar <- 3 + 4i
myVar
is.complex(3 + 4i)
is.complex(3)
myVar2 <- complex(real=3, imaginary=4)
myVar3 <- complex(argument=0.9272952, modulus=5)
myVar2
myVar3


