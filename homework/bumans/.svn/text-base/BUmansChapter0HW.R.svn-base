# Ben Umans
# Chapter 0


###################################################
#chunk number 1: aa_SweaveListingsPreparations
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


#Ben Umans
#Bio 26210
#Lab 1


###################################################
# Exercise 1a
###################################################
x0 <- 0
x1 <- 1
while(x1 < 1000) {     
  newX1 <- x0 + x1      
  x0 <- x1             
  x1 <- newX1          
}
x0

###################################################
#Exercise 1b

xdist <-abs(k-x)
ydist<-abs(k-y)

if (xdist > ydist) {
y
}else {
x
} 

###################################################
#Exercise 1c
x0 <- 0
x1 <- 1
while(x1 < k) {      
  newX1 <- x0 + x1     
  x0 <- x1             
  x1 <- newX1          
}

x1dist <- abs(k-x1)
x0dist<- abs(k-x0)

if (x1dist > x0dist) {
x0
}else {
x1
} 

###################################################
#Exercise 2a
#starting with Vector1 of length 1 and Vector2 of length 2

Vector3 <- c(Vector2, Vector1)
for (j in 1:2){
	for (ii in 1:2){
	if (Vector3[ii] > Vector3[ii+1]) {
	vectortemp <- Vector3[ii]
	Vector3[ii] <- Vector3[ii+1]
	Vector3[ii+1] <- vectortemp
	print(Vector3)	
	}else {
		print(Vector3)
		}
	}	
	}
	
	
	
	

###################################################
#Exercise 2b
#start with Vector2 of length 2

Vector2 <- c(5, 3)


if (Vector2[1] > Vector2[2]) {
	vectortemp <- Vector2[1]
	Vector2[1] <- Vector2[2]
	Vector2[2] <- vectortemp
	print(Vector2)	
	}else {
		print(Vector2)
		}

###################################################
#Exercise 2c

#This works by repeating the Vector2 sort module over the
#length of the vector and then repeating the whole
#sort procedure for a total of 9 passes.  This is because we can
#imagine the most extreme case in which the highest value begins 
#in position 1 and will then have to be moved 9 times to the final position.
#This is inefficient (since it may sort for more passes than needed), but
#for such a simple operation the consequence is irrelevant.


#start with Vector10 of length 10, or generate it randomly

for (j in 1:9){
	
	for (ii in 1:9){
	if (Vector10[ii] > Vector10[ii+1]) {
	vectortemp <- Vector10[ii]
	Vector10[ii] <- Vector10[ii+1]
	Vector10[ii+1] <- vectortemp
	print(Vector10)	
	}else {
		print(Vector10)
		}
	}	
	
	}

###################################################
#Exercise 3

#for a given k, if x^k=1 there will be k roots given by exp(2*i*pi*(t-1)/k)
#where t is some natural number.

#to begin, the user must define the order of the roots:
#k <- 

solutions <- c(1:k)

for (t in 1:k){
	solutions[t] <- exp(2i*pi*(t-1)/k)
	
	}

print(solutions)