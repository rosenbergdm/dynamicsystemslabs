###################################################
### Ben Umans Week 5 Assignment
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
#############################################################################
# name: Ben Umans
# assignment: 2 c
# date: 10/30/09
# filename: NAME_OF_FILE
#############################################################################
f <- function(N, t) {
	return(N-sin((4*t)/2*pi))
	}
tRange <- c(0, 10)
dt <- 0.1
NRange <- c(0, 10)
dN <- .2
h <- 0.5

tSteps <- diff(range(tRange))/dt
NSteps <- diff(range(NRange))/dN

plot(c(0, 9), c(-0.2, 3), main='rN-sin((4t)/(2pi))', xlab='t', ylab='N', fg=gray(0.6), yaxs='i')

for (ii in 0:(tSteps-1)) {
	for (jj in (0:NSteps-1)) {
		t <- ii*dt
		N <- jj*dN
		m <- f(N, t);
		u <- t+(h*dt)
		v <- N + (h*dt)*m
		
		arrows(t, N, u, v, code=2, length=0.02)
		}	
	}
#Fixed points are shown where the slope of the arrow equals zero (ie, it's flat).  These occur for increasingly large N.  They are generally not stable, as indicated by the fact that their neighboring points above and below do not both point to them.  That is, fixed points are approached from the left by positive slope regions and followed on the right by negative slope regions.  

#########################################################################

# name: Ben Umans
# assignment: 3 b
# date: 10/30/09
# filename: NAME_OF_FILE
#############################################################################
f <- function(V) {
	return((1/.15)*(-V+58.1-100*V-6960))
	}
tRange <- c(0, 10)
dt <- 0.5
VRange <- c(-80, 80)
dV <- 2
h <- 0.5
k <- 0.0001

tSteps <- diff(range(tRange))/dt
VSteps <- diff(range(VRange))/dV

plot(c(0, 9), c(-80, 70), main='Two-Channel Conductance', xlab='t', ylab='I', fg=gray(0.6), yaxs='i')

for (ii in 0:(tSteps-1)) {
	for (jj in (-80:VSteps-1)) {
		t <- ii*dt
		V <- jj*dV
		m <- f(V);
		u <- t+(h*dt)
		g <- V + (k*h*dt)*m
		
		arrows(t, V, u, g, code=2, length=0.02)
		}	
	}
lines(c(0, 9), c(-68, -68))

###############################################################
# name: Ben Umans
# assignment: 4 b
# date: 10/30/09
# filename: NAME_OF_FILE
#############################################################################


logisticdiff <- function (x) {
 return((x/3)*(1-(x/5)))
 	} 
plot(logisticdiff, xlim=c(0, 20), ylim=c(-2, 2), main='Flow on the Line')

comparison <- numeric(length=20)


for (ii in 1:20) {
	comparison[ii] <- logisticdiff(ii)
	}


for (ii in 1:20) {
	 if (comparison[ii] > 0) {
 		arrows(ii, -1, (ii+1), -1, length=0.1, col='red')
 		} else {
 		arrows((ii+1), -1, ii, -1, length=0.1, col='blue')
 			}
	}


