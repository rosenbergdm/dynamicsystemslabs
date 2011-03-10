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


