###################################################
### chunk number 1: aa_SweaveListingsPreparations
###################################################
source('./listingPreps.R');
# library('pgfSweave');


###################################################
### chunk number 2: noprompt
###################################################
options(prompt=' ', continue=' ')


###################################################
### chunk number 3: functor_def eval=FALSE
###################################################
## f1 <- function (x) 
##   x * (5 - 4 * x)
## f2 <- function (x) 
##   2 * x * (1 - 3 * x / 2)
## f3 <- function (x)
##   x / 2 - x^2 / 5
## f4 <- function (x)
##   x * (5/2 - 7 * x)
## # source('./maxima_utilities.R');


###################################################
### chunk number 4: functor_def2 eval=FALSE
###################################################
## plotCobWeb <- function (f, max_iter=50) {
##   ## initialize variables
##   f_exp <- deparse(body(f))
##   df <- mDeriv(f);
##   df_exp <- deparse(body(df))
##   fixed_pts <- sort(mSolve.RServe(paste(f_exp, 'x', sep='=')));
##   zeros <- sort(mSolve.RServe(paste(f_exp, '0', sep='=')));
## 
##   carrying_cap <- max(zeros - fixed_pts) ^ -1;
##   rate <- max(fixed_pts) * carrying_cap + 1;
##   
##   init_value <- min(zeros) + diff(zeros) / 4;
##   y <- x <- numeric(length=max_iter*2);
##   x[1] <- init_value;
##   y[1] <- 0;
## 
##   ## Loop over iterations
## 
##   for (ii in seq(along=1:max_iter)) {
##     y[2 * ii + c(0,1)] <- f(x[2 * ii - 1]);
##     x[2 * ii + c(0,1)] <- c(x[2 * ii - 1], y[2 * ii + 1]);
##   }
## 
##   ## Determine plot limits
##   if (all(is.finite(x) & is.finite(y))) {
##     xlim <- c(min(floor(zeros)), max(ceiling(zeros)));
##     ylim <- c(  max(c(0, min(f(c(zeros, fixed_pts, mean(zeros)))))),
##                 max(f(c(zeros, fixed_pts, mean(zeros)))))
##   } else {
##     yranges <- xranges <- c(1);
##     for (ii in 2:length(x)) {
##       xranges <- c(xranges, diff(range(x[1:ii])));
##       yranges <- c(yranges, diff(range(y[1:ii])));
##     }
##     xlim <- c(min(floor(zeros)), max(ceiling(zeros)));
##     ymagnitudes <- na.omit(yranges[-1] /  yranges[-length(yranges)])
##     first_runaway <- min( (1:length(ymagnitudes) )[ymagnitudes > 100])
##     
##     ylim <- c(0, max(c(y[1:(first_runaway - 1)], f(mean(zeros)), f(zeros), 
##               f(fixed_pts))));
##   }
## 
##   ## Draw plot elements
##   curve(f, from=xlim[1], to=xlim[2], ylim=ylim, xlim=c(min(zeros), 
##         max(zeros)), fg=gray(0.6), bty='n',col='red', xlab="$t$", 
##         ylab="$f(t)$");
##   abline(h=0, lwd=1.5);
##   abline(v=0, lwd=1.5);
##   abline(0, 1, col='blue', lwd=1);
##   lines(x, y, col='darkgreen', lwd=1);
## 
##   ########################################################
##   ##  Page break only: Function continues on next page  ##
##   ########################################################
## }


###################################################
### chunk number 5: functor_def3 eval=FALSE
###################################################
## {
##   ##########################################################
##   ##  Page break only: Function continues from last page  ##
##   ##########################################################
## 
##   ## Calculate label positions
##   typ <- text_y_positions <- mean(ylim) - diff(ylim) / 15 * c(4, 5, 6, 7, 8);
##   txp <- mean(zeros)
## 
##   ## Add plot description
##   title(main=paste("{\\larger\\bf Cobweb plot of $ f(x) = ",
##         gsub("\\*", "", f_exp),"$ }"));
##   text(   x=mean(zeros), y=typ[1], 
##           paste(  "Fixed points: $ \\{ ", paste(as.character(fixed_pts), 
##                 collapse=", "), " $ \\} \nCarrying capacity $ k=", 
##                 carrying_cap, " $"));
##   if(rate < 3 && rate > 1) {
##     text(x=txp, y=typ[3], paste('Rate: $ r= ', rate,
##                                 "\\quad \\rightarrow 1 < r < 3$"));
##     text(x=txp, y=typ[4], "{\\bf $\\therefore $ the fixed point is stable }");
## 
##   } else {
##     text(x=txp, y=typ[3], 
##          paste('Rate: $r = ', rate, "\\quad \\rightarrow r > 3$") );
##     text(x=txp, y=typ[4], 
##          "{\\bf $\\therefore $ the fixed point is unstable }");
##   }
## }


###################################################
### chunk number 6: functor_def3
###################################################
f1 <- function (x) 
  x * (5 - 4 * x)
f2 <- function (x) 
  2 * x * (1 - 3 * x / 2)
f3 <- function (x)
  x / 2 - x^2 / 5
f4 <- function (x)
  x * (5/2 - 7 * x)

plotCobWeb <- function (f, max_iter=50) {
  ## initialize variables
  f_exp <- deparse(body(f))
  df <- mDeriv(f);
  df_exp <- deparse(body(df))
  fixed_pts <- sort(mSolve.RServe(paste(f_exp, 'x', sep='=')));
  zeros <- sort(mSolve.RServe(paste(f_exp, '0', sep='=')));

  carrying_cap <- max(zeros - fixed_pts) ^ -1;
  rate <- max(fixed_pts) * carrying_cap + 1;
  
  init_value <- min(zeros) + diff(zeros) / 4;
  y <- x <- numeric(length=max_iter*2);
  x[1] <- init_value;
  y[1] <- 0;

  ## Loop over iterations

  for (ii in seq(along=1:max_iter)) {
    y[2 * ii + c(0,1)] <- f(x[2 * ii - 1]);
    x[2 * ii + c(0,1)] <- c(x[2 * ii - 1], y[2 * ii + 1]);
  }

  ## Determine plot limits
  if (all(is.finite(x) & is.finite(y))) {
    xlim <- c(min(floor(zeros)), max(ceiling(zeros)));
    ylim <- c(  max(c(0, min(f(c(zeros, fixed_pts, mean(zeros)))))),
                max(f(c(zeros, fixed_pts, mean(zeros)))))
  } else {
    yranges <- xranges <- c(1);
    for (ii in 2:length(x)) {
      xranges <- c(xranges, diff(range(x[1:ii])));
      yranges <- c(yranges, diff(range(y[1:ii])));
    }
    xlim <- c(min(floor(zeros)), max(ceiling(zeros)));
    ymagnitudes <- na.omit(yranges[-1] /  yranges[-length(yranges)])
    first_runaway <- min( (1:length(ymagnitudes) )[ymagnitudes > 100])
    
    ylim <- c(0, max(c(y[1:(first_runaway - 1)], f(mean(zeros)), f(zeros), 
              f(fixed_pts))));
  }

  ## Draw plot elements
  curve(f, from=xlim[1], to=xlim[2], ylim=ylim, xlim=c(min(zeros), 
        max(zeros)), fg=gray(0.6), bty='n',col='red', xlab="$t$", 
        ylab="$f(t)$");
  abline(h=0, lwd=1.5);
  abline(v=0, lwd=1.5);
  abline(0, 1, col='blue', lwd=1);
  lines(x, y, col='darkgreen', lwd=1);

  ## Calculate label positions
  typ <- text_y_positions <- mean(ylim) - diff(ylim) / 15 * c(4, 5, 6, 7, 8);
  txp <- mean(zeros)

  ## Add plot description
  title(main=paste("{\\larger\\bf Cobweb plot of $ f(x) = ",
        gsub("\\*", "", f_exp),"$ }"));
  text(   x=mean(zeros), y=typ[1], 
          paste(  "Fixed points: $ \\{ ", paste(as.character(fixed_pts), 
                collapse=", "), " $ \\} \nCarrying capacity $ k=", 
                carrying_cap, " $"));
  if(rate < 3 && rate > 1) {
    text(x=txp, y=typ[3], paste('Rate: $ r= ', rate,
                                "\\quad \\rightarrow 1 < r < 3$"));
    text(x=txp, y=typ[4], "{\\bf $\\therefore $ the fixed point is stable }");

  } else {
    text(x=txp, y=typ[3], 
         paste('Rate: $r = ', rate, "\\quad \\rightarrow r > 3$") );
    text(x=txp, y=typ[4], 
         "{\\bf $\\therefore $ the fixed point is unstable }");
  }
}


###################################################
### chunk number 7: functor_def11
###################################################
f <- f1;
plotCobWeb(f)


###################################################
### chunk number 8: functor_def12
###################################################
f <- f2
plotCobWeb(f2)


###################################################
### chunk number 9: functor_def13
###################################################
f <- f3
plotCobWeb(f)


###################################################
### chunk number 10: functor_def14
###################################################
f <- f4
plotCobWeb(f)


###################################################
### chunk number 11: nonhomogenous_part2 eval=FALSE
###################################################
## glucose <- function(t) {
##   100 *  exp(-1 * t / 100) * (4 * exp(t / 100) - 3)
## }
## 
## xlab <- "time $(t)$ {\\smaller (minutes)}"
## ylab <- "Blood glucose concentration $G(t)$ {\\smaller(mg / dl)}"
## main <- "$100 \\, {(4 \\, e^{\\frac{1}{100} \\, t} - 3)} e^{-\\frac{1}{100} \\, t}$"
## plot(glucose, from=0, to=800, xlab=xlab, ylab=ylab, main=main);


###################################################
### chunk number 12: nonhomogenous_part3
###################################################
glucose <- function(t) {
  100 *  exp(-1 * t / 100) * (4 * exp(t / 100) - 3)
}

xlab <- "time $(t)$ {\\smaller (minutes)}"
ylab <- "Blood glucose concentration $G(t)$ {\\smaller(mg / dl)}"
main <- "$100 \\, {(4 \\, e^{\\frac{1}{100} \\, t} - 3)} e^{-\\frac{1}{100} \\, t}$"
plot(glucose, from=0, to=800, xlab=xlab, ylab=ylab, main=main);


