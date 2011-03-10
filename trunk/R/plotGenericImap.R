#!/usr/bin/env r
# encoding: utf-8
# locale: en-US


#' plotGenericImap - Functor for plotting an iterated map from any logistic
#'   equation
#' Arguments:
#'   fun - function representing the logistic difference equation
#'   nIter - number of iterations (default=20)
#' Returns:
#'   None
plotGenericImap <- function(fun, nIter=20) {
  ## Finding a 'safe' initial value
  iVal <- 1;                              # Start with 0.5
  iiVal <- iVal / 2;
  while (fun(iVal) <= 0 | fun(iiVal) == iiVal) {
    ## If the function value of f(iVal) is negative OR if (iiVal, f(iiVal))
    ##   falls along the line of y=x, pick a smaller value.
    iVal <- iVal / 10;
    iiVal <- iVal / 2;
  }
  
  xRange <- numeric(length=2);
  xRange[1] <- 0;
  
  ## Find a good range to plot over
  xMax <- iiVal;                          # Start with xlim=c(0, iiVal);
  while (fun(xMax) >= 0) {
    ## If f(iiVal) is positive, pick a (slightly) larger value
    xMax <- 1.25 * xMax;
  }
  xRange[2] <- xMax;
  
  ## Initialize arrays
  yArr <- numeric(length=nIter * 2);
  xArr <- yArr;
  xArr[1] <- 0;
  yArr[1] <- iiVal;
  
  ## Run main iterative loop
  for (ii in 1:nIter) {
    yArr[2 * ii] <- fun(yArr[2 * ii - 1]);
    yArr[2 * ii + 1] <- yArr[2 * ii];
    xArr[2 * ii] <- xArr[2 * ii - 1];
    xArr[2 * ii + 1] <- yArr[2 * ii];
  }
  
  ## Plot the function.  I use bty='n' and fg=grey(0.6) to make prettier 
  ##   axes.  I then draw the x=0, y=0 axes using abline
  curve(fun, xRange[1], xRange[2], bty='n', fg=grey(0.6));
  abline(v=0);
  abline(h=0);
  ## Draw the line y=x.  The xRange[1]-100, xRange[2] + 100 ensures that the
  ##   line extends throughout the plot range.
  lines(c(xRange[1] - 100, xRange[2] + 100), 
        c(xRange[1] - 100, xRange[2] + 100), col='red');
  ## Draw the 'cobweb portion'
  lines(xArr, yArr, col='green');
  
  ## Since no value is returned, we can omit the return statement.
}
