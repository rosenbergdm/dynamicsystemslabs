###################################################
### chunk number 1: aa_SweaveListingsPreparations
###################################################
source('listingPreps.R');


###################################################
### chunk number 2: cobwebplot_func_1 eval=FALSE
###################################################
## cobImapFun <- function(x) {
##   return(2 * x - 0.5 * x ** 2);
## }
## max_iter <- 50;
## y_vector <- x_vector <- numeric(
##   max_iter * 2);
## y_vector[1] <- 0;
## x_vector[1] <- 0.5;
## 
## for(ii in 1:50) {
##   y_vector[2 * ii] <- cobImapFun(
##     x_vector[2 * ii - 1]);
##   y_vector[2 * ii + 1] <-
##     y_vector[2 * ii];
##   x_vector[2 * ii] <- 
##     x_vector[2 * ii - 1];
##   x_vector[2 * ii + 1] <- 
##     y_vector[2 * ii + 1];
## }
## 
## plot( -10:50 / 10, 
##       cobImapFun(-10:50 / 10),
##       xlab='x', 
##       ylab='y', 
##       main='Example cobweb',
##       type='l', 
##       xlim=c(0, 4),
##       ylim=c(0, 2.5), 
##       xaxs='i', 
##       yaxs='i');
## lines(-2:6, -2:6, col='red');
## lines(x_vector, y_vector, 
##       col='blue');


###################################################
### chunk number 3: cobwebplot_func_11
###################################################
cobImapFun <- function(x) {
  return(2 * x - 0.5 * x ** 2);
}
max_iter <- 50;
y_vector <- x_vector <- numeric(max_iter * 2);
y_vector[1] <- 0;
x_vector[1] <- 0.5;

for(ii in 1:50) {
  y_vector[2 * ii] <- cobImapFun(x_vector[2 * ii - 1]);
  y_vector[2 * ii + 1] <- y_vector[2 * ii];
  x_vector[2 * ii] <- x_vector[2 * ii - 1];
  x_vector[2 * ii + 1] <- y_vector[2 * ii + 1];
}

plot(-10:50 / 10, cobImapFun(-10:50 / 10),
     xlab='x', ylab='y', main='Example cobweb',
     type='l', xlim=c(0, 4),
     ylim=c(0, 2.5), xaxs='i', yaxs='i');
lines(-2:6, -2:6, col='red');
lines(x_vector, y_vector, col='blue');


