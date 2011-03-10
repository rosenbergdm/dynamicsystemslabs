Lab Exercise Chapter 1
Erin Mowers
10-16-09


#Exercise 1

inputEx<- INPUT A POLYNOMIAL EXPRESSION

result<-parsePolynomial(inputEx)
expoVector<- c(result[['exponents']])
coefVector<- c(result[['coefficients']])
a<- length(coefVector)
coefficients<- c(coefVector[1]*expoVector[1])
exponents<- c(expoVector[1]-1)
for (ii in 2:(a-1)){
    coef<- coefVector[ii]*expoVector[ii]
    expo<- expoVector[ii]-1
    coefficients<- c(coefficients, coef)
    exponents<- c(exponents, expo)
    }
deparsePolynomial(coefficients, exponents)



#Exercise 2a

diffEq <- function(x_at_t) {
    x_at_t1 <- x_at_t * (2 - (x_at_t / 2));
    return(x_at_t1);
    }
init_value <- 3
max_iter <- 10
x_array <- numeric(length=max_iter);
x_array[1] <- init_value;
for (ii in 2:max_iter) {
    new_value <- diffEq(x_array[ii-1]);
    x_array[ii] <- new_value
    }
fixed_points_x <- c(0,0)
fixed_points_y <- c(0,2)
y2 <- c(2,2,2,2,2,2,2,2,2,2)
plot(0:(max_iter-1), x_array, type='l', main='Mowers Excerise 2a', xlab='t', ylab='x(t)');
lines(0:(max_iter-1),y2, type='l', col='blue', lty=2)
points(fixed_points_x, fixed_points_y, pch=19, col='red', cex=1.5)
cat('See attached graph. The fixed points are x(t)=0 and x(t)=2 and are plotted at time t=0.
The red line on the graph demonstrates the carrying capacity of the population as 2 (the fixed point).
PLEASE NOTE: For this exercise, the inital value is 3.')



#Exercise2b

logisticEq <- function(x){
y <- INPUT;
return(y);
}
max_iter <- INPUT
initial_value <- INPUT

logisticEq_plot <- function(logisticEq){
    x_array <- numeric(max_iter);
    x_array[1] <- initial_value;
    for (ii in 2:max_iter) {
        new_value <- logisticEq(x_array[ii-1]);
        x_array[ii] <- new_value
        }
    plot(0:(max_iter-1), x_array, type='l',
        main='Mowers Logistic Equation Plot', xlab='t', ylab='x(t)');
    return(cat('See attached graph for iterated map.'))
    }
