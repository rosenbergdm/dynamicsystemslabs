Chapter 2 Exercises
Erin Mowers
10-23-09


#Exercise 2a)

cobwebEq <- function (x) {
    y <- 3*x-((3*x^2)/4)+1
    return(y)
    }
max_iter <- 50
initial_value <- -.5

y_vector <- x_vector <- numeric(max_iter*2)
y_vector[1] <- 0
x_vector[1] <- initial_value
for (ii in 1:max_iter) {
    y_vector[2*ii] <- cobwebEq(x_vector[2*ii-1])
    y_vector[2*ii+1] <- y_vector[2*ii]
    x_vector[2*ii] <- x_vector[2*ii-1]
    x_vector[2*ii+1] <- y_vector[2*ii+1]
    }
plot(-10:max_iter / 10,
    cobwebEq(-10:max_iter / 10),
    xlab='x',
    ylab='f(x)',
    main='Mowers Exercise 2a',
    type='l',
    xlim=c(-2,6),
    ylim=c(-2,6),
    xaxs='i',
    yaxs='i')
lines(-6:6, -6:6, col='red')
lines(x_vector, y_vector, col='blue')





#Exercise 2b

cobwebEq <- function (x) {
    y <- 100*x-(2*x^2)
    return(y)
    }
max_iter <- 600
initial_value <- 48

y_vector <- x_vector <- numeric(max_iter*2)
y_vector[1] <- 0
x_vector[1] <- initial_value
for (ii in 1:max_iter) {
    y_vector[2*ii] <- cobwebEq(x_vector[2*ii-1])
    y_vector[2*ii+1] <- y_vector[2*ii]
    x_vector[2*ii] <- x_vector[2*ii-1]
    x_vector[2*ii+1] <- y_vector[2*ii+1]
    }
plot(-10:max_iter / 10,
    cobwebEq(-10:max_iter / 10),
    xlab='x',
    ylab='f(x)',
    main='Mowers Exercise 2b',
    type='l',
    xlim=c(-20,70),
    ylim=c(-100,1300),
    xaxs='i',
    yaxs='i')
lines(-250:1300, -250:1300, col='red')
lines(x_vector, y_vector, col='blue')


#Exercise 2c

cobwebEq <- function (x) {
    y <- ((-100)*x)+((x^2)/2)
    return(y)
    }
max_iter <- 600
initial_value <- -5

y_vector <- x_vector <- numeric(max_iter*2)
y_vector[1] <- 0
x_vector[1] <- initial_value
for (ii in 1:max_iter) {
    y_vector[2*ii] <- cobwebEq(x_vector[2*ii-1])
    y_vector[2*ii+1] <- y_vector[2*ii]
    x_vector[2*ii] <- x_vector[2*ii-1]
    x_vector[2*ii+1] <- y_vector[2*ii+1]
    }
plot(-100:max_iter,
    cobwebEq(-100:max_iter),
    xlab='x',
    ylab='f(x)',
    main='Mowers Exercise 2c',
    type='l',
    xlim=c(-50,250),
    ylim=c(-5500,1000),
    xaxs='i',
    yaxs='i')
lines(-6000:6000, -6000:6000, col='red')
lines(x_vector, y_vector, col='blue')



#Exercise 3

logisticEq <- function(x) {
    y <- INPUT
    return(y)
    }
max_iter <- INPUT
initial_value <- INPUT


cobweb_plot <- function(logisticEq) {
    y_vector <- x_vector <- numeric(max_iter*2)
    y_vector[1] <- 0
    x_vector[1] <- initial_value
    for (ii in 1:max_iter) {
        y_vector[2*ii] <- logisticEq(x_vector[2*ii-1])
        y_vector[2*ii+1] <- y_vector[2*ii]
        x_vector[2*ii] <- x_vector[2*ii-1]
        x_vector[2*ii+1] <- y_vector[2*ii+1]
        }
    plot(-10:max_iter / 10,
        logisticEq(-10:max_iter / 10),
        xlab='x_at_t',
        ylab='x_at_t+1',
        main='Mowers Exercise 3',
        type='l',
        xlim=c(0,20),
        ylim=c(0,100),
        xaxs='i',
        yaxs='i')
    lines(-1:150, -1:150, col='red')
    lines(x_vector, y_vector, col='blue')
    return('Please see attached plot')
    }



#Excerise 4c
G_0 <- 100
k <- 0.01
gluc_conc <- function(t) {
    G <- G_0 * exp(-k*t)
    return(G)
    }
plot(0:360,
    gluc_conc(0:360),
    xlab='time t (min)',
    ylab='Glucose concentration G (mg/dL)',
    main='Mowers Exercise 4c',
    type='l',
    xlim=c(0,360)
    )


#Exercise 5c
G_0 <- 100
k <- 0.01
a <- 4
gluc_conc <- function(t) {
    G <- a/k + (G_0-a/k)*exp(-k*t)
    return(G)
    }
plot(0:420,
    gluc_conc(0:420),
    xlab='time t (min)',
    ylab='Glucose concentration G (mg/dL)',
    main='Mowers Exercise 5c',
    type='l',
    xlim=c(0,420),
    ylim=c(0,450)
    )

