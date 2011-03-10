# 
# name: Erin Mowers
# assignment: Chapter 5 Exercise
# date: 11/23/09
# filename: Mowers_Ch5_Exercise
#############################################################################







#/--#########################################################################
# name: Erin Mowers
# assignment: Ch. 5
# date: 11/23/09
# question: 1
# subquestion: a
# other files: NA
##########################################################################/--

logisticModel <- function(x, r){
y <- r * x * (1 - x);
return(y);
}


solve <- function(func, r, x_initial){
    x_array <- numeric(500);
    x_array[1] <- x_initial;
    for (ii in 2:500) {
        x_array[ii] <- func(x_array[ii-1], r);
    }
    return(x_array)
}

#func = logistic Model



#/--#########################################################################
# name: Erin Mowers
# assignment: Ch. 5
# date: 11/23/09
# question: 1
# subquestion: b & c
# other files: See print-out of graph.
##########################################################################/--

Bifurcation <- function(func, x_initial) {
  r_initial <- 0
  r_max <- 4
  step <- 0.01
  iter <- (r_max - r_initial) / step
  r <- c(length=iter)
  plot(c(0, r_max), c(0,1), type='n',
    main='Mowers 1b&c: Bifurcation',
    xlab='r', ylab='x')
  for (ii in 1:iter) {
    r[ii] = r_initial + ((ii - 1) * step)
    Xevery <- func(logisticModel, r[ii], x_initial)
    Xstab <- c(Xevery[201:500])
    for (jj in 1:(length(Xstab))) {
      points(r[ii], Xstab[jj])
    }
  }
}

    
#func = solv
Bifurcation(solve, 0.5) 



#/--#########################################################################
# name: Erin
# assignment: Ch. 5
# date: 11/23/09
# question: 1
# subquestion: d
# other files: NA
##########################################################################/-


logisticModel_plot <- function(func, x_initial, r) {
  x_plot <- numeric(300);
  x_all <- numeric(500);
  x_all[1] <- x_initial;
  for (ii in 2:500) {
    x_all[ii] <- func(x_all[ii-1], r);
    if (ii > 200) {
      x_plot[ii-200] <- x_all[ii]
    }
  }
  plot(c(201:500), x_plot, type='l',
    main='Mowers 1d: Representative Behavior',
    xlab='time t', ylab='x')
  return(cat('Please see plot'))
}
  
#func = logisticModel

#Single fixed point:
logisticModel_plot(logisticModel, 0.5, 2)

#Two-cycle:
logisticModel_plot(logisticModel, 0.5, 3.2)

#N-cycle:
logisticModel_plot(logisticModel, 0.5, 3.5)

#chaos:
logisticModel_plot(logisticModel, 0.5, 3.999999)




#/--#########################################################################
# name: Erin Mowers
# assignment: Ch. 5
# date: 11/23/09
# question: 2
# subquestion: a
# other files: See handwritten pages.
##########################################################################/-




#/--#########################################################################
# name: Erin Mowers
# assignment: Ch. 5
# date: 11/23/09
# question: 2
# subquestion: b
# other files: NA
##########################################################################/-

population <- function(x_initial, y_initial, generations) {
  f1 <- 0.5
  f2 <- 2
  s1 <- 0.1
  s2 <- 0
  x_group <- y_group <- numeric(generations)
  x_group[1] <- x_initial
  y_group[1] <- y_initial
  for (ii in 2:generations) {
    x_group[ii] <- (f1 * x_group[ii-1]) + (f2 * y_group[ii-1])
    y_group[ii] <- (s1 * x_group[ii-1]) + (s2 * y_group[ii-1])
  }
  plot(1:generations, x_group, type='l', col='red',
    main='Mowers 2b: Population propagation',
    xlab='time in generations',
    ylab='number of individuals')
  lines(1:generations, y_group, type='l', col='green')
  text(0.5 * generations, 0, 'X is in red and Y is in green')
  return(cat('The number of members of x for the generations specified is:','
    ', x_group,'
    ', 'The number of members of y for the generations specified is:', '
    ', y_group))
}




#/--#########################################################################
# name: Erin Mowers
# assignment: Ch. 5
# date: 11/23/09
# question: 2
# subquestion: c
# other files: See handwritten pages for print outs of graphs.
##########################################################################/-

#This plots relative fractions of each population.
#It doesn't answer the question, but I couldn't bear to destroy it.
population_plot <- function(x_initial, y_initial, generations) {
  f1 <- 0.5
  f2 <- 2
  s1 <- 0.1
  s2 <- 0
  x_group <- y_group <- numeric(generations)
  x_group[1] <- x_initial
  y_group[1] <- y_initial
  for (ii in 2:generations) {
    x_group[ii] <- (f1 * x_group[ii-1]) + (f2 * y_group[ii-1])
    y_group[ii] <- (s1 * x_group[ii-1]) + (s2 * y_group[ii-1])
  }
  total_pop <- x_group + y_group
  x_fraction <- x_group / total_pop
  y_fraction <- y_group / total_pop
  plot(c(0,generations), c(0, 1), type='n',
    main='Mowers 2c: Population distribution over time',
    xlab='time in generations',
    ylab='population distribution (relative)')
  lines(c(1:generations), x_fraction, type='l', col='red')
  lines(c(1:generations), y_fraction, type='l', col='green')
  text(0.5 * generations,0,'X fraction is in red, Y fraction is in green')
  return(cat('Please see graph. X fraction is red, Y fraction is green','
    ','X fraction converges to:', x_fraction[generations],'
    ', 'Y fraction converges to:', y_fraction[generations]))
}

#This plots growth rate for each X and Y over time to look at convergence
population_growth <- function(x_initial, y_initial, generations) {
  f1 <- 0.5
  f2 <- 2
  s1 <- 0.1
  s2 <- 0
  x_group <- y_group <- numeric(generations)
  x_growth <- y_growth <-tot_growth <- numeric(generations -1)
  x_group[1] <- x_initial
  y_group[1] <- y_initial
  for (ii in 2:generations) {
    x_group[ii] <- (f1 * x_group[ii-1]) + (f2 * y_group[ii-1])
    y_group[ii] <- (s1 * x_group[ii-1]) + (s2 * y_group[ii-1])
    x_growth[ii - 1] <- (x_group[ii] / x_group[ii-1]) 
    y_growth[ii - 1] <- (y_group[ii] / y_group[ii -1])
    tot_growth[ii -1] <- (x_group[ii] + y_group[ii]) /
      (x_group[ii - 1] + y_group[ii - 1])
  }
  plot(2:generations, tot_growth, type='l', col='blue',
    main='Mowers 2c: Population distribution over time',
    xlab='time in generations',
    ylab='growth rate')
  lines(2:generations, x_growth, col='red')
  lines(2:generations, y_growth, col='green')
  text(0.5 * (generations - 1), 0, 'Total growth is blue, X is red, Y is green')
  return(cat('The growth rate is converging to:', '
    ', x_growth[generations-1], 'for x', '
    ', 'and', y_growth[generations-1], 'for y', '
    ', 'and', (x_growth[generations - 1] + y_growth[generations -1]),
    'for total population'))
}

##To answer the question, use:
population_growth(0,1,20)
population_growth(1,0,20)
population_growth(2,5,20)
population_growth(1,1,20)






#/--#########################################################################
# name: Erin Mowers
# assignment: Ch. 5
# date: 11/23/09
# question: 2
# subquestion: d
# other files: See handwritten pages.
##########################################################################/-




#/--#########################################################################
# name: Erin Mowers
# assignment: Ch. 5
# date: 11/23/09
# question: 3
# subquestion: a
# other files: See handwritten pages.
#########################################################################/-


#/--#########################################################################
# name: Erin Mowers
# assignment: Ch. 5
# date: 11/23/09
# question: 3
# subquestion: b
# other files: See handwritten pages.
#########################################################################/-

pop_tot <- function(x_initial, y_initial, a, generations) {
  f1 <- 0.5
  f2 <- 2
  s1 <- 0.1
  s2 <- a
  x_group <- y_group <- numeric(generations)
  x_growth <- y_growth <- numeric(generations -1)
  x_group[1] <- x_initial
  y_group[1] <- y_initial
  for (ii in 2:generations) {
    x_group[ii] <- (f1 * x_group[ii-1]) + (f2 * y_group[ii-1])
    y_group[ii] <- (s1 * x_group[ii-1]) + (s2 * y_group[ii-1])
    total_group <- x_group + y_group
  }
  return(total_group)
}

varyingA <- function(func_tot, x_initial, y_initial) {
  generations <- 500
  step <- 0.05
  aMax <- 1
  iter <- aMax/step
  aVector <- yVals <- numeric(iter)
  aVector[1] <- 0
  totVals <- numeric(generations)
  for (ii in 2:iter) {
    aVector[ii] <- aVector[ii-1] + step
  }
  plot(c(0,1), c(0,10000), type='n',
    main='Mowers 3b: Total population varying a',
    xlab='value of parameter a',
    ylab='total population')
  for (ii in 1:iter) {
    totVals <- func_tot(x_initial, y_initial, aVector[ii], generations)
    yVals[ii] <- totVals[generations]
  }
  points(aVector, yVals)
  lines(aVector, yVals)
  return('See graph')
}

##func_tot = pop_tot
#To examine the convergence behavior, use:
varyingA(pop_tot, 100, 40)



#To find eigenvalues & vectors:

eigen(matrix(c(0.5, 2, 0.1, 0),nrow=2))
#Largest eigenvalue is 0.7623475

eigen(matrix(c(0.5, 2, 0.1, 0.2),nrow=2))
#Largest eigenvalue is 0.8216991

eigen(matrix(c(0.5, 2, 0.1, 0.4),nrow=2))
#Largest eigenvalue is 0.9

eigen(matrix(c(0.5, 2, 0.1, 0.6),nrow=2))
#Largest eigenvalue is 1

eigen(matrix(c(0.5, 2, 0.1, 0.8),nrow=2))
#Largest eigenvalue is 1.1216991

eigen(matrix(c(0.5, 2, 0.1, 1),nrow=2))
#Largest eigenvalue is 1.2623475


#/--#########################################################################
# name: Erin Mowers
# assignment: Ch. 5
# date: 11/23/09
# question: 3
# subquestion: c
# other files: See printed plots.
#########################################################################/-
#

#The bifurcation point is approximately a=0.6, where the largest eigenvalue is 1.

population <- function(x_initial, y_initial, generations, a) {
  f1 <- 0.5
  f2 <- 2
  s1 <- 0.1
  s2 <- a
  x_group <- y_group <- numeric(generations)
  x_group[1] <- x_initial
  y_group[1] <- y_initial
  for (ii in 2:generations) {
    x_group[ii] <- (f1 * x_group[ii-1]) + (f2 * y_group[ii-1])
    y_group[ii] <- (s1 * x_group[ii-1]) + (s2 * y_group[ii-1])
  }
  total_pop <- x_group +y_group
  return(plot(1:generations, total_pop, type='l', col='red',
    main='Mowers 3c: Population propagation',
    xlab='time in generations',
    ylab='number of individuals'))
}
  lines(1:generations, y_group, type='l', col='green')
  text(0.5 * generations, 0, 'X is in red and Y is in green')
  return(cat('The number of members of x for the generations specified is:','
    ', x_group,'
    ', 'The number of members of y for the generations specified is:', '
    ', y_group))
}
 
    
#To look at representative solutions, use:
population(100, 50, 20, 0.5)
population(100, 50, 20, 0.6)
population(100, 50, 20, 0.7)





#/--#########################################################################
# name: STUDENT_NAME
# assignment: ASSIGNMENT_NUMBER
# date: DATE_OF_SUBMISSION
# question: QUESTION_NUMBER
# subquestion: SUBQUESTION_LETTER_IF_PRESENT_OTHERWISE_EMPTY
# other files: NAMES_OF_OTHER_RELATED_FILES
##########################################################################/--


#/--#########################################################################
# name: STUDENT_NAME
# assignment: ASSIGNMENT_NUMBER
# date: DATE_OF_SUBMISSION
# question: QUESTION_NUMBER
# subquestion: SUBQUESTION_LETTER_IF_PRESENT_OTHERWISE_EMPTY
# other files: NAMES_OF_OTHER_RELATED_FILES
##########################################################################/-df
#



