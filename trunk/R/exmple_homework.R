#!/usr/bin/env r
# encoding: utf-8
# 
# name: Rosenberg, David M.
# assignment: 0
# date: 10/06/2008
# filename: rosenberg_homework_0.R
#############################################################################


#/--#########################################################################
# name: Rosenberg, David M.
# assignment: 0
# date: 10/06/2008
# question: 1
# subquestion: a
# other files: 
##########################################################################/--

x0 <- 0;
x1 <- 1;
while(x1 < k){
  next_value <- x0 + x1;
  x0 <- x1;
  x1 <- next_value;
}
if (x1 == k) {
  cat(k, ' is a fibonnaci number')
} else {
  cat(x0, ' is the largest fibonnaci number <= ', k)
}

#/--#########################################################################
# name: Rosenberg, David M.
# assignment: 0
# date: 10/06/2008
# question: 1
# subquestion: b
# other files: 
##########################################################################/--

if(abs(x-k) < abs(y-k)) {
  cat(x, ' is closer to ', k, '\n');
} else if (abs(x-k) == abs(y-k)) {
  cat(x, ' and ', y, ' are equidistant from ', k, '\n')
} else {
  cat(y, ' is closer to ', k, '\n')
}

#/--#########################################################################
# name: Rosenberg, David M.
# assignment: 0
# date: 10/06/2008
# question: 1
# subquestion: c
# other files: 
##########################################################################/--

x0 <- 0;
x1 <- 1;
while(x1 < k){
  next_value <- x0 + x1;
  x0 <- x1;
  x1 <- next_value;
}
if (x1 == k) {
  cat(k, ' is a fibonnaci number')
} else if(abs(x0-k) < abs(x1-k)){
  cat(x0, ' is the fibonnaci number closest to ', k)
} else {
  cat(x1, ' is the fibonnaci number closest to ', k)
}

#/--#########################################################################
# name: Rosenberg, David M.
# assignment: 0
# date: 10/06/2008
# question: 2
# subquestion: a
# other files: 
##########################################################################/--

## Given these two 'inputs'
sorted <- c(a, b) a < b
to_add <- d       new value
if(d < a) {
  c(d, a, b)
} else if (d < b) {
  c(a, d, b)
} else {
  c(a, b, d)
}

#/--#########################################################################
# name: Rosenberg, David M.
# assignment: 0
# date: 10/06/2008
# question: 2
# subquestion: b
# other files: 
##########################################################################/--

## unsorted input
unsorted <- c(a, b)
if (a <= b) {
  c(a, b)
} else {
  c(b, a)
}

#/--#########################################################################
# name: Rosenberg, David M.
# assignment: 0
# date: 10/06/2008
# question: 2
# subquestion: c
# other files: 
##########################################################################/--

## input
unsorted <- floor(runif(n=10, min=1, max=100))
unsorted
sorted <- numeric(length=length(unsorted))
for (ii in 1:9) {
  smallest <- unsorted[1]
  unsorted <- unsorted[-1]
  for (jj in 1:length(unsorted)) {
    if (unsorted[jj] < smallest) {
      temp <- unsorted[jj]
      unsorted[jj] <- smallest
      smallest <- temp
    }
  }
  sorted[ii] <- smallest;
}
sorted[10] <- unsorted
sorted

#/--#########################################################################
# name: Rosenberg, David M.
# assignment: 0
# date: 10/06/2008
# question: 3
# subquestion: 
# other files: rosenberg.solution_hint.pdf 
##########################################################################/--

k <- 15
args <- seq(from=0, to=k-1, by=1) * pi / k
mods <- c(rep(c(1, -1), k))[1:k]
solutions <- complex(modulus=mods, argument=args)
solutions
solutions ^ 15
