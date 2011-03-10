# name: Sheu, Bonnie
# assignment: Chapter 0 Lab Exercise 
# date:
# filename: Lab0_HW_BonnieSheu
##################################################################

##################################################################
# name: Sheu, Bonnie
# assignment: Chapter 0 Lab Exercise
# date: 
# question: 1
# subquestion: a
# other files: 
##################################################################
--

# Assign arbitrary numerical value to k

k <- 100
x0 <- 0
x1 <- 1

while (x1 <= k) {
  newX1 <- x0 + x1;
  x0 <- x1;
  x1 <- newX1;
}

if (x1 == k) {
  cat(k, ' is a fibonnaci number')
} else {
  cat(x0, ' is the greatest fibonnaci number <= ', k)
}

#/--
##################################################################
# name: Sheu, Bonnie
# assignment: Chapter 0 Lab Exercise
# date: 
# question: 1
# subquestion: b
# other files: 
##################################################################
--

# Assign arbitrary numerical values to k, x, and y

k <- 1
x <- 5
y <- 8

if (abs(k-x) > abs(k-y)) {
  cat(y, "is closer to k.")
} else if (abs(k-x) < abs(k-y)) {
  cat(x, "is closer to k.")
} else if (abs(k-x) == abs(k-y)) {
  cat(x, "is equidistant from k as y.")
}

#/--
##################################################################
# name: Sheu, Bonnie
# assignment: Chapter 0 Lab Exercise
# date: 
# question: 1
# subquestion: c
# other files: 
##################################################################
--

# Assign arbitrary numerical value to k

k <- 50
x0 <- 0
x1 <- 1

while (x1 < k) {
  newX1 <- x0 + x1;
  x0 <- x1;
  x1 <- newX1;
}

if (abs(k-x0) > abs(k-x1)) {
  cat(x1, "is the fibonacci number closest to k.")
} else if (abs(k-x0) < abs(k-x1)) {
  cat(x0, "is the fibonacci number closest to k.")
} else if (x1 == k) {
  cat(x1, "is a fibonnaci number.")
}

#/--
##################################################################
# name: Sheu, Bonnie
# assignment: Chapter 0 Lab Exercise
# date: 
# question: 2
# subquestion: a
# other files: 
##################################################################
--

# Assign arbitrary numerical values to vector members

vec1 <- c(10, 100)
vec2 <- c(50)

if (vec2 > vec1[2]) {
  cat(vec3 <- c(vec1[1], vec1[2], vec2))
} else if (vec2 < vec1[1]) {
  cat(vec3 <- c(vec2, vec1[1], vec1[2]))
} else if ((vec2 > vec1[1]) & (vec2 < vec1[2])) {
  cat(vec3 <- c(vec1[1], vec2, vec1[2]))
}

#/--
##################################################################
# name: Sheu, Bonnie
# assignment: Chapter 0 Lab Exercise
# date: 
# question: 2
# subquestion: b
# other files: 
##################################################################
--

# Assign arbitrary numerical values to vector members

vec1 <- c(10, 1)
vec2 <- c(min(vec1[1], vec1[2]), max(vec1[1], vec1[2]))

vec2


#/--
##################################################################
# name: Sheu, Bonnie
# assignment: Chapter 0 Lab Exercise
# date: 
# question: 2
# subquestion: c
# other files: 
##################################################################
--

vec1 <- round(runif(n=10, min=0, max=1000), digits=0)
vec1
vec2 <- c()

while (length(vec1) > 0) {
  min(vec1)
  vec2 <- c(vec2, min(vec1))
  nVec1 <- vec1 > max(vec2)
  vec1 <- c(vec1[nVec1[TRUE]])
}
vec2

#/--
##################################################################
# name: Sheu, Bonnie
# assignment: Chapter 0 Lab Exercise
# date: 
# question: 3
# subquestion: 
# other files: 
##################################################################
--

k <- 15
arg <- seq(from = 0, to = k-1, by = 1) * pi / k
mod <- c(rep(c(1, -1), k))[1:k]
solutions <- complex(modulus = mod, argument = arg)
solutions
solutions ^ 15

#/--
##################################################################