1. (a) Starting with the ﬁbonacci sequence examples from the tutorial, write a code chunk which takes as input a number k and ﬁnds the largest ﬁbonacci number less than or equal to k .

k <- _          # Here, the '_' should be replaced with the desired k.
x0 <- 0         # Definition of initial terms
x1 <- 1
while(x1 < k) {     # Start of while loop that defines the next fibonacci
   newX1 <- x0 + x1    # number, until the largest is greater than k.
   x0 <- x1
   x1 <- newX1
}
if (x1 == k) {        # If k happens to be a fibonacci number, this
   x1          # command displays k (x1).
} else {          # Otherwise, the code displays the second-to-last
   x0          # term (x0), which will be the largest fibonacci
}              # number </= k.


(b) Write a code chunk which takes as input three numbers (say, k, x, and y) and prints either the second number (x) or the third number (y) depending on which is closer to k.

k <- _                    # Input of k, x, and y by replacing '_'
x <- _
y <- _
a <- abs(k-x)                   # These two commands find the absolute
b <- abs(k-y)                   # value of the difference from k of x and y.
if (a == b) {                   # These if clauses simply sort out the possible
   cat(x, "and", y, "are equidistant from", k, ".")  # results and provide a sentence explaining
} else if (min(a, b) == a) {           # which variable (x or y) is closest to k.
   cat(x, "is closer to", k, "than", y, ".")
} else {
   cat(y, "is closer to", k, "than", x, ".")
} 


(c) Write a code chunk which takes as input a number k and returns the ﬁbonacci number which is closest to k. 

k <- _          # Input of k
x0 <- 0         # Definition of starting terms for fibonacci sequence
x1 <- 1
while(x1 < k) {     # Start of loop defining each successive term of the
   newX1 <- x0 + x1    # fibonacci sequence until the last term is greater
   x0 <- x1        # than k.
   x1 <- newX1
}
a <- abs(k-x1)      # Next, we apply the previous code to our two closest
b <- abs(k-x0)      # fibonacci numbers, x0 and x1.
if (x1 == k) {                  # The last clauses once again sort out all
   cat(x1, "is a fibonacci number.")     # possibilities: k is a fibonacci number, x1 is closer,
} else if (a == b) {               # x0 is closer, or x0 and x1 are equidistant from k.
   cat("The fibonacci numbers", x0, "and", x1, "are equidistant from", k, ".")
} else if (min(a, b) == a) {
   cat(x1, "is the closest fibonacci number to", k, ".")
} else {
   cat(x0, "is the closest fibonacci number to", k, ".")
}


2. (a) Write a code chunk which takes a “sorted” numeric vector of length 2 and another numeric vector of length 1 and prints a single “sorted” vector of length 3. 

vec1 <- c(_, _)             # Input of two vectors, one sorted of length two,
vec2 <- c(_)               # another of length one.
if (vec1[1] < vec2 & vec1[2] < vec2) {    # Then, three possibilities in this if clause determine
  newVec <- c(vec1, vec2)            # how the new vector will be constructed: if vec2 is
} else if (vec1[1] > vec2 & vec1[2] > vec2) {   # less than both numbers in vec1, it goes first; if it
  newVec <- c(vec2, vec1)            # is greater than both, it goes last; otherwise, it is placed
} else if (vec1[1] < vec2 & vec1[2] > vec2) {   # in the middle of the new vector.
  newVec <- c(vec1[1], vec2, vec1[2])
}
newVec                  # New vector is printed


(b) Write a code chunk which takes an “unsorted” numeric vector of length 2 and prints the values of that vector, “sorted.” 

vec1 <- c(_, _)         # Input of unsorted vector of length two.
if (vec1[2] < vec1[1]) {        # Next, if the second term is smaller than the first, the two
  newVec <- c(vec1[2], vec1[1]) # are swapped.
  newVec
} else {              # Otherwise, the vector is simply reprinted.
  vec1
}


(c) Write a code chunk that takes an “unsorted” numeric vector of length 10 and prints the sorted values to the screen. 

vec1 <- c(_, _, _, _, _, _, _, _, _, _)       # Input of an unsorted vector of length 10.
while (vec1[1]>vec1[2] | vec1[2]>vec1[3] |     # Start of while loop that checks if any term
  vec1[3]>vec1[4] | vec1[4]>vec1[5] |       # in vec1 is larger than the term immediately
  vec1[5]>vec1[6] | vec1[6]>vec1[7] |       # after it. If this is true for any term, the following
  vec1[7]>vec1[8] | vec1[8]>vec1[9] |       # chunk of if clauses check each term and its
  vec1[9]>vec1[10]) {               # predecessor, and swap them if the first is larger.
   if (vec1[2] < vec1[1]) {
   vec1 <- c(min(vec1[1:2]), 
   max(vec1[1:2]), vec1[3:10])
   }
   if (vec1[3] < vec1[2]) {
   vec1 <- c(vec1[1], min(vec1[2:3]), 
   max(vec1[2:3]), vec1[4:10])
   }
   if (vec1[4] < vec1[3]) {
   vec1 <- c(vec1[1:2], min(vec1[3:4]), 
   max(vec1[3:4]), vec1[5:10])
   }
   if (vec1[5] < vec1[4]) {
   vec1 <- c(vec1[1:3], min(vec1[4:5]), 
   max(vec1[4:5]), vec1[6:10])
   }
   if (vec1[6] < vec1[5]) {
   vec1 <- c(vec1[1:4], min(vec1[5:6]), 
   max(vec1[5:6]), vec1[7:10])
   }
   if (vec1[7] < vec1[6]) {
   vec1 <- c(vec1[1:5], min(vec1[6:7]), 
   max(vec1[6:7]), vec1[8:10])
   }
   if (vec1[8] < vec1[7]) {
   vec1 <- c(vec1[1:6], min(vec1[7:8]), 
   max(vec1[7:8]), vec1[9:10])
   }
   if (vec1[9] < vec1[8]) {
   vec1 <- c(vec1[1:7], min(vec1[8:9]), 
   max(vec1[8:9]), vec1[10])
   }
   if (vec1[10] < vec1[9]) {
   vec1 <- c(vec1[1:8], min(vec1[9:10]), 
   max(vec1[9:10]))
   }
}                        # Once the while loop's condition returns 'FALSE,'
vec1                   # the vector has been fully sorted and is printed.


3. Consider the equation:
      x^k -1 = 0,  x ∈ C,  k ∈ N 
  Write a code chunk that takes in integer (k) as input and prints all values x which satisfy the above equation. 

k <- _                # Input of k
if (k == 1) {               # If clause returns the only solution to k = 1 separately
   c(1 + 0i)             # as it does not work in the following chunk of code…
} else if (k < 0) {            # …as well as stipulating that k must be positive.
   cat("k must be a natural number.")
} else {                # Otherwise, the following for loop continually adds the
   psi_Vec <- c(0, (2 * pi / k))      # incremental value between psi(ii) and psi(ii+1), in this case
   for (ii in 3:k) {           # 2π/k, until k (or the point at which redundancy is reached).
   psi_incr <- xVec[2]       # Each solution is ultimately defined in a vector…
   psi_minus_1 <- psi_Vec[ii-1]
   new_psi <- psi_incr + psi_minus_1
   psi_Vec <- c(psi_Vec, new_psi)
   }
   exp(psi_Vec * 1i)         # …which is then plugged back in to the final
}                    # generalized solution and reprinted.

