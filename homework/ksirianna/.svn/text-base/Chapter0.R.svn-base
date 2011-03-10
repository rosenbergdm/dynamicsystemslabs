# Katie Sirianni
# Lab 0

Exercise 1:
a)Starting with the bonacci sequence examples from the tutorial, write a code chunk which takes
as input a number k and nds the largest bonacci number less than or equal to k

k<-
 x0 <- 0
 x1 <- 1
 while (x1 <k)  {
           newX1 <- x0 +x1
           x0 <- x1
           x1 <- newX1
}
cat(x0, "is the largest fibonacci number lower than k") 



b)Write a code chunk which takes as input three numbers (say, k, x, and y) and prints either the
second number (x) or the third number (y) depending on which is closer to k. (Hint: the functions
min(), max(), and abs() may be helpful. See the online documentation for their usage.)

k<-
x<-
y<-
  if(abs(x-k) < abs(y-k)) {
         cat(x, "is closest to k")
} else{
         cat(y, "is closest to k")
}

c) Write a code chunk which takes as input a number k and returns the bonacci number with is
closest to k.
k<-
 x0<- 0
 x1<-1
  while(x1 < k) {
         newX1 <- x0 + x1
         x0<-x1
         x1<-newX1
}
  if(abs(x0-k)<abs(newX1-k)) {
         cat(x0, "is the fibonacci number closest to k")
} else {
          cat(newX1, "is the fibonacci number closest to k")
}


Exercise 2:
a)Write a code chunk which takes a \sorted" numeric vector of length 2 and another numeric vector
of length 1 and prints a single \sorted" vector of length 3.
a<-
b<-                    # a and b will make up vector 1 (v1)
d<-                    # the number in vector 2 (v2)
v1<- c(a,b)
sortedV1<-sort(v1)
v2<- d
v3<- c(sortedV1,v2)         #this combines, but doesn't sort, the two vectors
finalVector<- sort(v3)

And without the sorted function:
a<-
b<-
c<-
v1<- c(a,b)  #where a is less than b
v2<-c
sortedVector<- c(min(v1),v2,max(v1))
sortedVector

b) Write a code chunk which takes an \unsorted" numeric vector of length 2 and prints the values of
that vector, \sorted."
a<- 
b<-
unsortedVector<-c(a,b)    #is it possible to have an unsorted vector of two numbers?
sortedVector<-sort(unsortedVector)

And without using the sort function:
a<-
b<-
v1<-c(a,b)
sortedVector<-c(min(v1),max(v1))
sortedVector

c)Write a code chunk that takes an \unsorted" numeric vector of length 10 and prints the sorted
values to the screen.

vector<- 1:100
uVec<- sample(vector,10)
uVec
sVec<-uVec[1]
   if(uVec[2]<uVec[1]) {
   sVec<-c(uVec[2], sVec)
 }  else{ 
  sVec<- c(sVec, uVec[2])
}
if(uVec[3]< min(sVec)) {
   sVec<-c(uVec[3], sVec)
 }  else if(uVec[3]<max(sVec)) {
  sVec<- c(sVec[1],uVec[3],sVec[2])
} else{ 
  sVec<- c(sVec, uVec[3])
}
if(uVec[4]< min(sVec)) {
   sVec<-c(uVec[4], sVec)
 }  else if(min(sVec)<uVec[4] & uVec[4] <sVec[2]) {
   sVec<- c(sVec[1], uVec[4],sVec[2],sVec[3])
} else if(sVec[2]<uVec[4] & uVec[4]<sVec[3]) {
  sVec<- c(sVec[1], sVec[2], uVec[4], sVec[3])
} else { 
  sVec<- c(sVec, uVec[4])
}
if(uVec[5]< min(sVec)) {
   sVec<-c(uVec[5], sVec)
 }  else if(min(sVec)<uVec[5] & uVec[5] <sVec[2]) {
   sVec<- c(sVec[1], uVec[5],sVec[2],sVec[3], sVec[4])
} else if(sVec[2]<uVec[5] & uVec[5]<sVec[3]) {
  sVec<- c(sVec[1], sVec[2], uVec[5], sVec[3], sVec[4])
} else if(sVec[3]<uVec[5] & uVec[5]< sVec[4]) { 
     sVec<-c(sVec[1],sVec[2],sVec[3],uVec[5],sVec[4])
} else{
  sVec<- c(sVec, uVec[5])
}
if(uVec[6]< min(sVec)) {
   sVec<-c(uVec[6], sVec)
 }  else if(min(sVec)<uVec[6] & uVec[6] <sVec[2]) {
   sVec<- c(sVec[1], uVec[6],sVec[2],sVec[3], sVec[4], sVec[5])
} else if(sVec[2]<uVec[6] & uVec[6]<sVec[3]) {
  sVec<- c(sVec[1], sVec[2], uVec[6], sVec[3], sVec[4], sVec[5])
} else if(sVec[3]<uVec[6] & uVec[6]< sVec[4]) { 
     sVec<-c(sVec[1],sVec[2],sVec[3],uVec[6],sVec[4], sVec[5])
} else if(sVec[4]<uVec[6] & uVec[6]< sVec[5]) { 
     sVec<-c(sVec[1],sVec[2],sVec[3],sVec[4],uVec[6], sVec[5])
} else{
  sVec<- c(sVec, uVec[6])
}
if(uVec[7]< min(sVec)) {
   sVec<-c(uVec[7], sVec)
 }  else if(min(sVec)<uVec[7] & uVec[7] <sVec[2]) {
   sVec<- c(sVec[1], uVec[7],sVec[2],sVec[3], sVec[4], sVec[5], sVec[6])
} else if(sVec[2]<uVec[7] & uVec[7]<sVec[3]) {
  sVec<- c(sVec[1], sVec[2], uVec[7], sVec[3], sVec[4], sVec[5], sVec[6])
} else if(sVec[3]<uVec[7] & uVec[7]< sVec[4]) { 
     sVec<-c(sVec[1],sVec[2],sVec[3],uVec[7],sVec[4], sVec[5], sVec[6])
} else if(sVec[4]<uVec[7] & uVec[7]< sVec[5]) { 
     sVec<-c(sVec[1],sVec[2],sVec[3],sVec[4],uVec[7], sVec[5], sVec[6])
} else if(sVec[5]<uVec[7] & uVec[7]< sVec[6]) { 
     sVec<-c(sVec[1],sVec[2],sVec[3],sVec[4],sVec[5], uVec[7], sVec[6])
} else{
  sVec<- c(sVec, uVec[7])
}
if(uVec[8]< min(sVec)) {
   sVec<-c(uVec[8], sVec)
 }  else if(min(sVec)<uVec[8] & uVec[8] <sVec[2]) {
   sVec<- c(sVec[1], uVec[8],sVec[2],sVec[3], sVec[4], sVec[5], sVec[6],sVec[7])
} else if(sVec[2]<uVec[8] & uVec[8]<sVec[3]) {
  sVec<- c(sVec[1], sVec[2], uVec[8], sVec[3], sVec[4], sVec[5], sVec[6], sVec[7])
} else if(sVec[3]<uVec[8] & uVec[8]< sVec[4]) { 
     sVec<-c(sVec[1],sVec[2],sVec[3],uVec[8],sVec[4], sVec[5], sVec[6], sVec[7])
} else if(sVec[4]<uVec[8] & uVec[8]< sVec[5]) { 
     sVec<-c(sVec[1],sVec[2],sVec[3],sVec[4],uVec[8], sVec[5], sVec[6], sVec[7])
} else if(sVec[5]<uVec[8] & uVec[8]< sVec[6]) { 
     sVec<-c(sVec[1],sVec[2],sVec[3],sVec[4],sVec[5], uVec[8], sVec[6], sVec[7])
} else if(sVec[6]<uVec[8] & uVec[8]<sVec[7]) { 
     sVec<-c(sVec[1],sVec[2],sVec[3],sVec[4],sVec[5], sVec[6], uVec[8], sVec[7])
} else{
  sVec<- c(sVec, uVec[8])
}
if(uVec[9]< min(sVec)) {
   sVec<-c(uVec[9], sVec)
 }  else if(min(sVec)<uVec[9] & uVec[9] <sVec[2]) {
   sVec<- c(sVec[1], uVec[9],sVec[2],sVec[3], sVec[4], sVec[5], sVec[6],sVec[7],sVec[8])
} else if(sVec[2]<uVec[9] & uVec[9]<sVec[3]) {
  sVec<- c(sVec[1], sVec[2], uVec[9], sVec[3], sVec[4], sVec[5], sVec[6], sVec[7], sVec[8])
} else if(sVec[3]<uVec[9] & uVec[9]< sVec[4]) { 
     sVec<-c(sVec[1],sVec[2],sVec[3],uVec[9],sVec[4], sVec[5], sVec[6], sVec[7],sVec[8])
} else if(sVec[4]<uVec[9] & uVec[9]< sVec[5]) { 
     sVec<-c(sVec[1],sVec[2],sVec[3],sVec[4],uVec[9], sVec[5], sVec[6], sVec[7], sVec[8])
} else if(sVec[5]<uVec[9] & uVec[9]< sVec[6]) { 
     sVec<-c(sVec[1],sVec[2],sVec[3],sVec[4],sVec[5], uVec[9], sVec[6], sVec[7], sVec[8])
} else if(sVec[7]<uVec[9] & uVec[9]< sVec[7]) { 
     sVec<-c(sVec[1],sVec[2],sVec[3],sVec[4],sVec[5], sVec[6], uVec[9], sVec[7], sVec[8])
} else if(sVec[7]<uVec[9] & uVec[9]< sVec[8]) { 
     sVec<-c(sVec[1],sVec[2],sVec[3],sVec[4],sVec[5], sVec[6], sVec[7], uVec[9], sVec[8])
} else{
  sVec<- c(sVec, uVec[9])
}
if(uVec[10]< min(sVec)) {
   sVec<-c(uVec[10], sVec)
 }  else if(min(sVec)<uVec[10] & uVec[10] <sVec[2]) {
   sVec<- c(sVec[1], uVec[10],sVec[2],sVec[3], sVec[4], sVec[5], sVec[6],sVec[7],sVec[8], sVec[9])
} else if(sVec[2]<uVec[10] & uVec[10]<sVec[3]) {
  sVec<- c(sVec[1], sVec[2], uVec[10], sVec[3], sVec[4], sVec[5], sVec[6], sVec[7], sVec[8], sVec[9])
} else if(sVec[3]<uVec[10] & uVec[10]< sVec[4]) { 
     sVec<-c(sVec[1],sVec[2],sVec[3],uVec[10],sVec[4], sVec[5], sVec[6], sVec[7],sVec[8], sVec[9])
} else if(sVec[4]<uVec[10] & uVec[10]< sVec[5]) { 
     sVec<-c(sVec[1],sVec[2],sVec[3],sVec[4],uVec[10], sVec[5], sVec[6], sVec[7], sVec[8], sVec[9])
} else if(sVec[5]<uVec[10] & uVec[10]< sVec[6]) { 
     sVec<-c(sVec[1],sVec[2],sVec[3],sVec[4],sVec[5], uVec[10], sVec[6], sVec[7], sVec[8], sVec[9])
} else if(sVec[7]<uVec[10] & uVec[10]< sVec[7]) { 
     sVec<-c(sVec[1],sVec[2],sVec[3],sVec[4],sVec[5], sVec[6], uVec[10], sVec[7], sVec[8], sVec[9])
} else if(sVec[7]<uVec[10] & uVec[10]< sVec[8]) { 
     sVec<-c(sVec[1],sVec[2],sVec[3],sVec[4],sVec[5], sVec[6], sVec[7], uVec[10], sVec[8], sVec[9])
} else if(sVec[8]<uVec[10] & uVec[10]< sVec[9]) { 
     sVec<-c(sVec[1],sVec[2],sVec[3],sVec[4],sVec[5], sVec[6], sVec[7], sVec[8], uVec[10], sVec[9])
} else{
  sVec<- c(sVec, uVec[10])
}
sVec


Exercise 3:
Write a code chunk that takes in integer (k) as input and prints all values x which satisfy the above
equation.

k<-
n<-1
solnVec <-exp((2*pi*i)/k)

for (n in 2:k) {
    x<- exp((2*pi*i*n)/k)
    n<-n+1
    solnVec<- c(solnVec, x)
}
solnVec

       
