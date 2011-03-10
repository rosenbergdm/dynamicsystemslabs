BonnieSheu
Lab1

QUESTION 1A

> k <- 100
> x0 <- 0
> x1 <- 1
> while(x1 <= k) {newX1 <- x0 + x1
+ x0 <- x1
+ x1 <- newX1}
> x0
[1] 89

----------------------------------------

QUESTION 1B

> k <- 1
> x <- 5
> y <- 8
> if (abs(k-x) > abs(k-y)) {
+ cat(y, "is closer to k.")} 
+ else if (abs(k-y) > abs(k-x)) {
+ cat(x, "is closer to k.")}
5 is closer to k.

----------------------------------------

QUESTION 1C

> k <- 50
> x0 <- 0
> x1 <- 1
> while(abs(k-x0) > abs(k-x1)) {
+ newX1 <- x0 + x1
+ x0 <- x1
+ x1 <- newX1}
> if (abs(k-x0) > abs(k-x1)) {
+ cat(x1, "is closer to k.")} 
+ else if (abs(k-x1) > abs(k-x0)) 
+ {cat(x0, "is closer to k.")}
55 is closer to k.

-----------------------------------------

QUESTION 2A

> vector1 <- c(10, 100)
> vector2 <- c(50)
> if (vector2 > vector1[2]) {
+ cat(vector3 <- c(vector1[1], 
+ vector1[2], vector2))} else if 
+ (vector2 < vector1[1]) {cat(
+ vector3 <- c(vector2, vector1[1],
+ vector1[2]))} else if ((vector2 > 
+ vector1[1]) & (vector2 < vector1[2]))
+ {cat(vector3 <- c(vector1[1], 
+ vector2, vector1[2]))}
10 50 100

-----------------------------------------

QUESTION 2B

> vector1 <- c(10, 1)
> vector2 <- c(min(vector1[1], vector1[2]),
+ max(vector1[1], vector1[2]))
> vector2
[1]  1 10

-----------------------------------------

QUESTION 2C (in progress...)

> vector1 <- c(203, 123, 56, 20, 1000, 89, 576, 94, 1, 0)
> if (vector1[1] < vector1[2]) {cat(vector2 <- c(vector1))} else if (vector1[1] > vector1[2]) {cat(vector2 <- c(vector1[2], vector1[1], vector1[3], vector1[4], vector1[5], vector1[6], vector1[7], vector1[8], vector1[9], vector1[10]))}
123 203 56 20 1000 89 576 94 1 0
> vector2
 [1]  123  203   56   20 1000   89  576   94    1    0
> if (min(vector2[1], vector2[2], vector2[3]) == vector2[1]) {cat(vector3 <- c(vector2))} else if (min(vector2[1], vector2[2], vector2[3]) == vector2[3]) {cat(vector3 <- c(vector2[3], vector2[1], vector2[2], vector2[4], vector2[5], vector2[6], vector2[7], vector2[8], vector2[9], vector2[10]))} else if ((vector2[3] > vector2[1]) & (vector2[3] < vector2[2])) {cat(vector3 <- c(vector2[1], vector2[3], vector2[2], vector2[4], vector2[5], vector2[6], vector2[7], vector2[8], vector2[9], vector2[10]))}
56 123 203 20 1000 89 576 94 1 0
> vector3
 [1]   56  123  203   20 1000   89  576   94    1    0
> if (min(vector3[1], vector3[2], vector3[3], vector3[4] == vector3[1])) {cat(vector4 <- c(vector3))} else if (min(vector3[1], vector3[2], vector3[3], vector3[4] == vector3[4])) {cat(vector4 <- c(vector3[4], vector3[1], vector3[2], vector3[3], vector3[5], vector3[6], vector3[7], vector3[8], vector3[9], vector3[10]))} else if ((vector3[4] > vector3[1]) & (vector3[4] < vector3[2])) {cat(vector4 <- c(vector3[1], vector3[4], vector3[2], vector3[3], vector3[5], vector3[6], vector3[7], vector3[8], vector3[9], vector3[10]))} else if ((vector3[4] > vector3[2]) & (vector3[4] < vector3[3])) {cat(vector4 <- c(vector3[1], vector3[2], vector3[4], vector3[3], vector3[5], vector3[6], vector3[7], vector3[8], vector3[9], vector3[10]))}
20 56 123 203 1000 89 576 94 1 0
> vector4
 [1]   20   56  123  203 1000   89  576   94    1    0

