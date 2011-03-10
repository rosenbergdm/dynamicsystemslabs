# Name: Si Tang
# Email: sugar@uchicago.edu
# UCID # : 396904


##################
# Exercise 1(a)
##################

cat("\n ~~Exercise 1(a)~~ \n")
cat("The input is k \n")
x0 <- 0
x1 <- 1
# k<- 10

while(x1<=k)
{
	x2<-x0+x1
	x0<-x1
	x1<-x2
}

cat("The nearest number to k in fibonacci sequence is: ")
cat(x0, "\n")

##################
# Exercise 1(b)
##################
cat("\n ~~Exercise 1(b)~~ \n")
cat("The input is x, y, k \n")
#x <- 1
#y <- 1000
#k <- 100
if( abs(x-k) < abs(y-k) )
{
	cat("Number nearest to k is:", x, "\n")
}else {
	cat("Number nearest to k is:", y, "\n")
}


####################
#Exercise 1(c)
####################
cat("\n ~~Exercise 1(c)~~ \n")
cat("Input is k \n")
x0 <- 0
x1 <- 1
#k <- 100
while(x1<k)
{
	x2 <- x0+x1
	x0 <- x1
	x1 <- x2
}

if( abs(x1 - k) < abs(x0 - k))
{
	cat("The Fibonacci number nearest to k is: ", x1, "\n")
} else {
	cat("The Fibonacci number nearest to k is: ", x0, "\n")
}


#####################
#Exercise 2(a)
#####################
# a <- c(1:2)
# b <- c(3)

cat("\n ~~Exercise 2(a)~~ \n")
cat("Two input vectors are a and b and output vector is c \n")

if(b>max(a))
{
  c <- c(a, b)
} else if (b<min(a)) {

	c <- c(b, a)
} else { c <- c (min(a), b, max(a) ) 
}

cat (c, "\n")


######################
#Exercise 2(b)
######################
#a <- c ( 10, 5)
cat("\n ~~Exercise 2(b)~~ \n")
cat("Vector to be sorted is a \n")

if( a[1] > a[2] )
{
	a <-c (a[2], a[1])
}

cat (a, "\n")


########################
#Exercise 2(c)
########################
cat("\n ~~Exercise 2(c)~~ \n")
cat("Vector to be sorted is d \n")
#d <- c(1,3,4,5,6,7,8,9,0,2)


for(l in c(1:10) )
{
  for(j in c(l:10) )
  {
	if ( d[l] > d[j])
	{
		tmp <- d[l]
		d[l] <- d[j]
		d[j] <- tmp
	}
  }
}

cat (d, "\n")


########################
#Exercise 3
########################
cat("\n ~~Exercise 3~~ \n")
cat("The equation is x^k -1 =0 ")
myComp<- c(1:k)

for (i in c(1:k) )
{
	myComp[i]<-complex(argument=(pi*2*i)/k, modulus=1)
	cat("root(", i, ")=", myComp[i], "\n")
}
