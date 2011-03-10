# Patrick Mann
# Chapter 0 Homework
# pmann.chapter0.R

# 1a

k <- 7
x0 <- 0
x1 <- 1
while (x1 <= k) {
	newX1 <- x0 + x1
	x0 <- x1
	x1 <- newX1
	}
	x0

# 1b

k<-6
x<-1
y<-15
if (abs(k-x) > abs(k-y)) {y
	 } else if (abs(k-x) < abs (k-y)) {x
	 	} else {c(x,y)}

# 1c

k <- 7
x0 <- 0
x1 <- 1
while (x1 <= k) {
	newX1 <- x0 + x1
	x0 <- x1
	x1 <- newX1
	}
	if (abs(k-x0) > abs(k-x1)) {x1
	 } else if (abs(k-x0) < abs (k-x1)) {x0
	 	} else {c(x0,x1)}

# 2a

VecX <- c(2,4)
VecY <- c(5)
VecZ <- c(VecX,VecY)
VecFin <- c(min(VecZ),median(VecZ),max(VecZ))
VecFin

# 2b

VecX <- c(4,2)
VecFin <- c(min(VecX),max(VecX))
VecFin

# 2c

x <- c(4,1,3,5,2,6,8,7,9,10)
VecFin <- min(x)
y <- min(x)
x <- x[x>y]
VecFin <- c(VecFin,min(x))
y <- min(x)
x <- x[x>y]
VecFin <- c(VecFin,min(x))
y <- min(x)
x <- x[x>y]
VecFin <- c(VecFin,min(x))
y <- min(x)
x <- x[x>y]
VecFin <- c(VecFin,min(x))
y <- min(x)
x <- x[x>y]
VecFin <- c(VecFin,min(x))
y <- min(x)
x <- x[x>y]
VecFin <- c(VecFin,min(x))
y <- min(x)
x <- x[x>y]
VecFin <- c(VecFin,min(x))
y <- min(x)
x <- x[x>y]
VecFin <- c(VecFin,min(x))
y <- min(x)
x <- x[x>y]
VecFin <- c(VecFin,min(x))
y <- min(x)
x <- x[x>y]
x <-VecFin
x


# 3

k <- 4
kii <- 1:k
x <- complex(modulus=1, argument=2*pi*kii/k)
x

