## Lab Assignment 0 
## Erin Mowers

## ***Please note: For variables that need to be defined by the user (usually denoted as "k" in the assignment), I have written them below in the form: "______<-input". 

## *Exercise 1a
k<-input
x0<-0
x1<-1
while(x1<=k){
    newX1<-x0+x1
    x0<-x1
    x1<-newX1
    }
    x0

#Exercise 1b
x<-input
y<-input
k<-input
a<-abs(k-x)
b<-abs(k-y)
if(a==b){
    cat(x)
    cat("AND")
    cat(y)
} else if (a<b){
x
} else{
y}

#Exercise 1c
k<-input
x0<-0
x1<-1
while(x1<=k){
    newX1<-x0+x1
    x0<-x1
    x1<-newX1
    }
a<-abs(k-x0)
b<-abs(k-x1)
if(a<b){
    x0
    }else{x1}


#Exercise 2a
firstVector<-input
secondVector<-input
a<-min(firstVector)
b<-max(firstVector)
c<-max(secondVector)
if(c<a){
    thirdVector<-c(c,a,b)
}else if (c<b){
    thirdVector<-c(a,c,b)
}else {
    thirdVector<-c(a,b,c)}
thirdVector

#Exercise 2b
firstVector<-input
a<-min(firstVector)
b<-max(firstVector)
newVector<-c(a,b)
newVector

#Exercise 2c
V<-input
for (ii in 1:8){
    iteration<-i
    for (i in 1:9){
        x1<-V[i]
        x2<-V[i+1]
        if (V[i]>V[i+1]){
        V[i]<-x2
        V[i+1]<-x1}
        }
    }
V


#Exercise 3
k<-input
solutionVector<-c(0)
myVar<-complex(real=0, imaginary=1)
for (n in 1:(k-1)){
    a<- exp(2*pi*myVar*n/k)
    solutionVector<-c(solutionVector, a)
    }
cat(solutionVector)

