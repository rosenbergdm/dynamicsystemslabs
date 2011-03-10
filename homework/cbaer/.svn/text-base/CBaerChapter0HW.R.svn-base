#Christina
# Lab 0

#1(a)
k<-1555
x0<-0
x1<-1
while(x1<1000) {
newX1 <-x0+x1
x0<-x1
x1<-newX1
}
x0 

#1(b)
x<-7
y<-2
k<-9
if (abs(k-x)<abs(k-y)) {
print(x)
} else if (abs(k-y)<abs(k-x)) {
print(y)
}

#1(c)
k<-72
x0<-0
x1<-1
while(x1<k) {
newX1<-x0+x1
x0<-x1
x1<-newX1
}
x0
x1
if (abs(k-x0)<=abs(k-x1)) {
cat(x0)
} else if (abs(k-x1)<abs(k-x0)) {
cat(x1)
}

#2(a)
aVec<-c(2,27)
bVec<-c(79)
if(bVec<=aVec[1]) {
cVec<-c(bVec, aVec)
} else if (bVec>aVec[2]) {
cVec<-c(aVec, bVec)
} else {cVec<-c(aVec[1], bVec, aVec[2])
}
cVec

#2(b)
xVec<-c(5, 3)
if(xVec[1]<=xVec[2]) {
xVec<-c(xVec[1], xVec[2])
} else {xVec<-c(xVec[2], xVec[1])}
xVec

#2(c)
ten<-c(3, 1, 35, 67, 43, 2, 98, 500, 58, 75)
sorted<-c()
while (length(ten)>0) {
min(ten)
sorted<-c(sorted, min(ten))
nten<-ten>max(sorted)
ten<-c(ten[nten[TRUE]])
}
sorted

#3
k<-3
kpieces<-c(0:(k-1))
arg=2*pi*kpieces/k
carg<-complex(argument=arg)
carg^k