####################################
#Melissa Runfeldt
#HWassignment#1 “Intro”. 9.30.2009
#Lab sec: Thurs 5-6:30
#Problem#:1.a
####################################


fibLessThan<-function(k){
	x0<-0
	x1<-1
	while(x1<=k){
	x3<-x0+x1
	x0<-x1
	x1<-x3
	}
	return(x0);
	}


####################################
#Melissa Runfeldt
#HWassignment#1 “Intro”. 9.30.2009
#Lab sec: Thurs 5-6:30
#Problem#:1.b
####################################

xorykay<-function(k,x,y){
	v1<-abs(k-x)
	v2<-abs(k-y)
	if(v1<v2){
	return(x)}
	else{return(y);}
	}

####################################
#Melissa Runfeldt
#HWassignment#1 “Intro”. 9.30.2009
#Lab sec: Thurs 5-6:30
#Problem#:1.c
####################################

closeFib<-function(k){
	x0<-0
	x1<-1
	while(x1<=k){
	x3<-x0+x1
	x0<-x1
	x1<-x3}
	v1<-abs(k-x0)
	v2<-abs(k-x1)
	if(v1<v2){
	return(x0);}
	else{return(x1);}
	}

####################################
#Melissa Runfeldt
#HWassignment#1 “Intro”. 9.30.2009
#Lab sec: Thurs 5-6:30
#Problem#:2.a
####################################

x<-44
y<-1
z<-99
sortedVec <- c(x,y)
otherVal <- z
sm <- min(sortedVec[1], otherVal)
md <- min(max(sortedVec[1], otherVal), sortedVec[2])
lg <- max(max(sortedVec[1], otherVal), sortedVec[2])
c(sm, md, lg)

####################################
#Melissa Runfeldt
#HWassignment#1 “Intro”. 9.30.2009
#Lab sec: Thurs 5-6:30
#Problem#:2.b
####################################

x<-44
y<-1
unsortedvec<-c(x,y)
base::sort(unsortedvec)

####################################
#Melissa Runfeldt
#HWassignment#1 “Intro”. 9.30.2009
#Lab sec: Thurs 5-6:30
#Problem#:2.c
####################################

unsortedvec<-c(55,1,8,9,22,508,44,66,3,23)
base::sort(unsortedvec)


####################################
#Melissa Runfeldt
#HWassignment#1 “Intro”. 9.30.2009
#Lab sec: Thurs 5-6:30
#Problem#:3
####################################

findxtok <- function (k) {
	newVec <- matrix(0,1,k)
	for (i in 1:k) {
	if (i ==1) {
	theta <- 0
	}
	else {
	theta <- (i-1)*2*pi/k
	}
	newVec[i] <- exp(1i*theta)
	}
	return(newVec)
	}

