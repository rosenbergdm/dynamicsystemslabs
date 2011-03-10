cat("Megan Liszewski
Mathematical Models for Biological Sciences I
Homework 1
10/9/09

")


k1<-1000
k2<-8
x<-10
y<-45
k3<-32
vector1<-c(7,8)
vector2<-c(5)

cat("#1a
")
x0<-0
x1<-1
while(x1<=k1){
	x2<-x0+x1
	x0<-x1
	x1<-x2}
cat("The largest fibonacci number less than or equal to",k1,"is",x0,".
")

cat("
#1b
")
dif<-c(abs(k2-x),abs(k2-y))
if(dif[1]>dif[2]) {
	cat(y,"is closer to",k2,"than",x,"is.
")
}else if(dif[2]>dif[1]){
	cat(x,"is closer to",k2,"than",y,"is.
")
}else if(dif[2]==dif[1]){
	cat(x,"and",y,"are equidistant from",k2,".
")}
	
cat("
#1c
")
y0<-0
y1<-1
while(y1<k3){
	y2<-y0+y1
	y0<-y1
	y1<-y2}
dif2<-c(abs(k3-y1),abs(k3-y0))
if(dif[1]>dif[2]){
	cat(y0,"is the fibonacci number that is closest to",k3,".")
}else if(dif[1]<dif[2]){
	cat(y1,"is the fibonacci number that is closest to",k3,".")
}else if(dif[1]==dif[2]){
	cat(y1,"and",y0,"are the fibonacci numbers that closest to",k3,".
")}

cat("

#2a
")

vector3<-c()
vector4<-c(vector1,vector2)
for(i in 1:3){
if(vector4[i]==min(vector4)){
	vector3[1]<-vector4[i]}
	else if(vector4[i]==max(vector4)){
		vector3[3]<-vector4[i]}
		else {
			vector3[2]<-vector4[i]}}
cat("Two vectors",vector1,"and",vector2,"sort to",vector3)


cat("

#2b
")
vector4<-c(6,4)
vector5<-c(min(vector4),max(vector4))
cat(vector4,"is sorted to",vector5)

cat("

#2c
")

unsortvec<-rnorm(10,mean=10)
cat("Unsorted vector",unsortvec,"
")
sortvec<-c(min(unsortvec))

for(ii in 1:length(unsortvec)){
minval<-which(unsortvec==min(unsortvec))

if((minval[1]!=1)&(minval[length(minval)]!=length(unsortvec))){
sortvec[ii]<-unsortvec[minval[1]]
unsortvec2<-c(unsortvec[1:(minval[1]-1)],unsortvec[(minval[1]+1):length(unsortvec)])
unsortvec<-unsortvec2}

else if(minval[1]==1){
sortvec[ii]<-unsortvec[minval[1]]
unsortvec2<-c(unsortvec[2:length(unsortvec)])
unsortvec<-unsortvec2}

else if(minval[length(minval)]==length(unsortvec)){
sortvec[ii]<-unsortvec[minval[1]]
unsortvec2<-c(unsortvec[1:(length(unsortvec)-1)])
unsortvec<-unsortvec2}}

cat("sorts to",sortvec,".
")

cat("
#3
")
k<-6
x<-c()
for(j in 1:k){
	x[j]<-exp(2*pi*(0+1i)*j/k)}
cat("For k=",k,"the solutions to the equation x^k-1 = 0 are",x)