##################################################
#melissa runfeldt
#assignment: Ch1 Lab Excercise
#problem 1
##################################################

inputExpression<-"2x^3+4x^5+6"
result<-parsePolynomial(inputExpression)
result[['exponents']]->ex1
result[['coefficients']]->co1

ex1-1->exponents
ex1*co1->coefficients

#outputexample#

deparsePolynomial(coefficients, exponents)
[1] "6x^2 + 0x^-1 + 20x^4"

##################################################
#melissa runfeldt
#assignment: Ch1 Lab Excercise
# problem 2.a
##################################################

x<-seq(from=0, to=10, length.out=500)
y<-x*(2-x/2)
plot (x,y)

##################################################
#melissa runfeldt
#assignment: Ch1 Lab Excercise
# problem 2.b.
##################################################

logisticImapB<-function(x){
	x1<-x*(2-x/2);
	return(x1);
	}
	
	init_value<-1;
	max_iter<-10
	x_array<-numeric(length=max_iter);
	x_array[1]<-init_value;
	
	for(ii in 2:max_iter){
		new_value<-logisticImapB(x_array[ii-1]);
		x_array[ii]<-new_value
		}
		
plot(1:max_iter,x_array,type='l',
main='f(x)=2x(1-x)',xlab='t',
ylab='x(t)');


##################################################
#melissa runfeldt
#assignment: Ch1 Lab Excercise
# problem 3.a.
##################################################

###Analytical determination of fixed points##
#f(x)=2x(1-x)

#x=2x(1-x)
#fixed point: x=0, 1/2

###Analytical determination of fixed point stability###

#f'(x)=2-4x
#f'(0)=2-4(0)=2	> 1	:Unstable

#f'(1/2)=2-4(1/2)=0	< 1	:Stable


###plot iterated map#

logisticImapB<-function(x){
	x1<-2*x*(1-x);
	return(x1);
	}
	
	init_value<-1;
	max_iter<-10
	x_array<-numeric(length=max_iter);
	x_array[1]<-init_value;
	
	for(ii in 2:max_iter){
		new_value<-logisticImapB(x_array[ii-1]);
		x_array[ii]<-new_value
		}
		
plot(1:max_iter,x_array,type='l',
main='f(x)=2x(1-x)',xlab='t',
ylab='x(t)');


###########################################
#melissa runfeldt
#assignment: Ch1 Lab Excercise
#problem 3.b.
################################################

###Analytical determination of fixed points##
#f(x)=4x(1-x)

#x=4x(1-x)
#fixed point: x=0, 2

###Analytical determination of fixed point stability##

#f'(x)=4-8x
#f'(0)=4-8(0)=4	> 1	:Unstable

#f'(1/2)=2-4(1/2)=0	< 1	:Stable


###plot iterated map#

logisticImapC<-function(x){
	x1<-4*x*(1-x);
	return(x1);
	}
	
	init_value<-1;
	max_iter<-8
	x_array<-numeric(length=max_iter);
	x_array[1]<-init_value;
	
	for(ii in 2:max_iter){
		new_value<-logisticImapC(x_array[ii-1]);
		x_array[ii]<-new_value
		}
		
plot(1:max_iter,x_array,type='l',
main='f(x)=4x(1-x)',xlab='t',
ylab='x(t)');

	