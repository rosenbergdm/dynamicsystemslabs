# name: Christina Baer
# assignment: 2
# date: 10/15/09
# filename: Bios Lab 2.R
#############################################################################

## Chapter 1 ##


#/--#########################################################################
# name: Christina Baer
# assignment: 2
# date: 10/15/09
# question: 1
# subquestion: 
# other files: 
##########################################################################/--

inputEx<-"x^3+2x^2-x-1"
parsePolynomial <- function(inputEx) {
 numberTerms <- nchar(gsub("[^x]", "", inputEx));
 coefficients <- numeric(length=(numberTerms)+1);
 exponents <- numeric((length=numberTerms)+1);
 splitTerms <- strsplit(gsub('-', '+ -', inputEx), '\\+')[[1]]
 for (ii in seq(along=splitTerms)) {
   term <- gsub(' ', '', splitTerms[ii]);
   if(gsub("[^x]", "", term) == '') {
     term <- paste(term, 'x^0', sep='');
   }
   if (gsub("([\\+|-]*)(.*)x.*", "\\2", term) == '') {
     term <- gsub('x', '1x', term);
   }
   if (gsub(".*x", "", term) == '') {
     term <- paste(term, '^1', sep='');
   }
   coef <- eval(as.integer(gsub('x.*', '', term)));
   coefficients[ii] <- coef;
   exponent <- eval(as.integer(gsub('.*\\^(.*)', '\\1', term)));
   exponents[ii] <- exponent;
 }
 o <- -(order(exponents)) + length(exponents) + 1;
 exponents <- exponents[o];
 coefficients <- coefficients[o];
 result<-list(exponents=exponents, coefficients=coefficients);
 return(result);
}

deparsePolynomial <- function(coefficients, exponents) {
 if (length(exponents) != length(coefficients)) {
   stop('Exponent vector and coefficient vector must be of equal length.\n')
 }
 out_string <- '';
 for (ii in seq(along=exponents)) {
   out_string <- paste(out_string, ' + ', as.character(coefficients[ii]),
                       'x^', as.character(exponents[ii]), sep='');
 }
 out_string <- gsub('\\+ -', '- ', out_string);
 out_string <- gsub('1x', 'x', out_string);
 out_string <- gsub(' x\\^0', ' 1', out_string);
 out_string <- gsub('x\\^0', '', out_string);
 out_string <- gsub('^ [\\+|-] ', '', out_string);
 out_string <- gsub('x\\^1', 'x', out_string);
 return(out_string);
}





der<-function(inputEx) {				

#parsePolynomial to find coefficients and exponents of original function

result<-parsePolynomial(inputEx) 			 																

#then manipulate the terms into the correct ones to generate the new terms

coefficients<-c(result[['coefficients']] * (result[['exponents']]))		
exponents<-c(result$exponents-1)			

#then make the new polynomial (the first derivative) with deparsePolynomial

out_string<-deparsePolynomial(coefficients, exponents)	
der1<-out_string
return(der1)
}

der1<-der(inputEx)
der1



#/--#########################################################################
# name: Christina Baer
# assignment: 2
# date: 10/15/09
# question: 2
# subquestion: a
# other files: BiosLab2-CSB-2a.pdf
##########################################################################/--

# logistic model equation

funa<- function(x) {
return(x*(2-(x/2)))
}

# logistic model with respect to time

modela<-function(x_at_t) {
x_at_t1<-funa(x_at_t);
return(x_at_t1)
}

# iterated map of the numerical solution

init_value <-1;
max_iter <-50
x_array<-numeric(length=max_iter);
x_array[1]<-init_value;
for (ii in 2:max_iter) {
new_value<-modela(x_array[ii-1]);
x_array[ii]<-new_value
}

plot2a<-plot(1:max_iter, x_array, 
main='Question 2a', xlab='t', type='l',
ylab='x(t)')

#PDF copy of plot
pdf(file="BiosLab2-CSB-2a.pdf")
plot2a<-plot(1:max_iter, x_array, 
main='Question 2a', xlab='t', type='l',
ylab='x(t)')
dev.off()


# find the fixed points from f(x) form "ax - bx^2" where a and b are integers
# multiply original expression by 2 to create integers
inputEx<-"4x-1x^2"
result<-parsePolynomial(inputEx)

# divide resulting coefficients by 2 to give true r and k

r<-result$coefficients[1]/2
k<- -result$coefficients[3]/2
fixed<-c(0)
fixed[2]<- (r-1)/k
fixed

#Solution: N_(t+1)=(2-(N_(t)/2))N_(t)
#Fixed points=0, (r-1)/k
#r=2, k=0.5, second fixed point=(2-1)/0.5=2
#Analytical fixed points match those indicated by the plot


#/--#########################################################################
# name: Christina Baer
# assignment: 2
# date: 10/15/09
# question: 2
# subquestion: b
# other files: BiosLab2-CSB-2b.pdf
##########################################################################/--

# modeling function takes a logistic difference equation as a variable, with 
# additional inputs of plot names and the initial value for the iterated map

model2<-function(funa, title='', plotname='', initial) {
modelb<-function(x_at_t) {
x_at_t1<-funa(x_at_t)
return(x_at_t1)
}

init_value <-initial
max_iter <-10
x_array<-numeric(length=max_iter);
x_array[1]<-init_value;
for (ii in 2:max_iter) {
new_value<-modelb(x_array[ii-1]);
x_array[ii]<-new_value
}
plotname<-plot(1:max_iter, x_array[1:max_iter], 
main=title, xlab='t', type='l',
ylab='x(t)')
return<-plotname
}

plot2b<-model2(funa, "Question 2b", plot2b, 1)

#PDF copy of plot
pdf(file="BiosLab2-CSB-2b.pdf")
plot2b<-model2(funa, "Question 2b", plot2b, 1)
dev.off()


# find the fixed points from f(x) form "ax - bx^2" where a and b are integers
# multiply original expression by an "integerfactor" to create integers

fp<-function(inputEx, integerfactor) {
result<-parsePolynomial(inputEx)

# divide resulting coefficients by "integerfactor" to give true r and k

r<-result$coefficients[1]/integerfactor
k<- -result$coefficients[3]/integerfactor
fixed<-c(0)
fixed[2]<- (r-1)/k
return(fixed)
}

fixed<-fp("4x-1x^2", 2)
fixed

# Fixed Points=0, (r-1)/k
# r=2, k=0.5, second fixed point=(2-1)/0.5=2
# Analytical fixed points match those indicated by the plot.


#/--#########################################################################
# name: Christina Baer
# assignment: 2
# date: 10/15/09
# question: 3
# subquestion: a
# other files: BiosLab2-CSB-3a.pdf
##########################################################################/--


funa<-function(x) {
return(2*x*(1-x))
}
plot3a<-model2(funa, "Question 3a", plot3a, 0.9)

#PDF copy of plot
pdf(file="BiosLab2-CSB-3a.pdf")
plot3a<-model2(funa, "Question 3a", plot3a, 0.9)
dev.off()

inputEx<-"2x-2x^2"
fixed<-fp(inputEx, 1)
fixed

# Fixed Points=0, (r-1)/k
# r=2, k=2, (r-1)/k=0.5
# Analytical fixed points match those indicated by the plot.

#/--#########################################################################
# name: Christina Baer
# assignment: 2
# date: 10/15/09
# question: 3
# subquestion: b
# other files: BiosLab2-CSB-3b.pdf
##########################################################################/--


funa<-function(x) {
return(4*x*(1-x))
}

plot3b<-model2(funa, "Question 3b", plot3b, 0.7499999)

#PDF copy of plot
pdf(file="BiosLab2-CSB-3b.pdf")
plot3b<-model2(funa, "Question 3b", plot3b, 0.7499999)
dev.off()


inputEx<-"4x-4x^2"
fixed<-fp(inputEx, 1)
fixed

# Fixed Points=0, (r-1)/k
# r=4, k=4, (r-1)/k=0.75
# Plotted fixed points are 0 and 0.75 and match those on the plot.
