\name{mDeriv}
\alias{mDeriv}
\title{Symbolically calculate derivates.}
\usage{mDeriv(expr, var=x, degree=1)}
\description{Symbolically calculate derivates.}
\details{Description here.}
\value{a function corresponding to the requested
derivative.}
\seealso{\code{\link{mInteg}}}
\seealso{\code{\link{mSolve}}}
\author{David Rosenberg \email{rosenbergdm}
\arguments{\item{expr}{the mathematical function to operate on
This argument can be given as a function, an
unevaluated expression, a character string or
raw text.}
\item{var='x'}{variable to perform differentiation with
respect to.}
\item{degree=1}{Order of derivative.}}
\examples{
## You can use raw expressions like this.
mDeriv(x^3 - 2 * x^2 + 1, 'x')

## 'Simple' functions are functions that
##   only involve calculator -type operations.
##   for a full list, see TODO_FUN_LIST

f <- function(x) {
    return(cos(2 * x) + sin(x)) 
}

## You can use the function directly or you can use
## just the body of the function.
mDeriv(f, 'x')
mDeriv(deparse(body(f)), 'x')

## The 'var' argument defaults to 'x'
mDeriv('x^2')

## The 'degree' argument defaults to 1
mDeriv("(y - 1)^2 * (y + 3)^3", 'y', 1);
mDeriv("(y - 1)^2 * (y + 3)^3", 'y', 2);

}
