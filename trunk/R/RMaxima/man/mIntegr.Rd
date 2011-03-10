\name{mIntegr}
\alias{mIntegr}
\title{Symbolically calculate derivates.}
\usage{mIntegr(expr, var=x, degree=1)}
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
\examples{mDeriv(x^3 - 2 * x^2 + 1)
f <- function(x) {
return(cos(2 * x) + sin(x)) 
}
mDeriv(f)
mDeriv("(y - 1) * (y + 3)")}
