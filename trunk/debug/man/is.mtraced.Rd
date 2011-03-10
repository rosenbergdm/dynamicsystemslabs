\name{is.mtraced}
\alias{is.mtraced}
\title{Check if a function has been mtraced.}
\description{Check if a function has been mtraced.}
\usage{
is.mtraced(f)
}
\arguments{\item{ f}{a function}}
\value{TRUE or FALSE.}
\examples{
fff <- function() 99
is.mtraced( fff)
mtrace( fff)
is.mtraced( fff)
}
\keyword{debugging}
