\name{go-skip-qqq}
\alias{go-skip-qqq}
\alias{go}
\alias{skip}
\alias{qqq}
\title{Flow control for debugger}
\description{\code{go}, \code{skip} and \code{qqq} ONLY work inside the debugger, i.e. while you are paused at a D(\dots)> prompt during the execution of a function that has been \code{mtrace}d. \code{go} makes the debugger begn executing code continuously, without user intervention; \code{skip(n)} shifts the execution point; \code{qqq()} quits the debugger.}
\usage{
go(line.no) # line.no can be missing
skip(line.no)
qqq()
}
\arguments{\item{ line.no}{a line number, as shown in the code window (see also DETAILS)}}
\details{
\code{go()} without any argument puts the debugger into "go mode", whereby it starts executing function code without pausing for input (see \code{package?debug}). \code{go(n)} basically means "run continuously until you reach line n". It sets a temporary breakpoint at line \code{n}, which is triggered the first time line \code{n} is reached and then immediately cleared.

\code{skip(n)} moves the execution point (highlighted in the code window) to line \code{n}, without executing any intervening statements. You can \code{skip} forwards and backwards, and between the main function code and the exit code. You can skip in and out of loops and conditionals, except that you can't skip into a \code{for} loop (the execution point will move to the start of the loop instead). Note that skipping backwards does not undo any statements already executed. \code{skip} is useful for circumventing errors, and for ensuring that exit code gets run before calling \code{qqq()}.

\code{qqq()} quits the debugger, closing all code windows, and returns to the command prompt. No further code statements will be executed, which means no exit code either; take care with open files and connections.
}
\note{Previous versions of the debugger used \code{q()} to quit. With hindsight, this seems unwise, so I've changed it. However, calls to \code{q()} while debugging are still intercepted, and a warning is printed.}
\seealso{ \code{\link{debug}}, \code{\link{mtrace}}, \code{\link{bp}}}
\author{Mark Bravington}
\keyword{debugging}
