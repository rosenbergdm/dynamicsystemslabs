\name{mtrace}
\alias{mtrace}
\alias{mtrace.off}
\alias{check.for.tracees}
\alias{tracees}
\title{Interactive debugging}
\description{\code{mtrace} sets or clears debugging mode for a function; \code{mtrace.off} clears debugging mode for all functions; \code{check.for.tracees} shows which functions are in debugging mode.}
\usage{
mtrace( fname, tracing=TRUE, char.fname, from=mvb.sys.parent(), update.tracees=TRUE, return.envs=FALSE)
# Usual: mtrace( fname) or mtrace( fname, tracing=FALSE, return.envs=FALSE)
mtrace.off()
check.for.tracees( where=1)
}
\arguments{
\item{ fname}{quoted or unquoted function name}
\item{ tracing}{TRUE to turn tracing on, FALSE to turn it off}
\item{ char.fname}{(rarely used) if your function name is stored in a character object \code{x}, use \code{char.fname=x}}
\item{ from}{where to start looking for \code{fname} (not usually needed)}
\item{ where}{(character or integer) position in search path}
\item{ update.tracees}{don't set this parameter! It's only for use by other functions}
\item{ return.envs}{if TRUE, this will return a list of the environments where the function has been replaced by the \code{mtraced} version}
}
\details{
\code{mtrace(myfun)} modifies the body code of \code{myfun}, and also stores debugging information about \code{myfun} in \code{tracees$myfun}. Next time the function is invoked, the modified debug-ready version will be called instead of the orginal. \code{mtrace} does not modify source code (or other) attributes, so \code{myfun} will "look" exactly the same afterwards. \code{mtrace(myfun,FALSE)} restores \code{myfun} to normal. \code{mtrace.off} unmtraces all mtraced functions (see below for exceptions).

Because \code{mtrace} modifies function bodies (possibly in several places, if namespaced packages are involved), calling \code{save.image} or \code{save} while functions are still \code{mtrace}d is probably not a good idea-- if the saved image is reloaded in a new \R session, the debugger won't know how to handle the previously \code{mtrace}d functions, and an error message will be given if they are invoked. The \code{Save} and \code{Save.pos} functions in package \pkg{mvbutils} will get round this without your having to manually untrace and retrace functions.

If you do see a "maybe saved before being un-mtraced?" error message when \code{myfun} is invoked, all is not lost; you can restore \code{myfun} to health via \code{mtrace(myfun,F)}, or put it properly into debugging mode via \code{mtrace(myfun)}. \code{mtrace.off} won't work in such cases, because \code{myfun} isn't included in \code{tracees}.

\code{mtrace} looks for a function in the following places: first in the frame stack (unless \code{from} has been set-- see below), then in the search path, then in all namespaces, then in all S3 methods tables. If several copies of a function are found, all will get modified (mtraced) to the \emph{same} code; ditto when unmtracing.

\code{mtrace} etc \emph{doesn't} (currently) handle S4 methods. However, I doubt there's any fundamental problem there (hopefully I will never have to find out)-- the first issue, though, is that S4 methods have mangled names. If you can work out what the mangled name is, then you can mtrace the "real" version and all may work.

\code{check.for.tracees} checks for functions which have been \code{mtrace}d, but only in one directory. By contrast, \code{names(tracees)} will return all functions that are currently known to be \code{mtrace}d. However, unlike \code{check.for.tracees}, \code{names(tracees)} won't show functions that were saved during a previous \R session in an \code{mtrace}d state.

\code{mtrace.off} will untrace all functions returned by \code{names( tracees)} and/or \code{check.for.tracees( 1)}.

\code{mtrace} puts a breakpoint (see \code{\link{bp}}) at line 1, but clears all other breakpoints.

\code{mtrace} can handle \code{mlocal} functions, but not (yet) \code{do.in.envir} functions-- the latter appear as monolithic statements in the code window. See package \pkg{mvbutils} for more details.

If you use \code{fixr} to edit functions, \code{mtrace} will automatically be re-applied when an updated function file is sourced back in. Otherwise, you'll need to call \code{mtrace} manually after updating a function.

The \code{from} argument is only used in the following situations. The first is to mtrace an S4 method that lives somewhere. Suppose there is a function \code{f} which first defines functions \code{g} and \code{h}, then calls \code{g}. Now suppose you have \code{mtrace}d \code{g} and that \code{g} is running, with the code window displayed. If you want to \code{mtrace(h)}, the problem is that \code{h} is not visible from the frame of \code{g}. To tell \code{mtrace} where to find \code{g}, call \code{mtrace( h, from=sys.parent())}. [You can also replace \code{sys.parent()} with the absolute frame number of \code{f}, if \code{f} has been \code{mtrace}d and its code window is visible.] \code{mtrace} will then look through the enclosing environments of \code{from} until it finds a definition of \code{h}.

If \code{myfun} has been defined in a namespaced package, then there may be several copies of \code{myfun} in the system, different ones being used at different times. \code{mtrace} will change them all; see \code{\link{fun.locator}} if you really want to know more.

If \code{mtrace(bar)} is called while function \code{foo} is being debugged (\code{mtrace(foo)} having previously been called), and \code{bar} has been redefined within \code{foo} or a parent environment of \code{foo}, then only the redefined copy of \code{bar} will be \code{mtrace}d.
}
\value{Not normally used. \code{mtrace} by default returns an invisible copy of the modified function body. If you set \code{return.envs=TRUE}, it will instead return a list of the environments in which the function has been modified. This is only intended for "internal use".}
\examples{
\dontrun{
mtrace(glm) # turns tracing on
names( tracees) # "glm"
check.for.tracees( "package:base") # "glm"
glm(stupid.args) # voila le debugger
qqq() # back to command prompt
mtrace( glm, FALSE)
mtrace.off() # turns it off for all functions
}
}
\keyword{debugging}
