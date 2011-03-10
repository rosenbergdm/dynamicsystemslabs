## exercise.R
###################################################
### chunk number 1: aa_SweaveListingsPreparations
###################################################
source('listingPreps.R');


###################################################
### chunk number 2: dirfieldex1text eval=FALSE
###################################################
## ## Define f(x,t)
## f <- exampleFun1 <- function(x, t) {
##   return(1 / 90 * (90 - x) * x)
## }
## 
## ## Set plotting parameters
## tRange <- c(0, 10)
## dt <- 0.5
## xRange <- c(0, 100)
## dx <- 5
## h <- 0.7        ## delta t
## k <- 0.5        ## aspect correction
## 
## tSteps <- diff(range(tRange)) / dt
## xSteps <- diff(range(xRange)) / dx
## 
## ## Initialize the plot.  Note
## ##   don't copy these labels!'
## plot(c(0, 9), c(0, 90), type='n',
##   main=paste("{\\large\\bf $f(x,t)",
##   " = \\frac{x}{90} (90 - x)$ }"),
##   xlab='{\\larger $t$}', xaxs='i',
##   ylab='{\\larger $x$}', fg=gray(0.6),
##   yaxs='i')
## 
## 
## ## Main loop
## for (ii in 0:(tSteps-1)) {
##   for (jj in 0:(xSteps-1)) {
##     t <- ii * dt
##     x <- jj * dx
##     m <- f(x, t);
##     u <- t + (h * dt)
##     v <- x + (k * h * dt) * m
##     arrows(t, x, u, v, code=2,
##       length=0.05)
##   }
## }


###################################################
### chunk number 3: dirfieldex1field
###################################################
## Define f(x,t)
f <- exampleFun1 <- function(x, t) {
  return(1 / 90 * (90 - x) * x)
}

## Set plotting parameters
tRange <- c(0, 10)
dt <- 0.5
xRange <- c(0, 100)
dx <- 5
h <- 0.7        ## delta t
k <- 0.5        ## aspect correction

tSteps <- diff(range(tRange)) / dt
xSteps <- diff(range(xRange)) / dx

## Initialize the plot.  Note
##   don't copy these labels!'
plot(c(0, 9), c(0, 90), type='n',
  main=paste("{\\large\\bf $f(x,t)",
  " = \\frac{x}{90} (90 - x)$ }"),
  xlab='{\\larger $t$}', xaxs='i',
  ylab='{\\larger $x$}', fg=gray(0.6),
  yaxs='i')

## Main loop
for (ii in 0:(tSteps-1)) {
  for (jj in 0:(xSteps-1)) {
    t <- ii * dt
    x <- jj * dx
    m <- f(x, t);
    u <- t + (h * dt)
    v <- x + (k * h * dt) * m
    arrows(t, x, u, v, code=2,
      length=0.05)
  }
}


###################################################
### chunk number 4: dirfieldex2
###################################################
curve(exampleFun1, from=-10, to=100, col='blue', main="{\\textbf{Flow lines}}",
xlab='{\\larger $t$ }', fg=gray(0.6),
ylab=paste("{\\larger\\bf $f(x,t)", 
" = \\frac{x}{90} (90 - x)$ }"),bty='n');
arrows(c(2.5, -2.5, 100), c(1, -1, -1), c(87.5, -12.5, 92.5),c(1, -1, -1), col='red', type=2, lwd=2, length=0.05)
abline(h=0, col='black', lwd=1.5);
abline(v=0, col='black', lwd=1.5);



## guide.R
###################################################
### chunk number 1: aa_SweaveListingsPreparations
###################################################
#require(base)
require(SweaveListingUtils)
require(pgfSweave)
oldRset <- .myRset <- getSweaveListingOption("Rset")
oldRout <- .Rout <- getSweaveListingOption('Rout')
#options(warn=3)
#options(error=recover)
.myRset[['literate']]<-"{<-}{<-}2{<<aa_-}{<<aa_-}2"
.myRset$basicstyle <- "{\\footnotesize\\color{Rcommentcolor}}"
.myRset[['keywordstyle']] <- "{\\footnotesize\\bf\\color{red}}"
.myRset$numbers <- 'left'
.myRset$commentstyle <- "{\\color{black}\\ttfamily\\itshape}"
.myRset$numberstyle="\\tiny"
.Rout$fancyvrb <- 'true'
.Rout$keywordstyle <- "{\\color{Routcolor}}"
.Rout$breaklines <- 'true'
.Rout$linewidth <- "{0.5\\textwidth}"
.myRset$extendedchars <- 'true'
.myRset$breaklines <- 'true'
.myRset$linewidth="{0.5\\textwidth}"
.myRset$otherkeywords <- "{!,!=,~,$,*,\\&,\\%/\\%,\\%*\\%,\\%\\%,<-,<<aa_-,/, \\%in\\%}"
setToBeDefinedPkgs(pkgs = c("base"), keywordstyle="\\bf\\color{red}")
SweaveListingoptions(Rset=.myRset, Rout=.Rout, intermediate = FALSE)
#SweaveListingPreparations()
setCacheDir('cache2')
options(device=quartz);
par(mar=c(2,2,2,2))



## listingPreps.R
#require(base)
require(SweaveListingUtils)
require(pgfSweave)
oldRset <- .myRset <- getSweaveListingOption("Rset")
options(width=50);
oldRout <- .Rout <- getSweaveListingOption('Rout')
#options(warn=3)
#options(error=recover)
.myRset[['literate']]<-"{<-}{<-}2{<<aa_-}{<<aa_-}2"
.myRset$basicstyle <- "{\\tiny\\color{Rcommentcolor}}"
.myRset[['keywordstyle']] <- "{\\tiny\\bf\\color{red}}"
.myRset$numbers <- 'left'
.myRset$commentstyle <- "{\\color{black}\\ttfamily\\itshape}"
.myRset$numberstyle="\\tiny"
.myRset$escapeinside="{(*@}{@*)}"
.Rout$fancyvrb <- 'true'
.Rout$keywordstyle <- "{\\color{Routcolor}}"
.Rout$breaklines <- 'true'
.Rout$linewidth <- "{0.7\\textwidth}"
.myRset$extendedchars <- 'true'
.myRset$breaklines <- 'true'
.myRset$linewidth="{0.7\\textwidth}"
.myRset$otherkeywords <- "{!,!=,~,$,*,\\&,\\%/\\%,\\%*\\%,\\%\\%,<-,<<aa_-,/, \\%in\\%}"
setToBeDefinedPkgs(pkgs = c("base"), keywordstyle="\\bf\\color{red}")
SweaveListingoptions(Rset=.myRset, Rout=.Rout, intermediate = FALSE)
#SweaveListingPreparations()
setCacheDir('cache2')
#options(device=quartz);
source('maxima_utilities.R')
par(mar=c(2,2,2,2))

## maxima_utilities.R
#!/usr/bin/env r
# encoding: utf-8
#
# maxima_utils.R
# Copyright (c) 2009 David M. Rosenberg and the University of Chicago.
# All rights reserved.
#
# $Author: root $
# $LastChangedDate: 2009-10-17 16:11:40 -0500 (Sat, 17 Oct 2009) $
# $LastChangedRevision: 128 $
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/
#
# Maintainer: David M. Rosenberg <rosenbergdm@uchicago.edu>
# Created on 2009-10-13
##########################################################################

# maxima_bin="ssh biostat@apalmerlab1.palmerlab.org"
maxima_binary <- maxima_bin <- "/usr/local/bin/rmaxima"

normExpArg <- function (argname, callMatch) {
  mc <- callMatch
  if (class(trySilent(
          expString <- as.character(as.expression(mc[[argname]])))) != 
                                  "try-error" && 
          length(grep(mc[[argname]], pattern = "[\\+|-|\\*|\\/|\\^]")) 
                                          > 0
     ) { }
  else if (class(trySilent(g <- match.fun(mc[[argname]]))) != 
    "try-error") {
    tmpString <- deparse(body(g))
    expString <- tmpString[grep(tmpString, pattern = "\\{|\\}", 
        invert = TRUE)]
    expString <- gsub("^return\\((.*)\\)(;)?$", "\\1", gsub(" ", 
        "", paste(expString, collapse = "")))
  }
  else if (is.character(mc[[argname]])) {
    expString <- mc[[argname]]
  }
  else {
    words <- .RMaximaEnv$translation_table
    for (ii in seq(along=words$maxima)) {
      expString <- gsub(words$maxima, words$R[ii], expString);
    }
  } 
  return(expString)
}

#' Symbolically calculate derivates.
#'
#' Description here.
#'
#' @param expr the mathematical function to operate on
#'   This argument can be given as a function, an
#'   unevaluated expression, a character string or
#'   raw text.
#' @param var='x' variable to perform differentiation with
#'   respect to.
#' @param degree=1 Order of derivative.
#' @return a function corresponding to the requested
#'   derivative.
#' @export
#' @seealso \code{\link{mInteg}}
#' @seealso \code{\link{mSolve}}
#' @author David Rosenberg \email{rosenbergdm@uchicago.edu}
#' @examples
#' mDeriv(x^3 - 2 * x^2 + 1)
#' f <- function(x) {
#'     return(cos(2 * x) + sin(x)) 
#' }
#' mDeriv(f)
#' mDeriv("(y - 1) * (y + 3)")
mDeriv <- function(expr, var='x', degree=1) {
  mc <- as.list(match.call(expand.dots=TRUE));
  p1 <- normExpArg('expr', mc);

  maxima_cmd <- sprintf('print(diff(%s,%s,%d));', p1, var, degree)
  res2 <- res <- .performMaximaCall(maxima_cmd);
  f <- function() {};
  reserved <- .RMaximaEnv$translation_table;
  for (ii in 1:length(reserved$R)) {
    res2 <- gsub(reserved$maxima[ii], reserved$R[ii], res2);
    res <- gsub(reserved$maxima[ii], '', res)
  }
  chars <- strsplit(res, "")[[1]];
  argnames <- unique(chars[ chars %in% letters ])
  
  args <- vector(mode='list', length=length(argnames));
  names(args) <- argnames;
  formals(f) <- args;
  body(f) <- parse(text=res2);
  return(f);
}


#' Symbolically calculate derivates.
#'
#' Description here.
#'
#' @param expr the mathematical function to operate on
#'   This argument can be given as a function, an
#'   unevaluated expression, a character string or
#'   raw text.
#' @param var='x' variable to perform differentiation with
#'   respect to.
#' @param degree=1 Order of derivative.
#' @return a function corresponding to the requested
#'   derivative.
#' @export
#' @seealso \code{\link{mInteg}}
#' @seealso \code{\link{mSolve}}
#' @author David Rosenberg \email{rosenbergdm@uchicago.edu}
#' @examples
#' mDeriv(x^3 - 2 * x^2 + 1)
#' f <- function(x) {
#'     return(cos(2 * x) + sin(x)) 
#' }
#' mDeriv(f)
#' mDeriv("(y - 1) * (y + 3)")
mInt <- function(expr, var='x', degree=1) {
  mc <- as.list(match.call(expand.dots=TRUE));
  p1 <- normExpArg('expr', mc);

  maxima_cmd <- sprintf('print(integrate(%s,%s,%d));', p1, var, degree)
  res2 <- res <- .performMaximaCall(maxima_cmd);
  f <- function() {};
  reserved <- .RMaximaEnv$translation_table;
  for (ii in 1:length(reserved$R)) {
    res2 <- gsub(reserved$maxima[ii], reserved$R[ii], res2);
    res <- gsub(reserved$maxima[ii], '', res)
  }
  chars <- strsplit(res, "")[[1]];
  argnames <- unique(chars[ chars %in% letters ])
  
  args <- vector(mode='list', length=length(argnames));
  names(args) <- argnames;
  formals(f) <- args;
  body(f) <- parse(text=res2);
  return(f);
}

mSolve <- function(expr, var='x') {
  p1 <- as.character(expr);
  cmd <- paste('display2d:false; stardisp:true; print(solve(', p1, ',', var,
               '));');
#  cmd2 <- paste("echo 'echo \"", cmd, '" | ', "/usr/bin/maxima'", ' | ',
#                maxima_bin);
  cmd2 <- sprintf("echo '%s' | /usr/local/bin/rmaxima", cmd);
  res <- system(cmd2, intern=TRUE)[9];
  res <- strsplit(gsub('\\]', '', gsub(var, '', gsub('[=|\\[]', '', res))), 
                  ', ')[[1]]
  solutions <- c();
  for (ii in res) {
    solutions <- c(solutions, eval(parse(text=ii)));
  }
  return(solutions);
}


.performMaximaCall <- function(maxima_command, ...) {
  opt_string <- paste(.RMaximaEnv$maxima_opts, collapse=" ");
  maxima_cmd <- paste(opt_string, maxima_command, sep=" ");
  
  if (.RMaximaEnv$is_local) {
    maxima_bin <- .RMaximaEnv$localMaxima;
    sys_cmd <- sprintf("echo '%s' | %s", maxima_cmd, maxima_bin);
    res <- system(sys_cmd, intern=TRUE);
  } else {
    con <- .RMaximaEnv$RServeConnection;
    maxima_bin <- .RMaximaEnv$maxima_bin;
    sys_cmd <- shQuote(sprintf("echo '%s' | %s", maxima_command, maxima_bin));
    cmd_name <- paste(sys_cmd, .RMaximaEnv$mID, sep="_");
    RSassign(con, obj=sys_cmd, name=cmd_name);
    res <- RSeval(quote(eval(sprintf("system(%s, intern=TRUE)", cmd_name))));
  }
  res <- res[1:length(res) > 5];
  res.out <- paste( res[grep("^(\\(%)", res, invert=TRUE,
                   perl=TRUE)], collapse=' ');
  return(res.out);
}



##################################################
## More robust version here                     ##
##################################################

.createRserveEnv <- function() {
  ## sanity check
  require(Rserve)
  require(digest)
  
  rserve_hostname <- 'rosenbergdm.uchicago.edu';
  rserve_port <- 6311;
  rserve_user <- 'biostat';
  rserve_auth <- 'pErdos2';

  .RMaximaEnv <<- new.env(parent=.GlobalEnv, hash=TRUE);
  RServeConnection <- RSconnect(host=rserve_hostname, port=rserve_port);
  RSlogin(RServeConnection, user=rserve_user, pwd=rserve_auth);
    
  maxima_bin <- '/usr/local/bin/maxima'
  localMaxima <- Sys.which(c('maxima', 'maxima'))
  if (all(localMaxima == "")) {
    is_local <- FALSE
  } else {
    is_local <- TRUE;
    localMaxima <- localMaxima[1];
  }
  mid <- substring(digest(c(Sys.time(), utils::sessionInfo())), 1, 16);
  maxima_opts <- c('display2d:false;', 'stardisp:true;')
  
  translation_table <- data.frame(maxima=c('cos', 'sin', 'tan', 'exp', 'atan2', 'atan', 'log', 'del', 'sqrt', 'rat', '%e', 'log', 'sec', 'csc'), R=c('cos', 'sin', 'tan', 'exp', 'atan2', 'atan', 'log', 'del', 'sqrt', 'rat', 'exp(1)', 'log', 'sec', 'csc'))
  
  for (obj in c('maxima_bin', 'localMaxima', 'is_local', 'mid',
                'RServeConnection', 'rserve_auth', 'rserve_user',
                'rserve_port', 'rserve_hostname', 'maxima_opts', 
                'translation_table') ) {
    assign(obj, envir=.RMaximaEnv, value=get(obj));
  }
}

.destroyRserveEnv <- function() {
  ##
  ## TODO: Package teardown.
  ##
}


mSolve.RServe <- function(equ, var='x', ..., symbolic=FALSE) {
  ##  sanity check
  if ( (length(equ) > 1) | symbolic ) {
    warning('Not yet implemented');
    return();
  } else if (length(equ) < 1) {
    warning('You must specify the equation(s) to solve.');
  }
  
  ##  values to (eventually) substitute into the result.
  var_values <- list(...);
  
  ##  prep command string
  maxima_commands <- c( 
                        'display2d:false;',
                        'stardisp:true;',
                         paste('print(solve(', equ, ', ', var, '));', sep='')
                      );
                      
  shell_wrap_commands <- paste(   
                                 "echo \"", 
                                 paste(maxima_commands, collapse=' '),
                                 '" | ',
                                 maxima_binary,
                                 sep=' '
                              );
  
  ## Recall hidden persistent connection
  con <- get('RServeConnection', envir=.RMaximaEnv);
  #server_id <- get('server_id', envir=.RMaximaEnv)
  
  RSassign(con, shell_wrap_commands, 'shell_wrap_commands');
  raw_output <- RSeval( con, 
                        quote(system(shell_wrap_commands, intern=TRUE) ) 
                      );
  
  ## High-level deparse of maxima's  output
  
  output_no_message <- raw_output[1:length(raw_output) > 5];
  
  print_lines <- paste( 
              output_no_message[
            grep("^[(\\(%)|(rat)]", output_no_message,
          invert=TRUE, perl=TRUE)],
              collapse=' '
                      );

  deparsed_result <- strsplit(
                            gsub('\\]', '', 
                                gsub(var, '', 
                                  gsub('[=|\\[]', '', print_lines
                                      )
                                    )
                                ),', '
                             )[[1]];
  
  solutions <- numeric(length=length(deparsed_result));
  
  ##
  ## TODO: This is where there ... substitutions need to be made.
  ##
  
  for (ii in seq(along=deparsed_result)) {
    solutions[ii] <- eval( parse(text=deparsed_result[ii]),
                           var_values );
    
  }
  
  return(solutions);
}


mSolve2 <- function(expr, var='x', with=list()) {
  mc <- as.list(match.call(expand.dots=TRUE))
  terms <- as.character(mc$expr)[2:3];
  for (jj in seq(along=terms)) {
    tt <- terms[jj];
    subterms <- strsplit(tt, ' ')[[1]];
    for (ii in seq(along=subterms)) {
      ss <- subterms[ii]
      if(length(grep('^\\w+\\(.*\\)', ss) != 0)) {
        funcname=gsub('\\(.*\\)', '', ss);
        if(length(ls(pattern=paste('^', funcname, '$', sep=''), envir=.GlobalEnv)) != 0) {
          tmpString <- deparse(body(funcname))
          expString <- tmpString[grep(tmpString, pattern = "\\{|\\}", 
              invert = TRUE)]
          expString <- gsub("^return\\((.*)\\)(;)?$", "\\1", gsub(" ", 
              "", paste(expString, collapse = "")))
          varname <- gsub('.*\\((.*)\\)', '\\1', ss);
          subterms[ii] <- gsub(names(as.list(args(funcname)))[1], varname, expString)
        }
      }
    }
    terms[jj] <- paste(subterms, collapse=" ");
  }
  equation <- paste(terms[1], '=', terms[2], sep='');
  maxima_cmd <- sprintf('print(solve(%s, %s));', equation, var);
  
  res <- .performMaximaCall(maxima_cmd);
  deparsed_result <- strsplit(
                            gsub('\\]', '', 
                                gsub(var, '', 
                                  gsub('[=|\\[]', '', res
                                      )
                                    )
                                ),', '
                             )[[1]];
  
  solutions <- list()

  for (jj in seq(along=deparsed_result)) {
    rres2 <- rres <- deparsed_result[jj];
    f <- function() {};
    reserved <- .RMaximaEnv$translation_table;
    for (ii in 1:length(reserved$R)) {
      rres2 <- gsub(reserved$maxima[ii], reserved$R[ii], rres2);
      rres <- gsub(reserved$maxima[ii], '', rres)
    }
    chars <- strsplit(rres, "")[[1]];
    argnames <- unique(chars[ chars %in% letters ])

    args <- vector(mode='list', length=length(argnames));
    names(args) <- argnames;
    formals(f) <- args;
    body(f) <- parse(text=rres2);
    solutions[[jj]] <- f;
    if (length(with) != 0) {
      solutions[[jj]] <- eval(body(solutions[[jj]]), pairlist(with)[[1]])
    }
  }
  
  
  return(unlist(solutions));
}
.createRserveEnv();


mSolve <- mSolve.RServe;

## solution.R
###################################################
### chunk number 1: aa_SweaveListingsPreparations
###################################################
source('listingPreps.R');



