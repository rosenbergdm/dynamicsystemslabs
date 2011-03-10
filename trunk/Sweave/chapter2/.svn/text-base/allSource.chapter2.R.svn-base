## exercise.R
###################################################
### chunk number 1: aa_SweaveListingsPreparations
###################################################
source('listingPreps.R');


###################################################
### chunk number 2: cobwebplot_func_1 eval=FALSE
###################################################
## cobImapFun <- function(x) {
##   return(2 * x - 0.5 * x ** 2);
## }
## max_iter <- 50;
## y_vector <- x_vector <- numeric(
##   max_iter * 2);
## y_vector[1] <- 0;
## x_vector[1] <- 0.5;
## 
## for(ii in 1:50) {
##   y_vector[2 * ii] <- cobImapFun(
##     x_vector[2 * ii - 1]);
##   y_vector[2 * ii + 1] <-
##     y_vector[2 * ii];
##   x_vector[2 * ii] <- 
##     x_vector[2 * ii - 1];
##   x_vector[2 * ii + 1] <- 
##     y_vector[2 * ii + 1];
## }
## 
## plot( -10:50 / 10, 
##       cobImapFun(-10:50 / 10),
##       xlab='x', 
##       ylab='y', 
##       main='Example cobweb',
##       type='l', 
##       xlim=c(0, 4),
##       ylim=c(0, 2.5), 
##       xaxs='i', 
##       yaxs='i');
## lines(-2:6, -2:6, col='red');
## lines(x_vector, y_vector, 
##       col='blue');


###################################################
### chunk number 3: cobwebplot_func_11
###################################################
cobImapFun <- function(x) {
  return(2 * x - 0.5 * x ** 2);
}
max_iter <- 50;
y_vector <- x_vector <- numeric(max_iter * 2);
y_vector[1] <- 0;
x_vector[1] <- 0.5;

for(ii in 1:50) {
  y_vector[2 * ii] <- cobImapFun(x_vector[2 * ii - 1]);
  y_vector[2 * ii + 1] <- y_vector[2 * ii];
  x_vector[2 * ii] <- x_vector[2 * ii - 1];
  x_vector[2 * ii + 1] <- y_vector[2 * ii + 1];
}

plot(-10:50 / 10, cobImapFun(-10:50 / 10),
     xlab='x', ylab='y', main='Example cobweb',
     type='l', xlim=c(0, 4),
     ylim=c(0, 2.5), xaxs='i', yaxs='i');
lines(-2:6, -2:6, col='red');
lines(x_vector, y_vector, col='blue');



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

## pgfsweave-script.R
#!/usr/bin/env Rscript

version <- '$Id: pgfsweave-script.R 44 2009-04-30 07:06:55Z cameronbracken $\n'
usage <- "Usage: pgfsweave-script.R [options] file

A simple front-end for pgfSweave()

Options:
  -h, --help                print short help message and exit
  -v, --version             print pgfSweave version info and exit
  -d, --dvi                 dont use texi2dvi option pdf=T i.e. call latex 
                            (defalt is pdflatex)
  -s, --pgfsweave-only      dont compile to pdf/dvi, only run Sweave
  -n, --graphics-only       dont use the texi2dvi() funciton in R, compile 
                            graphics only ; ignored if --pgfsweave-only is
                            used

Package repositories: 
http://www.rforge.net/pgfSweave/ (for scm)
http://r-forge.r-project.org/projects/pgfsweave/ (for precompiled packages)
"

library(getopt)
library(pgfSweave)

#Column 3: Argument mask of the flag. An integer. Possible values: 
# 0=no argument, 1=required argument, 2=optional argument. 
optspec <- matrix(c(
  'help'          , 'h', 0, "logical",
  'version'       , 'v', 0, "logical",
  'dvi'           , 'd', 0, "logical",
  'pgfsweave-only', 's', 0, "logical",
  'graphics-only' , 'n', 0, "logical"
),ncol=4,byrow=T)

opt <- try(getopt(optspec),silent=TRUE)
if(class(opt) == 'try-error') opt <- list()

if( !is.null(opt$help    )) { cat(usage); q(status=1) }
if( !is.null(opt$version )) { cat(version); q(status=1) }
opt$dvi <- ifelse(is.null(opt$dvi), FALSE, TRUE )
opt[['pgfsweave-only']] <- ifelse(
                            is.null(opt[['pgfsweave-only']]), FALSE, TRUE )
opt[['graphics-only']] <- ifelse(
                            is.null(opt[['graphics-only']]), FALSE, TRUE )
                            
args <- commandArgs(TRUE)
file <- args[length(args)]

cat('pgfsweave-script.R: using options:\n')
print(opt)

if(opt[['graphics-only']]){
    # In the first case run through pgfSweave but no not compile document or 
    # graphics from within R. Instead, run the shell script generated by 
    # pgfSweave() to compile the graphics
    
    pgfSweave(file, pdf=!opt$dvi,compile.tex=FALSE) 
                   
        #compile the graphics
    bn <- strsplit(basename(file), "\\.Rnw")[[1]][1]
    dn <- dirname(file)
    fn <- file.path(dn, bn)
    cmds <- readLines(paste(fn, "sh", sep = "."))
    dummy <- lapply(cmds, system)
    
}else{
    # In this case, just make a call to the R function pgfSweave() with the
    # options intact.
    
    pgfSweave(file, pdf=!opt$dvi,compile.tex = !opt[['pgfsweave-only']])
}



## solution.R
###################################################
### chunk number 1: aa_SweaveListingsPreparations
###################################################
source('./listingPreps.R');
# library('pgfSweave');


###################################################
### chunk number 2: noprompt
###################################################
options(prompt=' ', continue=' ')


###################################################
### chunk number 3: functor_def eval=FALSE
###################################################
## f1 <- function (x) 
##   x * (5 - 4 * x)
## f2 <- function (x) 
##   2 * x * (1 - 3 * x / 2)
## f3 <- function (x)
##   x / 2 - x^2 / 5
## f4 <- function (x)
##   x * (5/2 - 7 * x)
## # source('./maxima_utilities.R');


###################################################
### chunk number 4: functor_def2 eval=FALSE
###################################################
## plotCobWeb <- function (f, max_iter=50) {
##   ## initialize variables
##   f_exp <- deparse(body(f))
##   df <- mDeriv(f);
##   df_exp <- deparse(body(df))
##   fixed_pts <- sort(mSolve.RServe(paste(f_exp, 'x', sep='=')));
##   zeros <- sort(mSolve.RServe(paste(f_exp, '0', sep='=')));
## 
##   carrying_cap <- max(zeros - fixed_pts) ^ -1;
##   rate <- max(fixed_pts) * carrying_cap + 1;
##   
##   init_value <- min(zeros) + diff(zeros) / 4;
##   y <- x <- numeric(length=max_iter*2);
##   x[1] <- init_value;
##   y[1] <- 0;
## 
##   ## Loop over iterations
## 
##   for (ii in seq(along=1:max_iter)) {
##     y[2 * ii + c(0,1)] <- f(x[2 * ii - 1]);
##     x[2 * ii + c(0,1)] <- c(x[2 * ii - 1], y[2 * ii + 1]);
##   }
## 
##   ## Determine plot limits
##   if (all(is.finite(x) & is.finite(y))) {
##     xlim <- c(min(floor(zeros)), max(ceiling(zeros)));
##     ylim <- c(  max(c(0, min(f(c(zeros, fixed_pts, mean(zeros)))))),
##                 max(f(c(zeros, fixed_pts, mean(zeros)))))
##   } else {
##     yranges <- xranges <- c(1);
##     for (ii in 2:length(x)) {
##       xranges <- c(xranges, diff(range(x[1:ii])));
##       yranges <- c(yranges, diff(range(y[1:ii])));
##     }
##     xlim <- c(min(floor(zeros)), max(ceiling(zeros)));
##     ymagnitudes <- na.omit(yranges[-1] /  yranges[-length(yranges)])
##     first_runaway <- min( (1:length(ymagnitudes) )[ymagnitudes > 100])
##     
##     ylim <- c(0, max(c(y[1:(first_runaway - 1)], f(mean(zeros)), f(zeros), 
##               f(fixed_pts))));
##   }
## 
##   ## Draw plot elements
##   curve(f, from=xlim[1], to=xlim[2], ylim=ylim, xlim=c(min(zeros), 
##         max(zeros)), fg=gray(0.6), bty='n',col='red', xlab="$t$", 
##         ylab="$f(t)$");
##   abline(h=0, lwd=1.5);
##   abline(v=0, lwd=1.5);
##   abline(0, 1, col='blue', lwd=1);
##   lines(x, y, col='darkgreen', lwd=1);
## 
##   ########################################################
##   ##  Page break only: Function continues on next page  ##
##   ########################################################
## }


###################################################
### chunk number 5: functor_def3 eval=FALSE
###################################################
## {
##   ##########################################################
##   ##  Page break only: Function continues from last page  ##
##   ##########################################################
## 
##   ## Calculate label positions
##   typ <- text_y_positions <- mean(ylim) - diff(ylim) / 15 * c(4, 5, 6, 7, 8);
##   txp <- mean(zeros)
## 
##   ## Add plot description
##   title(main=paste("{\\larger\\bf Cobweb plot of $ f(x) = ",
##         gsub("\\*", "", f_exp),"$ }"));
##   text(   x=mean(zeros), y=typ[1], 
##           paste(  "Fixed points: $ \\{ ", paste(as.character(fixed_pts), 
##                 collapse=", "), " $ \\} \nCarrying capacity $ k=", 
##                 carrying_cap, " $"));
##   if(rate < 3 && rate > 1) {
##     text(x=txp, y=typ[3], paste('Rate: $ r= ', rate,
##                                 "\\quad \\rightarrow 1 < r < 3$"));
##     text(x=txp, y=typ[4], "{\\bf $\\therefore $ the fixed point is stable }");
## 
##   } else {
##     text(x=txp, y=typ[3], 
##          paste('Rate: $r = ', rate, "\\quad \\rightarrow r > 3$") );
##     text(x=txp, y=typ[4], 
##          "{\\bf $\\therefore $ the fixed point is unstable }");
##   }
## }


###################################################
### chunk number 6: functor_def3
###################################################
f1 <- function (x) 
  x * (5 - 4 * x)
f2 <- function (x) 
  2 * x * (1 - 3 * x / 2)
f3 <- function (x)
  x / 2 - x^2 / 5
f4 <- function (x)
  x * (5/2 - 7 * x)

plotCobWeb <- function (f, max_iter=50) {
  ## initialize variables
  f_exp <- deparse(body(f))
  df <- mDeriv(f);
  df_exp <- deparse(body(df))
  fixed_pts <- sort(mSolve.RServe(paste(f_exp, 'x', sep='=')));
  zeros <- sort(mSolve.RServe(paste(f_exp, '0', sep='=')));

  carrying_cap <- max(zeros - fixed_pts) ^ -1;
  rate <- max(fixed_pts) * carrying_cap + 1;
  
  init_value <- min(zeros) + diff(zeros) / 4;
  y <- x <- numeric(length=max_iter*2);
  x[1] <- init_value;
  y[1] <- 0;

  ## Loop over iterations

  for (ii in seq(along=1:max_iter)) {
    y[2 * ii + c(0,1)] <- f(x[2 * ii - 1]);
    x[2 * ii + c(0,1)] <- c(x[2 * ii - 1], y[2 * ii + 1]);
  }

  ## Determine plot limits
  if (all(is.finite(x) & is.finite(y))) {
    xlim <- c(min(floor(zeros)), max(ceiling(zeros)));
    ylim <- c(  max(c(0, min(f(c(zeros, fixed_pts, mean(zeros)))))),
                max(f(c(zeros, fixed_pts, mean(zeros)))))
  } else {
    yranges <- xranges <- c(1);
    for (ii in 2:length(x)) {
      xranges <- c(xranges, diff(range(x[1:ii])));
      yranges <- c(yranges, diff(range(y[1:ii])));
    }
    xlim <- c(min(floor(zeros)), max(ceiling(zeros)));
    ymagnitudes <- na.omit(yranges[-1] /  yranges[-length(yranges)])
    first_runaway <- min( (1:length(ymagnitudes) )[ymagnitudes > 100])
    
    ylim <- c(0, max(c(y[1:(first_runaway - 1)], f(mean(zeros)), f(zeros), 
              f(fixed_pts))));
  }

  ## Draw plot elements
  curve(f, from=xlim[1], to=xlim[2], ylim=ylim, xlim=c(min(zeros), 
        max(zeros)), fg=gray(0.6), bty='n',col='red', xlab="$t$", 
        ylab="$f(t)$");
  abline(h=0, lwd=1.5);
  abline(v=0, lwd=1.5);
  abline(0, 1, col='blue', lwd=1);
  lines(x, y, col='darkgreen', lwd=1);

  ## Calculate label positions
  typ <- text_y_positions <- mean(ylim) - diff(ylim) / 15 * c(4, 5, 6, 7, 8);
  txp <- mean(zeros)

  ## Add plot description
  title(main=paste("{\\larger\\bf Cobweb plot of $ f(x) = ",
        gsub("\\*", "", f_exp),"$ }"));
  text(   x=mean(zeros), y=typ[1], 
          paste(  "Fixed points: $ \\{ ", paste(as.character(fixed_pts), 
                collapse=", "), " $ \\} \nCarrying capacity $ k=", 
                carrying_cap, " $"));
  if(rate < 3 && rate > 1) {
    text(x=txp, y=typ[3], paste('Rate: $ r= ', rate,
                                "\\quad \\rightarrow 1 < r < 3$"));
    text(x=txp, y=typ[4], "{\\bf $\\therefore $ the fixed point is stable }");

  } else {
    text(x=txp, y=typ[3], 
         paste('Rate: $r = ', rate, "\\quad \\rightarrow r > 3$") );
    text(x=txp, y=typ[4], 
         "{\\bf $\\therefore $ the fixed point is unstable }");
  }
}


###################################################
### chunk number 7: functor_def11
###################################################
f <- f1;
plotCobWeb(f)


###################################################
### chunk number 8: functor_def12
###################################################
f <- f2
plotCobWeb(f2)


###################################################
### chunk number 9: functor_def13
###################################################
f <- f3
plotCobWeb(f)


###################################################
### chunk number 10: functor_def14
###################################################
f <- f4
plotCobWeb(f)


###################################################
### chunk number 11: nonhomogenous_part2 eval=FALSE
###################################################
## glucose <- function(t) {
##   100 *  exp(-1 * t / 100) * (4 * exp(t / 100) - 3)
## }
## 
## xlab <- "time $(t)$ {\\smaller (minutes)}"
## ylab <- "Blood glucose concentration $G(t)$ {\\smaller(mg / dl)}"
## main <- "$100 \\, {(4 \\, e^{\\frac{1}{100} \\, t} - 3)} e^{-\\frac{1}{100} \\, t}$"
## plot(glucose, from=0, to=800, xlab=xlab, ylab=ylab, main=main);


###################################################
### chunk number 12: nonhomogenous_part3
###################################################
glucose <- function(t) {
  100 *  exp(-1 * t / 100) * (4 * exp(t / 100) - 3)
}

xlab <- "time $(t)$ {\\smaller (minutes)}"
ylab <- "Blood glucose concentration $G(t)$ {\\smaller(mg / dl)}"
main <- "$100 \\, {(4 \\, e^{\\frac{1}{100} \\, t} - 3)} e^{-\\frac{1}{100} \\, t}$"
plot(glucose, from=0, to=800, xlab=xlab, ylab=ylab, main=main);



## week2_lecture.R
###################################################
### chunk number 1: aa_SweaveListingsPreparations
###################################################
source('listingPreps.R');
options(prompt='>', continue=' ', width=40)
#.Rout$escapeinside="{(*@}{@*)}"
#.myRset$escapeinside="{(*@}{@*)}"
#SweaveListingoptions(Rset=.myRset, Rout=.Rout, intermediate = FALSE)
#SweaveListingPreparations()


###################################################
### chunk number 2: v1
###################################################
c('hello', TRUE); c(1, TRUE)
class(c('hello', TRUE)); class(c(1, TRUE));
c('Hello', 14);
class(c(TRUE, TRUE));


###################################################
### chunk number 3: v11
###################################################
my_name <- c('D', 'a', 'v', 'i',
             'd', ' ', 'R', '.');
length(my_name);
names(my_name);
names(my_name) <- 
  c(  paste('first', as.character(1:5),
          sep="_"), 
      'space',
      paste('last', as.character(1:2),
            sep="_") 
   );
my_name;


###################################################
### chunk number 4: v2
###################################################
temp_vec <- 1:5;
temp_vec
temp_vec <- c(temp_vec, 6);
temp_vec;
temp_vec2 <- c(temp_vec, 'Cat');
temp_vec2;
temp_vec[-2];
temp_vec[3] <- 100;
temp_vec;
temp_vec1 <- 1:5; temp_vec2 <- 7:10;
c(temp_vec1, 6, temp_vec2);


###################################################
### chunk number 5: index1
###################################################
my_name[1:3];
my_name[(1:3) * 2];
my_name[c(1, 5, length(my_name))];
my_name['first_3'];
my_name[c('first_1', 'last_1')]


###################################################
### chunk number 6: index2
###################################################
temp_vec <- 1:10;
temp_vec <= 5;
temp_vec[temp_vec <= 5];
my_name == 'D'
my_name[my_name == 'D'];
my_name[my_name == ' ' | my_name == '.'];
my_name[!(my_name %in% c(' ', '.'))];


###################################################
### chunk number 7: lists1
###################################################
list_ex <- list(my_name, 1:5, function(x) { return(1/x) });
list_ex;
list_ex2 <- list(name_ta=my_name, one_to_five=1:5, myfun = function(x) { return(1/x) });


###################################################
### chunk number 8: lists2
###################################################
list_ex2;
class(list_ex);
class(list_ex2[[1]]);


###################################################
### chunk number 9: dfoverview
###################################################
cost_per=c(0.10, 0.25, 0.50);
items=c('spam', 'egg', 'foobar');
on_hand=c(123, 153, 55);
df <- data.frame(items=items, on_hand=on_hand, cost_per=cost_per);
df
total_value <- df$on_hand * df$cost_per


###################################################
### chunk number 10: dfoverview1
###################################################
df <- cbind(df, total_value=total_value)
df
df2 <- cbind(df, weight_per=c(0.5, 0.1, 3))
df2


###################################################
### chunk number 11: dfoverview2
###################################################
df2 <- cbind(
      df2, 
      total_weight=df2$on_hand * df2$weight_per,
      value_density=df2$total_value/(df2$on_hand * df2$weight_per)
            )
df2
class(df2);
is.list(df2);


###################################################
### chunk number 12: complextypessyntax
###################################################
df <- data.frame(items=items, on_hand=on_hand, cost_per=cost_per);
list1 <- as.list(df);
list1;
names(df);
names(df)[1] <- c('My items');
df


###################################################
### chunk number 13: widen
###################################################
op <- options(); options(width=60)


###################################################
### chunk number 14: funcdoc01 eval=FALSE
###################################################
## help(ls);
## ?ls
## ?`+`  # use backquotes when R doesn't
##       #  like the question marks


###################################################
### chunk number 15: funcdoc02
###################################################
# help(package=RInterval); 
# truncated for space
cat(paste(help(package=RInterval)$info[[1]], collapse="\n"), '\n');


###################################################
### chunk number 16: funcdoc03
###################################################
help.search('digest');


###################################################
### chunk number 17: funcdoc04
###################################################
## help.start();


###################################################
### chunk number 18: funcdoc1
###################################################
example(is.integer);


###################################################
### chunk number 19: funcdoc2
###################################################
isTRUE;
lsf.str;


###################################################
### chunk number 20: unwiden
###################################################
options(op);



