#!/usr/bin/env r
# encoding: utf-8
#
# maxima_utils.R
# Copyright (c) 2009 David M. Rosenberg and the University of Chicago.
# All rights reserved.
#
# $Author: root $
# $LastChangedDate: 2009-10-19 10:20:21 -0500 (Mon, 19 Oct 2009) $
# $LastChangedRevision: 135 $
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

maxima_bin="ssh biostat@apalmerlab1.palmerlab.org"

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
  
  if (is.language(mc$expr)) {
    if (is.function(mc$expr)) {
      raw <- deparse(body(expr))
      safe_lines <- raw[grep('\\{|\\}', raw, invert=TRUE)];
      p1 <- gsub("^return\\((.*)\\)(;)?$", "\\1", 
                 gsub(" ", '', paste(safe_lines, collapse="")));
    } else if (class(trySilent(is.function(expr))) == 'try-error') {
      p1 <- as.character(as.expression(mc$expr));
    } else if (is.function(expr)) {
      raw <- deparse(body(expr))
      safe_lines <- raw[grep('\\{|\\}', raw, invert=TRUE)];
      p1 <- gsub("^return\\((.*)\\)(;)?$", "\\1", 
                 gsub(" ", '', paste(safe_lines, collapse="")));
    } else if (is.character(expr)) {
      safe_lines <- expr[grep('\\{|\\}', expr, invert=TRUE)];
      p1 <- gsub("^return\\((.*)\\)(;)?$", "\\1", 
                 gsub(" ", '', paste(safe_lines, collapse="")));
    } else {
      p1 <- as.character(as.expression(mc$expr));
    }
  } else if (is.expression(expr)) {
    p1 <- as.character(expr);
  } else if (is.character(expr)) {
    p1 <- expr;
  } else {
    stop('This should not happen.')
  }

  cmd <- paste('display2d:false; stardisp:true; print(diff(', p1, ',', var, 
               ',', as.character(degree), '));');
  cmd2 <- paste("echo 'echo \"", cmd, '" | ', "/usr/bin/maxima'", ' | ',
                maxima_bin);
  res <- system(cmd2, intern=TRUE, ignore.stderr=TRUE)[9];
  f <- function() {};
  args <- list(length=1);
  names(args) <- var;
  formals(f) <- args;
  body(f) <- parse(text=res);
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
mIntegr <- function(expr, var='x', degree=1) {
  mc <- as.list(match.call(expand.dots=TRUE));
  
  if (is.language(mc$expr)) {
    if (is.function(mc$expr)) {
      raw <- deparse(body(expr))
      safe_lines <- raw[grep('\\{|\\}', raw, invert=TRUE)];
      p1 <- gsub("^return\\((.*)\\)(;)?$", "\\1", 
                 gsub(" ", '', paste(safe_lines, collapse="")));
    } else if (class(trySilent(is.function(expr))) == 'try-error') {
      p1 <- as.character(as.expression(mc$expr));
    } else if (is.function(expr)) {
      raw <- deparse(body(expr))
      safe_lines <- raw[grep('\\{|\\}', raw, invert=TRUE)];
      p1 <- gsub("^return\\((.*)\\)(;)?$", "\\1", 
                 gsub(" ", '', paste(safe_lines, collapse="")));
    } else if (is.character(expr)) {
      safe_lines <- expr[grep('\\{|\\}', expr, invert=TRUE)];
      p1 <- gsub("^return\\((.*)\\)(;)?$", "\\1", 
                 gsub(" ", '', paste(safe_lines, collapse="")));
    } else {
      p1 <- as.character(as.expression(mc$expr));
    }
  } else if (is.expression(expr)) {
    p1 <- as.character(expr);
  } else if (is.character(expr)) {
    p1 <- expr;
  } else {
    stop('This should not happen.')
  }
  
  if (is.language(mc$expr)) {
    p1 <- expr <- as.character(as.expression(mc$expr));
  } else if(is.function(expr)) {
    raw <- deparse(body(f))
    safe_lines <- raw[grep('\\{|\\}', raw, invert=TRUE)];
    p1 <- gsub(" ", '', paste(safe_lines, collapse=""));
  } else if (is.expression(expr)) {
    p1 <- as.character(expr);
  } else {
    stop('This should not happen.')
  }
  p1 <- as.character(expr);

  cmd <- paste('display2d:false; stardisp:true; print(integrate(', p1, ',',
               var, ',', as.character(degree), '));');
  cmd2 <- paste("echo 'echo \"", cmd, '" | ', "/usr/bin/maxima'", ' | ', 
                maxima_bin);
  res <- system(cmd2, intern=TRUE, ignore.stderr=TRUE)[9];
  f <- function() {};
  args <- list(length=1);
  names(args) <- var;
  formals(f) <- args;
  body(f) <- parse(text=res);
  return(f);
}

# mTaylor <- function() {
#   
# }

mSolve <- function(expr, var='x') {
  p1 <- as.character(expr);
  cmd <- paste('display2d:false; stardisp:true; print(solve(', p1, ',', var,
               '));');
  cmd2 <- paste("echo 'echo \"", cmd, '" | ', "/usr/bin/maxima'", ' | ',
                maxima_bin);
  res <- system(cmd2, intern=TRUE)[9];
  res <- strsplit(gsub('\\]', '', gsub(var, '', gsub('[=|\\[]', '', res))), 
                  ', ')[[1]]
  solutions <- c();
  for (ii in res) {
    solutions <- c(solutions, eval(parse(text=ii)));
  }
  return(solutions);
}





##################################################
## More robust version here                     ##
##################################################

.createRserveEnv <- function() {
  rserve_hostname <- 'rosenbergdm.uchicago.edu';
  rserve_port <- 6311;
  rserve_user <- 'biostat';
  rserve_auth <- 'pErdos2';

  maxima_binary <- '/usr/local/bin/rmaxima'

  ## sanity check
  if (exists('.RMaximaEnv')) {
    stop(paste('The R socket server environment .RMaximaEnv is already',
                'in use.  Package loading aborted.'));
  }
  
  tryCatch( library('Rserve'), 
            error=function(e) {
              stop(paste('The RServe package could not be loaded.', 
                         'RMaxima package load aborted.') );
             }
          );
  
  .RMaximaEnv <<- new.env(parent=.GlobalEnv, hash=TRUE);
  RServeConnection <- RSconnect(host=rserve_hostname, port=rserve_port);
  RSlogin(RServeConnection, user=rserve_user, pwd=rserve_auth);
  assign('RServeConnection', envir=.RMaximaEnv, value=RServeConnection);
  rm(RServeConnection);
  
  assign( 'server_id', 
          envir=.RMaximaEnv, 
          value=digest::digest(c(date(), sessionInfo()))
        )
  
  RSassign( c=.RMaximaEnv$RServeConnection,
            name=paste('maxima_binary', .RMaximaEnv$server_id, sep=''),
            maxima_binary  );
}

.destroyRserveEnv <- function() {
  ##
  ## TODO: Package teardown.
  ##
}


mSolve.RServe <- function(equ, var='x', ..., symbolic=FALSE) {
  ##  sanity check
  if ( (length(equ) > 1) | symbolic ) {
    stop('Not yet implemented');
  } else if (length(equ) < 1) {
    stop('You must specify the equation(s) to solve.');
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
  server_id <- get('server_id', envir=.RMaximaEnv)
  
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
