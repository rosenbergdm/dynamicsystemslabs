#!/usr/bin/env r
# encoding: utf-8
#
# Lab0_2.R
# Copyright (c) 2009 David M. Rosenberg and the University of Chicago.
# All rights reserved.
#
# $Author: root $
# $LastChangedDate: 2009-09-14 22:33:58 -0500 (Mon, 14 Sep 2009) $
# $LastChangedRevision: 19 $
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
# Created on 2009-09-08
##########################################################################

setClass('MenuItem',
         representation(key='character',
                        description='character',
                        callback='function'
         ),
);

noOp <- function() { }

MenuItem <- function(key, description, callback=NULL) {
  if (is.null(callback)) {
    callback <- noOp;
  }
  if((nchar(key) != 1) | (toupper(key) != key)) {
    stop('key must be a single capital letter');
  }
  if((nchar(description) > 25)) {
    warning('Description is being truncated to 25 characters.');
    description <- substr(description, 1, 25);
  }
  obj <- new('MenuItem', key=key, description=description, callback=callback)
}

setGeneric('print');

setMethod('print', signature('BasicMenu'), function(x, ...) {
    cat(paste(rep('\n', 40), collapse='\n'));
    cat(x@title, x@instructions, '\n', sep="\n");
    for (ii in seq(along=x@items)) {
      textOut <- ''
      descLength=nchar(x@items[[ii]]@description)
      nSpaces <- 60 - descLength
      space <- paste(rep(' ', nSpaces), collapse="")
      textOut <- paste(x@items[[ii]]@description, space, x@items[[ii]]@key, sep="", collapse="")
      cat(textOut, '\n')
    }
  }
);

setGeneric('getSelection', function(object, ...) {
  return(standardGeneric('getSelection'));
})

setMethod('getSelection', signature('BasicMenu'), function(object, ...) {
    inputChar <- '_';
    choices <- c();
    for (ii in seq(along=object@items)) {
      choices <- c(choices, object@items[[ii]]@key);
    }
    while (!inputChar %in% choices) {
      inputString <- readline(prompt=paste(object@instructions, '> ', sep=""));
      inputChar <- substr(inputString, 1, 1);
    }
    return((1:length(choices))[choices %in% inputChar]);
  }
);

setGeneric('loopOver', function(object, ...) {

  }
);

setClass('MenuItemList',
         representation(items='list'),
         validity=function(object) {
           for(ii in seq(along=object@items)) {
             if(!(is(object@items[ii], 'MenuItem'))) {
               return('A MenuItemList may only contain MenuItems.')
             }
           }
           return(TRUE);
         }
);

setClass('BasicMenu',
         representation(title='character',
                        instructions='character'
                       ),
         contains=c('MenuItemList')
);


setClass('DataModel',
         representation(model='function',
                        coefficients='numeric',
                        restrictions='list'
                       )
);

setClass('Session',
         representation(username='character',
                        history='list',
                        observations='data.frame',
                        data='DataModel',
                        seed='numeric'
                       )
);

printableHelp <- function(topic) {
  helpFile <- help(topic);
  if (length(helpFile) == 0) {
    stop("No help was found for topic ", topic);
  }
  helpFileLines <- readLines(helpFile);
  helpFileText <- paste(helpFileLines, collapse='\n');
  helpFileText <- gsub('_\\\b(.{1})', '\\1', helpFileText);
  return(helpFileText);
}

reformatSource <- function(inputFile, outputFile=NULL, overwrite=FALSE) {
  if (!file.exists(inputFile)) {
    stop(inputFile, " does not exist!\n");
  }
  if (is.null(outputFile)) {
    outputFile <- gsub('(.*)\\.(.*)', '\\1\\.reformatted\\.\\2', inputFile);
  }
  if (file.exists(outputFile)) {
    if (!overwrite) {
      warning(outputFile, " already exists but is being overwritten.\n");
    } else {
      stop(outputFile, " already exists.  use overwrite=TRUE to force.");
    }
  }
  protectComments <- function(textLine) {
    outLine <- gsub('( +)?#(.*)\n', 'cat("\\1 #\\2\n")', textLine);
    return(outLine);
  }

  unprotectComments <- function(textLine) {
    outLine <- gsub('cat\\(\\\"(.*)#(.*)\\\"\\)', '\\1#\\2', textLine)
    return(outLine);
  }

  sourceLines <- readLines(inputFile);
  tmp <- file(tempfile(), 'w');

  protectedLines <- character(length=length(sourceLines))
  for (ii in seq(along=sourceLines)) {
    protectedLines[ii] <- protectComments(sourceLines[ii])
  }
  writeLines(protectedLines, con=tmp, sep="");
  close(tmp);


}

postCodeFile <- function(inputFile) {

}

winToUnix <- function(inputFile) {

}

unixToWin <- function(inputFile) {

}


submitFile <- function(inputFile) {


}



parsePolynomial <- function(inputExpression, referenceVariable='x') {
  numberTerms <- nchar(gsub("[^x]", "", inputExpression));
  coefficients <- numeric(length=numberTerms+1);
  exponents <- numeric(length=numberTerms+1);
  splitTerms <- strsplit(gsub('-', '+ -', inputExpression), '\\+')[[1]]
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
  o <- order(exponents);
  exponents <- exponents[o];
  coefficients <- coefficients[o];
  result <- list(exponents=exponents, coefficients=coefficients);
  return(result);
}


deparsePolynomial <- function(inputObject) {
  outString <- '';
  for (ii in seq(along=inputObject$exponents)) {
    outString <- paste(outString, ' + ', inputObject$coefficients[ii],
                       'x^', inputObject$exponents[ii], sep='');
  }
  outString <- gsub('\\+ -', '-', outString);
  outString <- gsub('x\\^0', '', outString);
  outString <- gsub('x\\^1', 'x', outString);
  outString <- gsub(' ', '', outString);
  outString <- gsub('^\\+', '', outString);
  outString <- gsub('x\\^\\-(\\d+)', 'x\\^\\(-\\1\\)', outString)
  outString <- gsub('([\\+|-])1x', '\\1x', outString);
  return(outString);
}