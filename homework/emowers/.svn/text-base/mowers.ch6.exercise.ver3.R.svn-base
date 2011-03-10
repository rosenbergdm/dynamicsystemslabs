# 
# name: Erin Mowers
# assignment: Chapter 6
# date: 12/4/09
# filename: Mowers_Ch6_Exercise
#############################################################################







#/--#########################################################################
# name: Erin Mowers
# assignment: Chapter 6
# date: 12/04/09
# question: 1
# subquestion: a
# other files: 
##########################################################################/--

ErinEigen <- function(M) {
  a <- as.complex(M[1,1])
  b <- as.complex(M[1,2])
  c <- as.complex(M[2,1])
  d <- as.complex(M[2,2])
  trace <- a + d
  determinant <- (a*d) - (b*c)
  eigen1 <- (trace + sqrt((trace)^2 - (4 * determinant))) / 2
  eigen2 <- (trace - sqrt((trace)^2 - (4 * determinant))) / 2
  eigens <- c(eigen1, eigen2)
  return(eigens)
  }





#/--#########################################################################
# name: Erin Mowers
# assignment: Chapter 6
# date: 12/04/09
# question: 1
# subquestion: b
# other files: 
##########################################################################/--

testEigenFunction <- function(fun, nTests=50) {
  for (ii in 1:nTests) {
    A <- matrix(floor(runif(n=4, min=-100, max=100)), nrow=2);
    sysResult <- as.complex(eigen(A)$values);
    testResult <- as.complex(fun(A));
    if (! (isTRUE(all.equal(sysResult, testResult)) ||
           isTRUE(all.equal(sysResult, testResult[c(2, 1)])) ) ) {
      cat(paste('Error encountered in test #', ii,
                ', further tests aborted.\n', sep='') );
      return(list(testMatrix=A, systemResult=sysResult,
                  testResult=testResult));
    }
  }
  cat(paste('Success.  All ', nTests,
            ' tests completed successfully.\n', sep=''));
}

#To test:
testEigenFunction(ErinEigen, nTests=50)



#/--#########################################################################
# name: Erin Mowers
# assignment: Chapter 6
# date: 12/04/09
# question: 1
# subquestion: c
# other files: NA
##########################################################################/--

M1 <- matrix(c(0,1,-2,0), nrow=2)
M2 <- matrix(c(3,4,2,1), nrow=2)
M3 <- matrix(c(-4,3,-2,-1), nrow=2)
M4 <- matrix(c(2,1,1,2), nrow=2)

#To find the eigenvalues:
ErinEigen(M1)
#Zero real part, nonzero imaginary part

ErinEigen(M2)
#Positive real part, zero imaginary part
#Negative real part, zero imaginary part

ErinEigen(M3)
#Negative real part, nonzero imaginary part


ErinEigen(M4)
#Positive real part, zero imaginary part




#/--#########################################################################
# name: Erin Mowers
# assignment: Chapter 6
# date: 12/04/09
# question: 2
# subquestion: a
# other files: NA
##########################################################################/--

normalizeVector <- function(vec, total=1.0) {
  iLength <- sqrt(sum(vec ^ 2));
  return(vec * total / iLength)
}

#For the below, M is a 2x2 matrix,
#xRange and yRange are vectors of length two.
phase_plane <- function(M, xRange, yRange) {
  a <- M[1,1]
  b <- M[1,2]
  c <- M[2,1]
  d <- M[2,2]
  plot(c(xRange[1], xRange[2]), c(yRange[1], yRange[2]),
    type='n', main='Mowers 2a: Phase Plane',
    xlab='x', ylab='y')
  xstep <- abs(xRange[1] - xRange[2]) / 20
  ystep <- abs(yRange[1] - yRange[2]) / 20
  xVec <- yVec <- numeric(length=21)
  xVec[1] <- xRange[1]
  yVec[1] <- yRange[1]
  for(ii in 2:21) {
    xVec[ii] <- xVec[ii - 1] + xstep
    yVec[ii] <- yVec[ii - 1] + ystep
  }
  for(ii in 1:21) {
    for(iii in 1:21) {
      xdot <- (a * xVec[ii]) + (b * yVec[iii])
      ydot <- (c * xVec[ii]) + (d * yVec[iii])
      derVec <- normalizeVector(c(xdot, ydot), total=1.0)
      arrows(xVec[ii], yVec[iii],
        xVec[ii] + derVec[1], yVec[iii] + derVec[2], length=0.05)
    }
  }
  return('See graph')
}



#/--#########################################################################
# name: Erin Mowers
# assignment: Chapter 6
# date: 12/04/09
# question: 2
# subquestion: b
# other files: NA
##########################################################################/--

normalizeVector <- function(vec, total=1.0) {
  iLength <- sqrt(sum(vec ^ 2));
  return(vec * total / iLength)
}

solveOdeSystem <- function(mat, x, y) {
  e <- eigen(mat);
  eVectors <- e$vectors;
  eVals <- e$values;
  ab <- solve(eVectors, matrix(c(x, y), nrow=2));
  A <- ab[1];
  B <- ab[2];
  t <- seq(0, 100, by=0.01);
  xx <- (A * eVectors[1, 1]) * exp(eVals[1] * t) + 
        (B  * eVectors[1, 2]) * exp(eVals[2] * t);
  yy <- (A * eVectors[2, 1]) * exp(eVals[1] * t) + 
        (B  * eVectors[2, 2]) * exp(eVals[2] * t);
  return(list(x=xx, y=yy));
}

#For the below, M is a 2x2 matrix,
#xRange and yRange are vectors of length two, and
#initialVals is a list of paired x & y values.
phase_plane2 <- function(M, xRange, yRange, initialVals=NULL) {
  a <- (M[1,1])
  b <- (M[1,2])
  c <- (M[2,1])
  d <- (M[2,2])
  plot(c(xRange[1], xRange[2]), c(yRange[1], yRange[2]),
    type='n', main='Mowers: Phase Plane with Solution Curves',
    xlab='x', ylab='y')
  xstep <- abs(xRange[1] - xRange[2]) / 20
  ystep <- abs(yRange[1] - yRange[2]) / 20
  xVec <- yVec <- numeric(length=21)
  xVec[1] <- xRange[1]
  yVec[1] <- yRange[1]
  for(ii in 2:21) {
    xVec[ii] <- xVec[ii - 1] + xstep
    yVec[ii] <- yVec[ii - 1] + ystep
  }
  for(ii in 1:21) {
    for(iii in 1:21) {
      xdot <- (a * xVec[ii]) + (b * yVec[iii])
      ydot <- (c * xVec[ii]) + (d * yVec[iii])
      derVec <- (normalizeVector(c(xdot, ydot), total=1.0))
      arrows(xVec[ii], yVec[iii],
        xVec[ii] + derVec[1], yVec[iii] + derVec[2], length=0.05)
    }
  }
  if(!is.null(initialVals)) {
    iter2 <- length(initialVals)
    for(ii in 1:iter2) {
      solutions <- solveOdeSystem(M, initialVals[[ii]][1], initialVals[[ii]][2])
      lines(solutions$x, solutions$y, col=ii+1, lwd=3)
    }
  }
  return('See graph')
}



#/--#########################################################################
# name: Erin Mowers
# assignment: Chapter 6
# date: 12/04/09
# question: 2
# subquestion: c
# other files: NA
##########################################################################/--

mat1 <- matrix(c(0,1,-2,0), nrow=2)
mat2 <- matrix(c(3,4,2,1), nrow=2)
mat3 <- matrix(c(-4,3,-2,-1), nrow=2)
mat4 <- matrix(c(2,1,1,2), nrow=2)

initialValues <- list(c(3,7), c(-4, 0.5))

#To produce the plots requested:
phase_plane2(mat1, c(-12,12), c(-10,10), initialValues)
phase_plane2(mat2, c(-20,20), c(-20,20), initialValues)
phase_plane2(mat3, c(-10,10), c(-10,10), initialValues)
phase_plane2(mat4, c(-10,10), c(-10,10), initialValues)




#/--#########################################################################
# name: Erin Mowers
# assignment: Chapter 6
# date: 12/04/09
# question: 3
# subquestion: a
# other files: See handwritten pages.
##########################################################################/--



#/--#########################################################################
# name: Erin Mowers
# assignment: Chapter 6
# date: 12/04/09
# question: 3
# subquestion: b
# other files: See handwritten pages.
##########################################################################/--



#/--#########################################################################
# name: Erin Mowers
# assignment: Chapter 6
# date: 12/04/09
# question: 3
# subquestion: c
# other files: See print outs of graphs for explanation.
##########################################################################/--

relationship1 <- matrix(c(2,1,1,2), nrow=2)
phase_plane2(relationship1, c(-10,10), c(-10,10))

relationship2 <- matrix(c(1,-1,-1,1), nrow=2)
phase_plane2(relationship2, c(-10,10), c(-10,10))

relationship3 <- matrix(c(1,-2,-2,1), nrow=2)
phase_plane2(relationship3, c(-10,10), c(-10,10))

relationship4 <- matrix(c(-3,-2,-2,-3), nrow=2)
phase_plane2(relationship4, c(-10,10), c(-10,10))




##############
#Provided functions
##############

normalizeVector <- function(vec, total=1.0) {
  iLength <- sqrt(sum(vec ^ 2));
  return(vec * total / iLength)
}

solveOdeSystem <- function(mat, x, y) {
  e <- eigen(mat);
  eVectors <- e$vectors;
  eVals <- e$values;
  ab <- solve(eVectors, matrix(c(x, y), nrow=2));
  A <- ab[1];
  B <- ab[2];
  t <- seq(0, 100, by=0.01);
  xx <- (A * eVectors[1, 1]) * exp(eVals[1] * t) + 
        (B  * eVectors[1, 2]) * exp(eVals[2] * t);
  yy <- (A * eVectors[2, 1]) * exp(eVals[1] * t) + 
        (B  * eVectors[2, 2]) * exp(eVals[2] * t);
  return(list(x=xx, y=yy));
}

testEigenFunction <- function(fun, nTests=50) {
  for (ii in 1:nTests) {
    A <- matrix(floor(runif(n=4, min=-100, max=100)), nrow=2);
    sysResult <- as.complex(eigen(A)$values);
    testResult <- as.complex(fun(A));
    if (! (isTRUE(all.equal(sysResult, testResult)) ||
           isTRUE(all.equal(sysResult, testResult[c(2, 1)])) ) ) {
      cat(paste('Error encountered in test #', ii,
                ', further tests aborted.\n', sep='') );
      return(list(testMatrix=A, systemResult=sysResult,
                  testResult=testResult));
    }
  }
  cat(paste('Success.  All ', nTests,
            ' tests completed successfully.\n', sep=''));
}

testEigenFunction2 <- function(fun, nTests=50) {
  for (ii in 1:nTests) {
    mat <- matrix(floor(runif(n=4, min=-100, max=100)), nrow=2);
    
    sSol <- eigen(mat);
    sSol$vectors[, 1] <- as.complex(normalizeVector(sSol$vectors[, 1]));
    sSol$vectors[, 2] <- as.complex(normalizeVector(sSol$vectors[, 2]));
    sSol$values <- as.complex(sSol$values);
    
    mySol <- fun(mat);
    mySol$vectors[, 1] <- as.complex(normalizeVector(mySol$vectors[, 1]));
    mySol$vectors[, 2] <- as.complex(normalizeVector(mySol$vectors[, 2]));
    mySol$values <- as.complex(mySol$values);
    
    if (!( isTRUE(all.equal(sSol$values, mySol$values)) || 
           isTRUE(all.equal(sSol$values, mySol$values[c(2, 1)] )) )) {
      cat(sprintf('Error: eigenvalue result mismatch on iteration %d.\n',
                  ii));
      return(list(matrix=mat, sSolution=sSol, mySolution=mySol));
    }
    if (!(isTRUE(all.equal(sSol$values, mySol$values)))) {
      sSol$vectors <- sSol$vectors[, c(2, 1)];
    }
    if (   (!isTRUE(all.equal(sSol$vectors[,1], mySol$vectors[,1]))) && 
           (!isTRUE(all.equal(-1 * sSol$vectors[,1], mySol$vectors[,1]))) ) {
      cat(sprintf('Error: eigenvector result mismatch on iteration %d.\n',
          ii));
      return(list(matrix=mat, sSolution=sSol, mySolution=mySol));
    } else if ( (!isTRUE(all.equal(sSol$vectors[,2], mySol$vectors[,2]))) &&
                (!isTRUE(all.equal(-1 * sSol$vectors[,2],
                                        mySol$vectors[,2])))) {
      cat(sprintf('Error: eigenvector result mismatch on iteration %d.\n',
                  ii));
      return(list(matrix=mat, systemSolution=sSol, mySolution=mySol));
    }
  }
  cat(sprintf('Success over %d trials.\n', nTests));
}

