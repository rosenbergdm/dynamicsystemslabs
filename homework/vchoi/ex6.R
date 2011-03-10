## Vchoi ex6


myEigen = function(matrix) {
  det = (matrix[1,1]*matrix[2,2]) - (matrix[1,2]*matrix[2,1])
  trace = matrix[1,1]+matrix[2,2]
  if((4*det)>(trace^2)) {
    tosqrt = (trace^2) - (4*det)
    tosqrt = as.complex(tosqrt)
    e1 = (trace + sqrt(tosqrt))/2
    e2 = (trace - sqrt(tosqrt))/2
  } else {
    e1 = (trace + sqrt((trace^2)-(4*det)))/2
    e2 = (trace - sqrt((trace^2)-(4*det)))/2
  }
  return (c(e1,e2))
}

#############################################################################
drawPhasePlane = function(mat, xmin=-10, xmax=10, ymin=-10, ymax=10) { 
  stepsize = 1
  xsteps = ((xmax-xmin+1)/stepsize)
  ysteps = ((ymax-ymin+1)/stepsize)
  x = xmin
  plot(c(xmin,xmax), c(ymin,ymax), xlab='X', ylab='Y',type='n')
  abline(h=0)
  abline(v=0)
  for(ii in 1:xsteps) {
    y = ymin
    for(jj in 1:ysteps) {
      x2 = x*mat[1,1] + y*mat[1,2]
      y2 = x*mat[2,1] + y*mat[2,2]
      v = normalizeVector(c(x2, y2))
      arrows(x, y, v[1]+x, v[2]+y, length=0.06)
      y = y+stepsize
    }
    x = x+stepsize
  }
}

##############################################################################
drawPhasePlane2 = function(mat, initVals, xmin=-10, xmax=10, ymin=-10, ymax=10) { 
  stepsize = 1
  xsteps = ((xmax-xmin+1)/stepsize)
  ysteps = ((ymax-ymin+1)/stepsize)
  x = xmin
  plot(c(xmin,xmax), c(ymin,ymax), xlab='X', ylab='Y',type='n')
  abline(h=0)
  abline(v=0)
  for(ii in 1:xsteps) {
    y = ymin
    for(jj in 1:ysteps) {
      x2 = x*mat[1,1] + y*mat[1,2]
      y2 = x*mat[2,1] + y*mat[2,2]
      v = normalizeVector(c(x2, y2))
      arrows(x, y, v[1]+x, v[2]+y, length=0.06)
      y = y+stepsize
    }
    x = x+stepsize
  }
  if(!is.null(initVals)) {
    for(ii in 1:length(initVals)) {
      solnTab=solveOdeSystem(mat, initVals[[ii]][1], initVals[[ii]][2]) 
      lines(solnTab$x, solnTab$y, col=ii, lwd=2)
    }
  }
}

#########################################################################
#provided helper functions
#########################################################################
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


normalizeVector = function(vec, total=0.8) {
  iLength = sqrt(sum(vec^2))
  return(vec*total/iLength)
}
