## These functions are working with matrix. The 1st one set and get a matrix and its inverse 
## the 2nd one calculate and set its inverse only if there is no a cached one yet

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  cachedInvoo <- NULL 
  set <- function(userValoo = matrix()) {
    x <<- userValoo
    cachedInvoo <<- NULL
  }
  
  get <- function() x
  setInverse <- function(invValaa) {
    cachedInvoo <<- invValaa 
    return(cachedInvoo)
  }
  
  getInverse  <- function() cachedInvoo
  list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
}


## Write a short comment describing this function

cacheSolve <- function(x=makeCacheMatrix(1:4, nrow=2, ncol=2), ...)
{ 
  
  calculatedInverse <- x$getInverse() 
  
  if(!is.null(calculatedInverse) && is.matrix(calculatedInverse)) { 
    return(calculatedInverse)
  }
  
  matrixToSolve <- x$get()  
  
  calculatedInverse <- tryCatch({ 
    solve(matrixToSolve)
  }, warning=function(w) {
    message("101")
    message(w)
  }, error=function(e) {
    message("202")
    message(e)
    message("\n")
  })
  
  message("303") 
  x$setInverse(calculatedInverse)
}
        ## Return a matrix that is the inverse of 'x'
