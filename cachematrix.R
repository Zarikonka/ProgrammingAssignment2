makeCacheMatrix <- function(x= matrix()){
  inv <- NULL   ##making inverse as NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
    
  }
  get <- function() {x}   ##to get matrix
  setInverse <- function(inverse) {inv <<- inverse}  ## to get inverse matrix
  getInverse <- function() {inv}
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

##for getting the cache data 
cacheSolve <- function(x, ...){    ##getting cache data
  inv <- x$getInverse()
  if(!is.null(inv)){               ##checking whether or not inverse is null
    message("getting cached data")
    return(inv)                    ##returning inverse value
  }
  mat <- x$get()
  inv <- solve(mat, ...)           ##calculating inverse value
  x$setInverse(inv)
  inv                              ##returning inversed of x matrix
}


