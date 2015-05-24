## makeCacheMatrix and cacheSolve functions for caliculating inverse of a matrix
## The functions use variable scoping to implement caching for performance gain
##
## conditions       : matrix must be inverible
## usage            : 'm' be a invertible matrix, then
##                    m1 <- makeCacheMatrix(m) , then
##                    cacheSolve(m1)
##                            alternately
##                    cacheSolve(makeCacheMatrix(m))


## makeCacheMatrix  : creates the modefied matrix object, 
##                    returns list object with references to getter and setter functions
##                    accepts invertible matrix as input
##
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(mx){    ## store the matrix
    x <<- mx
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(minv) inv <<- minv
  getinv <- function() inv
  
  ## construct the list object with references to getters and setters
  list(set = set, get = get, setinv = setinv, getinv = getinv ) 
}


## cacheSolve       : solves for matrix inverse and stores inverse object in cache
##                    if already solved, retrieves previously stored inverse from cache
##                    returns inverse of matrix
##                    accepts the list object returned by makeCacheMatrix as input
## 
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()    ## retrieve the inverse
  if (!is.null(inv)){  ## check if inverse already caliculated and stored
    message("getting cached inverse of the matrix")
    return(inv)
  }
  
  ## inverse not available in cache, caliculate it
  mat <- x$get() ## get the matrix
  inv <- solve(mat) 
  x$setinv(inv)  ## store the caliculated inverse
  inv
}

test = function(mat){
  
  ## @mat: an invertible matrix
  
  temp = makeCacheMatrix(mat)
  
  start.time = Sys.time()
  cacheSolve(temp)
  dur = Sys.time() - start.time
  print(dur)
  
  start.time = Sys.time()
  cacheSolve(temp)
  dur = Sys.time() - start.time
  print(dur)
}