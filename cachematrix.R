
makeCacheMatrix <- function(x = matrix()) {
## makeCacheMatrix initializes objects with a given matrix
##     
## makeCacheMatrix takes a given matrix and initializes four operations below:    
##   set: initialize makeCacheMatrix with a given matrix
##   get: return the given matrix
##   setInv: initialize the inverse of matrix
##   getInv: return the inverse of the matrix
     
     
     ## initialize inv_m, the variable for the inverse of a matrix, to NULL
     inv_m <- NULL  
     
     ## initialize operations of makeCacheMatrix
     set <- function(y) {
          x <<- y
          inv_m <<- NULL
     }
     get <- function() x
     setInv <- function(inv) inv_m <<- inv 
     getInv <- function() inv_m
     
     ## returns four operations as a list
     list(set = set, get = get,
          setInv = setInv,
          getInv = getInv)   
}


cacheSolve <- function(x, ...) {
## cacheSolve function returns an inverse of given matrix
##     
## if the given matrix has not changed, cacheSolve does not solve the matrix 
## and returns the cached inverse of the matrix
## if the given matrix has changed, cacheSolve solves for the inverse of given matrix,
## sets the inverse as cached, and then returns the inverse of the given matrix 
     
     
     # get the cached inverse
     inv_m <- x$getInv() 
     
     # check to see if the inverse of a matrix has been cached
     # if cached, returns the cached inverse
     if(!is.null(inv_m)) {     
          message("getting cached data")
          return(inv_m)
     }
     
     # if not cached, solve for the inverse of a matrix, set the cache, and return the inverse
     data <- x$get()
     inv_m <- solve(data, ...)
     x$setInv(inv_m)
     inv_m
}
