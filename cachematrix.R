## This function creates a special "matrix" object that can cache its inverse
## It requires one parameter as a matrix. It will be the matrix kept and whose inverse is calculated
## It return 4 functions:
##    * get - getter function that return the matrix
##    * set - setter function that change the matrix for inversion
##    * getinverse - getter function that returns the calculated inverse or NULL if not calculated yet
##    * setinverse - setter funtion that keeps the calculated inverse
makeCacheMatrix <- function(x = matrix()) {
        ## Contains the inverse
        m <- NULL
        ## Sets the new matrix for inversion, clears the inversion data to avoid dirty cache reading
        set <- function(y) {
          x <<- y
          m <<- NULL
        }
        ## Returns the matrix for inversion
        get <- function() x
        ## Stores the inversion of the matrix as cache
        setinverse <- function(inverse) m <<- inverse
        ## Returns the inverse is already calculated, NULL otherwise
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed),
## then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
        ## If x does not contains getinverse is not a list created with makeCacheMatrix, create
        ## else continue
        if (!"getinverse" %in% names(x)) {
          p <- makeCacheMatrix(as.matrix(x))
        } else {
          p <- x
        }
        ## Get the inverse, just in case it has been already calculated, and return
        m <- p$getinverse()
        if(!is.null(m)) {
          message("getting cached data")
          return(m)
        }
        ## If not calculate, store in cache and return
        data <- p$get()
        m <- solve(data, ...)
        p$setinverse(m)
        m
}
