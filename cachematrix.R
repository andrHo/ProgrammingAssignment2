#Creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) m <<- inverse
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


#Returns the inverse of the matrix x. The inverse is retrieved 
#from cache if it has already been calculated.
cacheSolve <- function(x, ...) {
    #Try to get cached inverse
    m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    #Compute inverse and save it to the matrix object
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}
