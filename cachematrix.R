## It creates special object which can be used for cached calculation of inversion

makeCacheMatrix <- function(x = matrix()) {
        inversed <- NULL
        set <- function(y) {
                x <<- y
                inversed <<- NULL
        }
        get <- function() x
        setinversed <- function(mean) inversed <<- solve(x)
        getinversed <- function() inversed
        list(set = set, get = get,
             setinversed = setinversed,
             getinversed = getinversed)
}


## Function which makes fastened calculation for matrix inversion

cacheSolve <- function(x, ...) {
       inversed <- x$getinversed()
       if(!is.null(inversed)) {
                message("getting cached data")
                return(inversed)
       }
       data <- x$get()
       inversed <- solve(data, ...)
       x$setinversed(inversed)
       inversed
}
