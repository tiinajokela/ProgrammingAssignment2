
a<-matrix(c(-1, -2, 1, 1), 2, 2)
#make function which makes list of functions
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setsolve <- function(solve) m <<- solve
    getsolve <- function() m
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)
}

resultslist<-makeCacheMatrix(a)
# calculates the mean of spesific vector, while checking if mean has allready calculated and cached.
cacheSolve<- function(x, ...) {
    m <- x$getsolve()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setsolve(m)
    m
}


cacheSolve(resultslist)
