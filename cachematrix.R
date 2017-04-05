## The functions makeCacheMatrix and
## functions do


## The function makeCacheMatrix is a function which takes 
## a matrix as arguments and retrieve a list containing functions
## to set and get the value of that matrix and get and set the
## value of the inverse of that matrice. The special matrice obtained
## has the capacity to cache the value of its inverse. 

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinv <- function(solve) m <<- solve
        getinv <- function() m
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## The function cacheSolve takes a matrice and "..." as arguments. 
## It first check if the inverse of the matrice is "cached" in x$getinv. 
## If the inverse has already been calculated and cached, it retrieves the cached inverse
## Otherwise, it calculates the inverse of the matrix and caches its value.
##It returns the inverse of the matrice sent in the arguments.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinv()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinv(m)
        m
}
