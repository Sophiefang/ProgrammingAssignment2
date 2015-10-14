## The functions below are used to cache the iverse of a matrix


## MakecacheMatrix(x) creates a object that stores a matrix 
## and cache's its inverse

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function (y){
        x <<-y
        inv <<- NULL
    }
    get <- function() x
    setInv <- function(inv) inv <<- solve(x)
    getInv <- function() inv
    list(set=set, get=get,
         setInv=setInv,
         getInv=getInv)
}


## cacheSolve() calculates the inverse of the matrix created 
## using above functions
## However, It first checks if the inverse has already been 
## calculated. If so, it gets the inverse from the cache and 
## skips the computation. Otherwise, it calculates the inverse 
## and sets the value of inverse in the cache

cacheSolve <- function(x, ...) {     
    inv <- x$getInv()
    if (! is.null(inv)){
        message("getting cached inverse value of the matrix")
        return (inv)
    }
    data <- x$get()
    inv <- solve(data)
    x$setInv(inv)
    inv
}