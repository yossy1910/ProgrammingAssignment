## 1.Creates a special matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    m<-NULL
    set<-function(y){
        x<<-y
        m<<-NULL
    }
    get <- function() {
        x
    }
    setInverse <- function(inverse) {
        i <<- inverse
    }
    getInverse <- function() {
        m
    }
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}




## 2.Compute the inverse of the special matrix returned by "makeCacheMatrix"

cacheSolve <- function(x, ...) {
    m <- x$getInverse()
    if( !is.null(m) ) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data) %*% data
    x$setInverse(m)
    m
}