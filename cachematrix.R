# Matrix inversion can be a costly computation and there may be some benefit 
# to caching the inverse of a matrix rather than compute it repeatedly. The 
# following two functions are used to cache the inverse of a matrix. 


# The following function creates a list containing a function to 
# 1. set the value of a matrix 
# 2. get the value of a matrix 
# 3. set the value of inverse of a matrix 
# 4. get the value of inverse of a matrix 

makeCacheMatrix <- function(x = matrix()) { 
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setmatrix <- function(solve) m <<- solve
        getmatrix <- function() m
        list(set = set, get = get,
                setmatrix = setmatrix,
                getmatrix = getmatrix)
}




# The following function returns the inverse of the matrix.  If the inverse has 
# already been computed,it should return the inverse from cache and skip the 
# computation. Otherwise,  it should compute the inverse, set the value in the cache via 
# setmatrix function. 
 

# This function assumes that the matrix is always invertible 

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
        m <- x$getmatrix()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        mdata <- x$get()
        m <- solve(mdata, ...)
        x$setmatrix(m)
        m
}



# Sample Run
# k <- matrix(c(-1, -2, 1, 1), 2,2)
# x <- makeCacheMatrix(k)
# x$get() 
##      [,1] [,2]
## [1,]   -1    1
## [2,]   -2    1
# cacheSolve(x) - first time
##      [,1] [,2]
## [1,]    1   -1
## [2,]    2   -1
# cacheSolve(x) - second time
## getting cached data
##      [,1] [,2]
## [1,]    1   -1
## [2,]    2   -1