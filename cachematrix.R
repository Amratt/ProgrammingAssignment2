# Matrix inversion is usually a costly computation and there may be some benefit
# to caching the inverse of a matrix rather than compute it repeatedly. The
# following two functions are used to cache the inverse of a matrix.

## # makeCacheMatrix creates a list containing a function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix

# makeCacheMatrix: This function creates a special "matrix" 
# object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
        myInv <- NULL
        setMat <- function(y) {
                x <<- y
                myInv <<- NULL
        }
        getMat <- function(){
                x
        } 
        setInv <- function(invVal){
                myInv <<- invVal
        } 
        getInv <- function(){
                myInv
        } 
        list(setMat = setMat, getMat = getMat,
             setInv = setInv,
             getInv = getInv)
}



#cacheSolve: This function computes the inverse of the special "matrix" 
#returned by makeCacheMatrix above. If the inverse has already been calculated
# then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        myInv <- x$getInv()
        if(!is.null(myInv)) {
                message("getting cached data")
                return(myInv)
        }
        data <- x$getMat()
        myInv <- solve(data)
        x$setInv(myInv)
        myInv
}

# RUN TEST #
#  x = rbind(c(3, 3.5), c(3.2, 3.6))
#  temp1 = makeCacheMatrix(x)
#  temp1$getMat()
#  [,1] [,2]
#  [1,]  3.0  3.5
#  [2,]  3.2  3.6
#  y = rbind(c(5, 1), c(1, 3))
#  temp1$setMat(y)
#  temp1$getMat()
#  [,1] [,2]
#  [1,]    5    1
#  [2,]    1    3
#  cacheSolve(temp1)
#  [,1]        [,2]
#  [1,]  0.21428571 -0.07142857
#  [2,] -0.07142857  0.35714286