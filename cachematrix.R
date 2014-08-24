## Put comments here that give an overall description of what your
## functions do

# My Comments 1A:
# Overall description
# ###################
# 'makeCacheMatrix' are used to create an object that stores 
# a numeric matrix and caches the inverse of a matrix.
# 'cacheSolve' inverse the matrix and cash it via 'makeCacheMatrix'.
#
# For the functions 'makeCacheMatrix' and 'cacheSolve',  
# 'makeVector' and 'cachemean' are used as template, 
# see below 'My Comments 1B'.
#
# How to test the functions:
# 1. Set the matrix (needs to be square invertible matrix)
## To do that first store a function in a variable (sf => stored function):
## sf <- makeCacheMatrix()
## call the "set method" in function makeCacheMatrix, which set the matrix:
## sf$set( matrix( c(1 ,-1 , 1, -1, 2, 1, -1, 3, 4), nrow=3, ncol=3) )
# 2. To inverse the matrix:
## cacheSolve(sf)
# 3. Summary of test. Run the following:
## sf <- makeCacheMatrix()
## sf$set( matrix( c(1 ,-1 , 1, -1, 2, 1, -1, 3, 4), nrow=3, ncol=3) ) 
## cacheSolve(sf)
# 4. Expected output:
##      [,1] [,2] [,3]
## [1,]    5    3   -1
## [2,]    7    5   -2
## [3,]   -3   -2    1


## Write a short comment describing this function
# My Comments 2 (text taken ftom asignment):
# 'makeCacheMatrix': This function creates a special "matrix" object
# that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    # im = inverse of a matrix
    # 'im' assigned to NULL, with length 0
    im <- NULL 
    # 1.  set the value of the matrix
    set <- function(y) { 
        # cashes the matrix into variable x
        x <<- y 
        # 'im' is cashed to NULL, with length 0
        im <<- NULL 
    }
    # 2.  get the value of the matrix
    # returns the cashed matrix from cashed x
    get <- function() x 
    
    # 3.  set the value of the inverse of a matrix
    # cashes the inverse of a matrix
    setim <- function(imatrix) im <<- imatrix 
    # 4.  get the value of the inverse of a matrix
    # returns the inverse of a matrix
    getim <- function() im 
    
    # creates a special "vector", which is
    # really a list containing a function to
    list(set = set, # 1.  set the value of the matrix
         get = get, # 2.  get the value of the matrix
         setim = setim, # 3.  set the value of the inverse of a matrix
         getim = getim) # 4.  get the value of the inverse of a matrix
}


## Write a short comment describing this function
# My Comments 3 (text taken ftom asignment):
# `cacheSolve`: This function computes the inverse of the special
# "matrix" returned by `makeCacheMatrix` above. If the inverse has
# already been calculated (and the matrix has not changed), then
# `cacheSolve` retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    # im = inverse of a matrix
    # 'im' is assigned with method 'getim()' from 'makeCacheMatrix'  
    im <- x$getim()
    # If the inverse has already been calculated 
    # (and the matrix has not changed), then
    # `cacheSolve` retrieve the inverse from the cache.
    # That is done in this if-statement
    if(!is.null(im)) {
        message("getting cached data")
        return(im)
    }
    # 'data' is assigned with raw matrix
    data <- x$get()
    # 'data' is inverted and stored in in 'im'
    im <- solve(data, ...)
    # 'im' is cashed by setim()' from 'makeCacheMatrix'
    x$setim(im)
    # returns the cashed result, which is the inverse of a matrix
    im
}

### END ###
# Extra Comments:

# My Comments 1B:
# Used as template:
# makeVector <- function(x = numeric()) {
#     m <- NULL
#     set <- function(y) {
#         x <<- y
#         m <<- NULL
#     }
#     get <- function() x
#     setmean <- function(mean) m <<- mean
#     getmean <- function() m
#     list(set = set, get = get,
#          setmean = setmean,
#          getmean = getmean)
# }

# cachemean <- function(x, ...) {
#     m <- x$getmean()
#     if(!is.null(m)) {
#         message("getting cached data")
#         return(m)
#     }
#     data <- x$get()
#     m <- mean(data, ...)
#     x$setmean(m)
#     m
# }
