## Put comments here that give an overall description of what your
## functions do

# below function makeCacheMatrix creates the list to set the matrix, get the matrix
# also it is used to set the inverse of the matrix and get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setmean <- function(mean) m <<- mean
  getmean <- function() m
  list(set = set, get = get,
       setmean = setmean,
       getmean = getmean)

}

##Results

# > x <- matrix(c(1,4,4,1))
# > x
# [,1]
# [1,]    1
# [2,]    4
# [3,]    4
# [4,]    1
# > x <- matrix(c(1,4,4,1),nrow = 2,ncol = 2)
# > x
# [,1] [,2]
# [1,]    1    4
# [2,]    4    1
# > mcm <- makeCacheMatrix(x)
# > mcm
# $set
# function (y) 
# {
#   x <<- y
#   m <<- NULL
# }
# <environment: 0x0000000004a142a0>
#   
#   $get
# function () 
#   x
# <environment: 0x0000000004a142a0>
#   
#   $setmean
# function (mean) 
#   m <<- mean
#   <environment: 0x0000000004a142a0>
#     
#     $getmean
#   function () 
#     m
#   <environment: 0x0000000004a142a0>
#     
#     > mcm$get()
#   [,1] [,2]
#   [1,]    1    4
#   [2,]    4    1
#   > mcm$getmean()
#   NULL
#   > cacheSolve <- function(x, ...) {
#     +   m <- x$getmean()
#     +   if(!is.null(m)) {
#       +     message("getting cached data")
#       +     return(m)
#       +   }
#     +   data <- x$get()
#     +   m <- mean(data, ...)
#     +   x$setmean(m)
#     +   m
#     + }


# It returns the inverse of the matrix, but before return, it will check if the matrix is already cached
# or it has to be freshly created and set to the cache. This will compute the inverse of the matrix and 
# set to the cache. or if already present it will get the inverse of the matrix

cacheSolve <- function(x, ...) {
  m <- x$getmean()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- mean(data, ...)
  x$setmean(m)
  m
}

Results
# > cacheSolve(mcm)
# [1] 2.5
# > cacheSolve(mcm)
# getting cached data
# [1] 2.5
