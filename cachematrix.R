## Programming Assignment 2

## write an R function that is able to cache potentially time-consuming computations.
## Find the inverse of an invertible matrix.
## However, for a very large matrix, it may take too long to
## solve the inverse, especially if it has to be computed repeatedly (e.g.
## in a loop). If the contents of the matrix are not changing, 
## cache the value of the inverse so that when we need it again, it
## can be looked up in the cache rather than recomputed. In this
## Programming Assignment we take advantage of the scoping rules of
## the R language and how they can be manipulated to preserve state inside
## of an R object.

## The first function, `makeCacheMatrix` creates a special "matrix", which is
## really a list containing a function to

## 1.  set the value of the matrix
## 2.  get the value of the matrix
## 3.  set the value of the inverse
## 4.  get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
      x <<- y
      i <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) i <<- inverse
    getInverse <- function() i
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## The following function calculates the inverse of the special "matrix"
## created with the above function. However, it first checks to see if the
## inverse has already been calculated. If so, it `get`s the invers from the
## cache and skips the computation. Otherwise, it calculates the inverse of
## the data and sets the value of the inverse in the cache via the `setInverse`
## function.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    i <- x$getInverse()
    if(!is.null(i)) {
      message("getting Inverse of matrix")
      return(i)
    }
    data <- x$get()
    i <- solve(data) ## solve(data) %*% data ???
    x$setInverse(i)
    i
}
