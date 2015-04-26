### The second programming assignment for R programming on Coursera
### asks to exploit the benefits of lexical scoping in R by demonstrating
### that caching the result of a compute-intensive program, particularly
### when the data is not changing, makes sense and allows to save time
### and energy. As an example, the following two functions are used to
### create a special matrix, basically a list of four functions, and
### to cache the inverse (time comsuming operation) for future usage
### and return it if the matrix is unchanged.

# makeCacheMatrix is a function to create a special "matrix" object
# that can cache its most-recently computed inverse. It returns a
# list containing four functions which perform following operations
# on the matrix. 1) set the value, 2) get the value, 3) set the
# inverse, and 4) get the inverse.

makeCacheMatrix <- function(x = matrix()) {
  minv <- NULL # Initially inverse is null for any new matrix.
  set <- function(y) { # Replace the current matrix x by y.
    x <<- y
    minv <<- NULL # Accordingly inverse of y should be null.
  }
  get <- function() x # Simply retrieve the current matrix.
  setinv <- function(inv) minv <<- inv # Set inverse of a new matrix.
  getinv <- function() minv # Simply retrieve the inverse, either cached or null.
  list(set = set, get = get, setinv = setinv, getinv = getinv) # Return list containing
  # the four functions.
}


# cacheSolve is a function that returns the inverse of the matrix
# either by retrieving the already computed inverse from the cache
# or by computing it with the solve function.

cacheSolve <- function(x,...) { # x is a list containing four functions.
  minv <- x$getinv() # Simply retrieve the inverse from the above function.
  if(!is.null(minv)) { # Assuming inverse is already computed retrieve it
    message("getting cached data") # cached one by checking whether it is null.
    return(minv)
  }
  data <- x$get() # Inverse is not computed yet so retrieve the matrix.
  minv <- solve(data) # Compute the inverse of the matrix.
  x$setinv(minv) # Cache the newly computed inverse.
  minv # Return the inverse, either cached or computed.
}

# Sample run for an example. Product of a matrix and its inverse returns an identity matrix.
# mat <- makeCacheMatrix(matrix(c(1,2,3,4),2,2)) # Send matrix as an argument.
# mat$get() # Produces the above matrix.
# [,1] [,2]
# [1,]    1    3
# [2,]    2    4
# mat$getinv() # Initially null is returned.
# NULL
# > cacheSolve(mat) # Compute the inverse.
# [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5
# > mat$get() %*% mat$getinv() # Product of a matrix and its inverse produces an identity matrix.
# getting cached data
# [,1] [,2]
# [1,]    1    0
# [2,]    0    1
# > mat$set(matrix(c(4,3,2,1),2,2)) # Replace the current matrix by a new one.
# > mat$get()
# [,1] [,2]
# [1,]    4    2
# [2,]    3    1