## Cache Matrix
## File include functions and test data for the second programming assignment of
## the Cousera R Programming Course. 
## 
## File include the following sections
##   1. Predefined matrices for use to the test the include functions
##   2. makeCacheMatrix provides a function that instantiates a cacheMatrix
##   3. cacheSolve computes the inverst of cacheMatrixc
##   4. test examples 
## 
## Created 12-2020
#

## Section 1: test variables

# Test Matricies
m1 <- matrix(c(-1,1,2.5, -1), nrow=2, ncol=2)
m2 <- matrix(c(1,2,3,1,3,8,1,2,2), nrow=3, ncol=3)
m3 <- matrix(c(1,-5,0,0,1,0,0,0,1), nrow=3, ncol=3)

# Inverted Matrices
m1_inv <- solve(m1)
m2_inv <- solve(m2)
m3_inv <- solve(m3)


## Section 2: makeCache

# makeCache Matrix
# Function creates a matrix object that facilitates use of cacheSolve to compute
# the matrix inverse. 
#
# Input 
#    x is a matrix
#
# Returns a list that include: 
#    setMatrix()
#    getMatrix()
#    setInverse()
#    getInverse()
# 
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  setMatrix <- function(y) {
    x <<- y
    m <<- NULL
  }
  getMatrix <- function() x
  setInverse <- function(solve) m <<- solve
  getInverse <- function() m
  list(set = setMatrix, getMatrix = getMatrix,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Section 3: cacheSolve

# cacheSolve
# Function returned the cached inverse of a matrix if it exist. The 
# the matrix inverse. 
#
# Input 
#    x is a matrix
#
# Returns a list that include: 
#    set()
#    get()
#    setMatrix()
#    getMatrix()
# 
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getInverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$getMatrix()
  m <- solve(data, ...)
  x$setInverse(m)
  m
}


# Section 4: test examples
# 
# Test 1: validate inverse
a<-makeCacheMatrix(m1)
a$getMatrix()
a$getInverse()
b<-cacheSolve(a)
a$getInverse()
identical(b, m1_inv)

# Test 2: 
c<-makeCacheMatrix(m2)
c$getMatrix()
c$getInverse()
d<-cacheSolve(c)
c$getInverse()
identical(d, m2_inv)


# Test 3
e<-makeCacheMatrix(m3)
e$getMatrix()
e$getInverse()
f<-cacheSolve(e)
e$getInverse()
identical(f, m3_inv)