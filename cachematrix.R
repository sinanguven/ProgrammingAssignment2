## There are two functions in this file. The first one handles the basics 
## for a Matrix. The second one caches the inverse of a matrix if it's not
##already done before

## MakeCacheMatrix contains the set and get for a matrix and the inverse
## of a matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
    
  get <- function() x
  
  setinversematrix <- function(matrix) m <<- matrix

  getinversematrix <- function() m
  
  list(set = set, get = get,
       setinversematrix = setinversematrix,
       getinversematrix = getinversematrix)
}

## cacheSolve returns the inverse of a matrix if the inverse was calculated
## and cached earlier. If not, it calculates the inverse and caches it

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinversematrix()
  if (!is.null(m)) {
    message("getting cached inverse matrix")
    return(m)
  }
  data <- x$get()
  m <- solve(data)
  x.setinversematrix(m)
  m  
}
