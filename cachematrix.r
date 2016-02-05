makeCacheMatrix <- function(x = matrix()) {
  # setting null object for inverse matrix
  m <- NULL	
  
  #set matrix value
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  #get the value
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  #return the list of the precedent value
  list(set=set, get=get,
       setinverse=setinverse, 
       getinverse=getinverse)
}

cacheSolve <- function(x, ...) {
# we check if exist, if yes we get it already cache 

 m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data.")
    return(m)
  }
  # if no, get the matrix
  data <- x$get()
  # inverse
  m <- solve(data,...)
  #cache the matrix
  x$setinverse(m)
  # return result
  m
}
####test matrix#####
#    x = rbind(c(10, 5), c(5, 10))
#    mat_test = makeCacheMatrix(x)
#    mat_test$get()

#    cacheSolve(mat_test)