## R Programming Peer Graded Assignment
## Krista Johnson


## Overall, my functions take a matrix as input, 
## calculates the inverse (if an inverse is cached it is retrieves it),
## returns the inverse matrix

## This function will take a matrix as the input, save it to the parent environment,
## retrieve it from the environment, and set it to a variable in a named list

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL #initializing the i variable 
  set <- function(y) {
    x <<- y     #assigning the set value to the x variable in the parent environment
    i <<- NULL  #clearing any previously cached inverse values
  }
  get <- function() x   #gets the original matrix
  setinverse <- function(inverse) i <<- inverse #sets the inverse matrix (will be NULL until cached)
  getinverse <- function() i   #gets the inverse matrix
  list(set = set, get = get,   #creates a list with named values
       setinverse = setinverse,
       getinverse = getinverse)
  
}


## This function will retrieve the inverse matrix from the environment, if there isn't one cached,
## an inverse matrix will be calculated using the solve() function, and then set as the inverse matrix

cacheSolve <- function(x, ...) {
  i <- x$getinverse() #retrieving the inverse matrix and assigning it to the i variable
  if(!is.null(i)) {   #if a value is cached, retrieve that instead of computing the inverse
    message("getting cached data")
    return(i)
  }
  data_to_work <- x$get() #retrieve the original matrix
  i <- solve(data_to_work)   #use the solve function to compute the inverse, assign to i variable
  x$setinverse(i)   #set the inverse matrix
  i
}
