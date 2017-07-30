## This pair of functions caches the inverse of a matrix to speed up repeat computations

## makeCacheMatrix creates a matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) { #Create empty vector x
  m <- NULL #Create m as null. This is where the cached matrix will be stored
  set <- function(y) {
    x <<- y #Assigns value y to x in the parent environment
    m <<- NULL #Assigns NULL to m in the parent environment (i.e. wipes any existing cached values for m)
  }
  get <- function () x #Defines behaviour of get function. Retrieves x
  setinverse <- function(solve) m <<- solve #Defines setinverse function. Calculates inverse of m
  getinverse <- function () m # Defines getinverse function. Retrieves m
  list(set=set, get = get,
       setinverse = setinverse,
       getinverse = getinverse) #Assigns each function as an element within a list, returning it to the parent environment.
}


## This function calculates the inverse of the matrix created with the above function.
## It checks first if the inverse has already been calculated and returns that from cache if so.
## If the inverse does not already exist, it calculates it and stores it in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
        m <- x$getinverse() #Attempts to retrieve the inverse of the matrix passed as the argument
        if(!is.null(m)) {
          message("getting cached data")
          return(m) #If an inverse already exists, it will return this.
        }
        data <- x$get() #If there is no inverse in the cache, retrieve the matrix from the input
        m <- solve(data, ...) #Compute the inverse of the inputted matrix and assign this to m
        x$setinverse(m) #Store the inverse of the inputted matrix in the cache
        m #Print the inverse (either what's already stored in cache, or the newly stored version)
}
