## [20140822] JL This function is to cache the inverse of a matrix

##The first function, makeCacheMatrix creates a special "matrix", which is really a list containing a function to
##1.set the value of the matrix
##2.get the value of the matrix
##3.set the value of the inverse
##4.get the value of the inverse

makeCacheMatrix <- function(x = matrix()) 
  {
  
  ## Define the i as the inverse property
  i<- Null
  
  set <- function(matrix)
    {
        x <<-matrix
        i <<- Null
    }
  
  get <-function()
    {x}
  
  setInverse <-function(inverse)
    { i <<- inverse}
  
  getInverse <-function()
    {i}
  ##[20140822] JL return the list of functions
  list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
  
  }
  




## Write a short comment describing this function
##The following function calculates the inverse of the matrix created with the above function. 
##However, it first checks to see if the inverse value has already been calculated.
##If so, it gets the inverse value from the cache and skips the computation.
##Otherwise, it calculates the inverse of the data and sets the value of the Inverse in the cache via the setInverse function.

cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of 'x'
 i <- x$getinverse()
 if (!is.null(i))
 {message ("getting cached data...")
 return(i)
 }
 data <- x$get()
 i <-solve(data)%*%data ## matrix multiplication
 x$setInverse(i)
 i
  
}
