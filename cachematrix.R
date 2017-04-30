##The first function `makeCacheMatrix` stores a matrix and its inverse into variables  
##and returns those values whenever needed to the outside world. However, the access to
## those variables is only allowed through the functions defined within it.

## The second function actually computes the inverse of the matrix stored in the private 
## variable of the function `makeCacheMatrix` and store that inverse into another private
## variable of `makeCacheMatrix` but only for the first time.
## Since then for any subsequent requests of the inverse of the same matrix, it will fetch the 
## pre computed value from `makeCacheMatrix`'s private variable.

## `makeCacheMatrix` is bearing quite similar concept to `class` definition in object
## oriented programming where private variables are only allowed to be accessed by
## class methods.

##`makeCacheMatrix` is having two variables - mtrx and minv - and a set of four functions -
##set,get,setinverse and getinverse - to do the set and get operation on them.

makeCacheMatrix <- function(mtrx = matrix()){
  
  
  ## mtrx and minv, these two are the private variables of the function `makeCacheMatrix`
  ## and these can only be accessed via any of the functions listed below.
  
  ## For the first call to `makeCacheMatrix`, mtrx will have the matrix given to `makeCacheMatrix` as a 
  ##parameter and minv will be set to NULL as inverse of the matrix has not been computed yet.
  
  ## minv is a variable to store the inverse of the matrix mtrx.
  minv <- NULL
  ## This function is required to set the mtrx to a matrix other than the one passed on
  ## through the parameter of the function makeCacheMatrix.
  set <- function(y){
    
    ##Whenever new matrix comes in, precomputed inverse of the other matrix should be set to NULL    
    mtrx <<- y
    minv <<- NULL
  }
  
  ## This function returns the matrix stored inside the variable mtrx.
  get <- function() mtrx
  
  ## The following function sets the minv with the inverse matrix received as a parameter.  
  setinv <- function(inv) minv <<- inv
  
  ##This is the function to return the inverse matrix stored in minv.
  getinv <- function() minv
  
  ## makeCacheMatrix functions returns a list of the functions designed above to the 
  ## outside world through which they can manipulate mtrx and minv.
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## cacheSolve, when it is called through a reference to the makeCacheMatrix function, 
## it checks for the inverse already available there for the respective matrix. If it is
## found there cacheSolve retrieves that value, otherwise, compute it and stores into 
## makeCacheMatrix's variable for future reference.

cacheSolve <- function(x, ...) {
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
}
