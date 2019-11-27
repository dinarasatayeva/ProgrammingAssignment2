## The two functions below create a special object that stores a numeric matrix 
##  and caches its inverse

## The makeCacheMatrix function creates a special matrix, that contains a function to:
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse
## 4. get the value of the inverse
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y){
    x<<-y
    i<<-NULL
  }
  get<-function(){
    x
  }
  setinverse<-function(inverse){
    i<<-inverse
  }
  getinverse<-function(){
    i
  }
  list(set = set, get = get,setinverse = setinverse, getinverse = getinverse)
}


## cacheSolve function calculates the inverse of the special matrix created earlier
## It first checks  if the inverse has already been calculated.
## If yes, it gets the inverse from the cache and skips the computation. 
## Otherwise, it calculates the inverse 
##  and sets the value of the inverse in the cache.

cacheSolve <- function(x, ...) {
   i<-x$getinverse()
   if(!is.null(i)){
     message("getting cached data")
     return(i)
   }
   data <- x$get()
   i<-solve(data, ...)
   x$setinverse(i)
   i
}
