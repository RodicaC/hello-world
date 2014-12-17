## Put comments here that give an overall description of what your
## functions do

# function below create a list of funtions for the "special" matrix : set,get,setsolve,getsolve
makeMatrix <- function(x = matrix()) {
  s <- matrix()
  set <- function(y) {
    x <<- y
    s <<- matrix()
  }
  get <- function() x
  setsolve <- function() s <<- solve(x)
  getsolve <- function() s
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## this function return the inverse of the "special" matrix calculated above; before computing, it first checks to see if the
#inverse has already been computed, the returns the inverse from cache

cacheSolve <- function(x) {
  s<-x$getsolve() 
  if(!is.na(s) & length(x$get())==1) {
    message("getting cached data")
    return(s)
  }
  data <- x$get()
  s <- solve(data)
  x$setsolve()
  s
}

#test
x<-matrix()
length(x)==1
x<-cbind(c(1,3),c(2,5))
x
y<-solve(x)
mat<-makeMatrix(x)
mat$get()
mat$setsolve()
mat$getsolve()
mat$set(x)
cacheSolve(mat)
