## Programming Assigment 2
## R Programming Coursera

## Floribel Gonzalez Chacon

## Create matrix example with aleatory numbers
matriz_ex<-matrix(sample(1:500,100),10,10)

## makeCacheMatrix Function
## x is a matrix, I assigned a default value of matrix 10x10 aleatory
## output: list with 4 elements:set,get,setsolve, getsolve, 

makeCacheMatrix <- function(x = matrix(sample(1:500,100),10,10)) {
  s <- NULL
  set <- function(y) {
    x <<- y
    s <<- NULL
  }
  get <- function() 
    x
  setsolve <- function(solve) 
    s <<- solve
  getsolve <- function() 
    s
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## cacheSolve Function
## the function try to get the inverse matriz
cacheSolve <- function(x, ...) {
  s <- x$getsolve()
  if(!is.null(s)) {
    message("inversed matrix")
    return(s)
  }
  data <- x$get()
  s <- solve(data, ...)
  x$setsolve(s)
  s
}
