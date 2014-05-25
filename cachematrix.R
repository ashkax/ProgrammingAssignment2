## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    invx <- NULL   #the variable that would hold the cahced inverse
    set <- function(y) {  #the function set, that sets the matrix 
        x <<- y #set the value y, locally to x
        invx <<- NULL
    }
    get <- function() x #return the matrix 
    setinverse <- function(inverse) invx <<- inverse   #set the cached inverse matrix
    getinverse <- function() invx #get the inverse matrix
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse) #return the list that represent makeCacheMatrix
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        ## example:
        ## A=makeCacheMatrix(matrix(1:4,nrow=2))
        ## cacheSolve(A)
    inv <- x$getinverse()  #try to get the cahced inverse, if it exists, print it out and return
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()   #Since the cached inverse doesnt exist by this point, let's create it.
    inv <- solve(data, ...)
    x$setinverse(inv) #set the calculated inverse
    inv    #return the calculated inverse
}
