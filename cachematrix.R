## The first function (makeCacheMatrix) takes an invertible (assumed) square 
## matrix and returns a list of functions that 1) sets the matrix in local 
## env. 2) gets the matrix, 3) sets the inverse matrix and 4) gets the inverse
## matrix

## Ths returned list of functions is the input of the second function which
## used this functions to fetch the cached inverse matrix if available and 
## calculate inverse if it has not been done already

## Function 1 : described above
makeCacheMatrix <- function(x = matrix()) {

    m <- NULL
    
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    
    get <- function() x
    
    setmatrix <- function(solve) m <<- solve
    
    getmatrix <- function() m
    
    list(set = set, get = get, setmatrix = setmatrix, getmatrix = getmatrix)
    
}


## Function 2 : described above
cacheSolve <- function(x=matrix(), ...) {
        
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getmatrix()
    
    if (!is.null(inv)){
        message("Getting cached matrix/inv")
        return(inv)
    }
    
    inv.data <- x$get()
    inv <- solve(inv.data, ...)
    
    x$setmatrix(inv)
    
    return (inv)
    
}
