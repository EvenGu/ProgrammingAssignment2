## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
        
        MI <- NULL
        set <- function(y) {
                x <<- y
                MI <<- NULL
        }
        get <- function() {x}
        setMI <- function(MatInv) {
                MI <<- MatInv
        }
        getMI <- function() {MI}
        list(set = set, get = get, setMI = setMI, getMI = getMI)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## It checks cache first.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        MI <- x$getMI()
        if(!is.null(MI)) {
                message("getting cached data")
                return(MI)
        }
        # else
        Mat <- x$get()  # get the original matrix
        MI <- solve(Mat, ...)
        x$setMI(MI)
        MI
}
