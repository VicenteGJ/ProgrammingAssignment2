## makeCacheMatrix function
        ## Input: a square matrix. It should be numeric. In this part, the
        ##      matrix is stored in a variable called x
        ## Output: a list with 4 elements (set, get, setinv, getinv). They are
        ##       functions
        ##      set: set the values on the matrix 
        ##      get: get the values from the matrix
        ##      setinv: set the values of the inverse matrix. Compute the
        ##              inverse of the matrix using solve
        ##      getinv: get the values of the inverse matrix
makeCacheMatrix <- function(x = matrix()) {
        
        
        inv_matrix <- NULL
        
        ## set the values on the matrix 
        set <- function(y) {
                x <<- y
                inv_matrix <<- NULL
        }
        
        ## get the values from the matrix
        get <- function() x
        
        ## Compute the inverse of the matrix using solve
        setinv <- function(solve) inv_matrix <<- solve
        
        ##get the values of the inverse matrix
        getinv <- function() inv_matrix
        
        ##The four functions are stored in a list
        list(set = set, get = get, setinv = setinv, getinv = getinv)
        
}


## cacheSolve function that use a variable that is a list obtained 
##              from makeCacheMatrix
## Input: a list
## Output: a n x n matrix that is the inverse of X

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        ## Obtain the values of the inverse matrix
        inv_matrix <- x$getinv()
        
        ##In this part we evaluate if there exists the values of the inverse matrix
        ##if is true then a message is printed and the inverse matrix is returned
        ## This allows that solve() is not called again
        ##if is the first time that cacheSolve is called then the lines that 
        ## are into the if will not be executed
        if (!is.null(inv_matrix)){
                message("getting cached data")
                return(inv_matrix)
        }
        
        
        ##if the inverse matrix was not computed then the values of the matrix
        ## are stored in the variable called data
        data <- x$get()
        
        ## Using the solve function and data as input the inverse is computed.
        ## The result is stored in the variable called inv_matrix
        inv_matrix <- solve(data)
        
        ##The inverse matrix is stored in X calling setinv. This is useful when
        ## cacheSolve is called again using the same matrix 
        x$setinv(inv_matrix)
        inv_matrix
}
