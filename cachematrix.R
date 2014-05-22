## This function will set/get a matrix object or set/get the matrix's inverse.
## Action is determined by which subfunction is called.  Subfunction
## details are described within.
##
## Output variables:
##
##    "x" will contain the matrix
##    "m" will contain the matrix's inverse

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL                                       # When called with no subfunction specified, return NULL

        set <- function(y) {                            # When called with $set subfunction, put the
                x <<- y                                 # passed value of "y" into parent environment's "x"
                m <<- NULL                              # and put NULL into parent environment's "m",
                                                        # clearing the "cache"
        }

        get <- function() x                             # When called with $get subfunction specified,
                                                        # return (x), the original matrix

        setinverse <- function(incoming) m <<- incoming # When called with $setinverse subfunction,
                                                        # save passed value into parent environment's
                                                        # (m) cache


        getinverse <- function() m                      # When called with $getinverse subfunction,
                                                        # return the cached inverse matrix
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}




## This function computes the inverse of the matrix by makeCacheMatrix
## If the matrix has not changed and the inverse has already been calculated
## then the previously cached result with be returned rather than recalculated.
##
## Output variable:
##
##    "m" will contain the matrix's inverse

cacheSolve <- function(x, ...) {

        m <- x$getinverse()                      # Call subfunction $getinverse and save matrix
                                                 # inverse into local variable "m"
        if(!is.null(m)) {                        # If retrieved cached value was not null,
                message("getting cached data")   # advice that cached data is being used,
                return(m)                        # and leave this function, returning that data
        }

        data <- x$get()                          # If we get this far, there was no cached data
        m <- solve(data, ...)                    # so calculate the matrix inversse and store in
                                                 # local variable "m"

        x$setinverse(m)                          # Call subfunction $setinverse, passing the newly
                                                 # calculated inverse so that it will put in into the cache
        m                                        # Also exit, returning the inverse
}

