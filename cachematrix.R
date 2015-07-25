## The following functions take an invertible matrix as input and return the 
# inverse of the matrix. They also provide the means to cache the inverse
# and thus save computational power. 

## makeCacheMatrix creates a list of functions that
#	1. Set the value of the matrix
#	2. get the value of the matrix
#	3. set the value of the inverted matrix
#	4. get the value of the inverted matrix

makeCacheMatrix <- function(x = matrix()) {

        m <- NULL
        set <- function(y) {	#Change the matrix x, stored in the main function
                x <<- y
                m <<- NULL	# New vector => resetting m. 
        }
        get <- function() x	# Return the matrix x
        setinv <- function(solve) m <<- solve(x)	#set m as inverse of x
        getinv <- function() m	#Return the inverse of x
        list(set = set, get = get,	#Store the 4 functions created herein. 
             setinv = setinv,
             getinv = getinv)
}


## cacheSolve writes out the inverse of the matrix from makeCacheMatrix. It searches in the global environment to see if the inverse of the matrix has
# already been calculated. If so, it prints the inverse, if not it calculates
# the inverse and then prints it out.   

cacheSolve <- function(x, ...) {

	m <- x$getinv()		# Set m as the inverse of x
        if(!is.null(m)) {	# if m has been cached, return m
                message("getting cached data")
                return(m)
        }
        data <- x$get()		# Set data as the matrix x from prev function
        m <- solve(data, ...)	# Calculate the inverse of x and store in m
        x$setinv(m)		
        m
 
}
