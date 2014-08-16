## Put comments here that give an overall description of what your
## functions do
# 
# makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
# Sample results shown below. Expected result from inversing a matrix.
# > A <- matrix(c(4,4,-2,2,6,2,2,8,4),3,3) 
# > solve(A) ## inverse of matrix A using Solve(A). 
# [,1] [,2] [,3]
# [1,]  1.0 -0.5  0.5
# [2,] -4.0  2.5 -3.0
# [3,]  2.5 -1.5  2.0
# The second function, cacheSolve is to test if the inverse matrix is calculated, if not read in the matrix
# inverse the matrix using solve command.
# Sample results of the run


# > b <- makeCacheMatrix(matrix(c(4,4,-2,2,6,2,2,8,4),3,3))
# > b$get()  # the initial matrix A
# [,1] [,2] [,3]
# [1,]    4    2    2
# [2,]    4    6    8
# [3,]   -2    2    4


# > cacheSolve(b) # Pass b into cacheSolve. Correct results. Inverse of A.
# [,1] [,2] [,3]
# [1,]  1.0 -0.5  0.5
# [2,] -4.0  2.5 -3.0
# [3,]  2.5 -1.5  2.0
# > b$getmat() # test if results is stored. Stored.
# [,1] [,2] [,3]
# [1,]  1.0 -0.5  0.5
# [2,] -4.0  2.5 -3.0
# [3,]  2.5 -1.5  2.0
# > 





# makeCacheMatrix:
## Write a short comment describing this function
# This function gets the matrix and can cache its inverse.
# Sample results of the run are shown above.

makeCacheMatrix <- function(x = matrix()) {
        mat <- NULL
        set <- function(y) {
                x <<- y
                mat <<- NULL
        }
        get <- function() x
        setmat <- function(matrix) mat <<- matrix
        getmat <- function() mat
        list(set = set, get = get,
             setmat = setmat,
             getmat = getmat)

}


## Write a short comment describing this function
# The second function, cacheSolve is to test if the inverse matrix is calculated, if not read in the matrix
# inverse the matrix using solve command.
# Sample results of the run are shown above.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        ## Return a matrix that is the inverse of 'x'
        mat <- x$getmat()
        if(!is.null(mat)) {
                message("getting cached data")
                return(mat)
        }
        data <- x$get()
        mat <- solve(data, ...)
        x$setmat(mat)
        mat
}
