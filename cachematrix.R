##This source file contais two functions that create a cache of the results of the solve function
##see each function to know details of each one.
##Example of use:
##>  m1 <- matrix(c(1/2, -1/4, -1, 3/4), nrow = 2, ncol = 2)
##Takes the original m1 matrix and creates a new one matrix in the makeCacheMatrix way
##>  t <- makeCacheMatrix(m1)
##Calls cacheSolve to see if exists a previous solve calculated ou to calculate a new one, shows
##the inverted matrix of m1
##This cache behaviour makes the computation more fast.
##> cacheSolve(t)
##Calls againg cacheSolve shows the use of the cached version previously calculated
##> cacheSolve(t)


##This functions takes a matrix and creates a kind of object type structure that stores the original 
##matrix received and some functions to calculate, store and get the results of the solve that 
##calculates the inverse of a given matrix. 
makeCacheMatrix <- function(x) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setsolve <- function(solve) m <<- solve
    getsolve <- function() m
    res <- matrix(c(set = set, get = get,setsolve = setsolve,getsolve = getsolve),nrow=2,ncol=2)
    
}



## This fuction receives a makeCacheMatrix object and checks if exist previously any inverse precalculated
## or, in negative case, try to calculate this inverse, making a call to solve native function.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x[[2,2]]()
        if(!is.null(m)) {
            message("getting cached data")
            return(m)
        }
        data <- x[[2,1]]()
        m <- solve(data, ...)
        x[[1,2]](m)
        m
}
