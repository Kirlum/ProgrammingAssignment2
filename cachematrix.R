# The first function, makeCacheMatrix creates a special "matrix".
# set the value of the matrix   #'set'
# get the value of the matrix   #'get'
# set the value of the inverse of the matrix  #'setmatrix'
# get the value of the inverse of the matrix  #'getmatrix'
# finaly it creates a list with all these values  #'list'

makeCacheMatrix <- function(x = matrix()) {
    m<-NULL
    set<-function(y){
        x<<-y
        m<<-NULL
    }
    get<-function() x
    setmatrix<-function(solve) m<<- solve
    getmatrix<-function() m
    list(set=set, get=get,
         setmatrix=setmatrix,
         getmatrix=getmatrix)
}

# cacheSolve: This function computes the inverse of the special "matrix" returned by 
# makeCacheMatrix above. If the inverse has already been calculated (and the matrix has 
# not been changed), then the cachesolve retrieves the inverse from the cache and prints the 
# message: "getting cached data".

cacheSolve <- function(x=matrix(), ...) {
    m<-x$getmatrix()
    if(!is.null(m)){
        message("getting cached data")
        return(m)
    }
    matrix<-x$get()
    m<-solve(matrix, ...)
    x$setmatrix(m)
    m
}


# Example
# > x <- matrix(rnorm(16),4,4)
# > m = makeCacheMatrix(x)
# > m$get()
# [,1]        [,2]       [,3]       [,4]
# [1,] -2.2229792  0.03822971  0.6679352 -0.8261557
# [2,] -0.5668809  0.34328702  0.7716302 -0.7939099
# [3,] -1.2770956  2.09212895  0.4311810 -0.3923326
# [4,] -0.3471504 -1.31476899 -0.3807793 -2.5440555
# > cacheSolve(m)
# [,1]        [,2]        [,3]        [,4]
# [1,] -0.55298660  0.52471244 -0.06020795  0.02511745
# [2,] -0.28565024 -0.02155345  0.50073198  0.02226743
# [3,] -0.04302317  1.41146229 -0.45476577 -0.35636430
# [4,]  0.22952170 -0.27172047 -0.18249613 -0.35466990
# > cacheSolve(m)
# getting cached data
# [,1]        [,2]        [,3]        [,4]
# [1,] -0.55298660  0.52471244 -0.06020795  0.02511745
# [2,] -0.28565024 -0.02155345  0.50073198  0.02226743
# [3,] -0.04302317  1.41146229 -0.45476577 -0.35636430
# [4,]  0.22952170 -0.27172047 -0.18249613 -0.35466990

# > round(cacheSolve(m) %*% m$get(), 3)  # This is to controle if A * Ai = I is correct.
# [,1] [,2] [,3] [,4]
# [1,]    1    0    0    0
# [2,]    0    1    0    0
# [3,]    0    0    1    0
# [4,]    0    0    0    1