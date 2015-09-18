## This function stores a Matrix and determines it inverse

## Write a short comment describing this function
# makeCacheMatrix has four functions, get, set, getmatrix, setmatrix

makeCacheMatrix <- function(x = matrix()) {
   
    #initialises m, the inverse of matrix x, to null
    m <- NULL
    # when called, set substitutes the vector x with y, and sets the inverse m to null
    set <- function(y){
        x<<-y
        m<<-NULL
    }
    # get returns the vector x that was stored in the main function
    get<-function()x 
    
    #setmatrix stores the inverse of matrix x into the variable m
    setmatrix <- function(solve) m <<- solve
    
    #getmatrix returns the value of what was stored by setmatrix
    getmatrix <- function() m
    
    #list stores the four functions
    list(set=set,get=get,setmatrix=setmatrix,getmatrix=getmatrix)
}


## This bit stores the matrix into a cached variable and calcelates inverses

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    
    # first step is to retrieve the inverse of matrix x, m that was stored
    m <- x$getmatrix()
    # this bit checks that m is not null and returns it
    if(!is.null(m)){
        message("getting cached data")
        return(m)
    }
    # gets matrix stored by makeCacheMatrix
    matrix<-x$get()
    # inverses the matrix and stores it in variable m
    m<-solve(matrix,...)
    # using the setmatrix function, stores output into the inverse
    x$setmatrix(m)
    # prints the inverse
    m
}
