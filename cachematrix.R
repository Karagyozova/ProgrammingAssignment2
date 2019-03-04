## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        ##Initialize the inverse property
        inv<-NULL 
        ##Set the matrix,<<- Unlike the usual single arrow assignment (<-) that always assigns in the current environment, 
        ##the double arrow operator will keep looking up the chain of parent environments until it finds a matching name.
        set<-function(y) {
                x<<-y
                inv<<-NUL
        }
        ##Get the matrix
        get<-function()x
        ##Set the inverse of the matrix, solve() calculates the inverse
        setinverse<-function()inv<<-solve(x)
        ##Get the inverse of the matrix
        getinverse<-function()inv
        ##Return a list of the methods
        list(set=set,get=get,
             setinverse=setinverse,
             getinverse=getinverse)
}

## Write a short comment describing this function
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv<-x$geinverse()
        ##Return only the inverse if it is already set
        if(!is.null(inv)){
                message("getting cached data")
                return(inv)
        }
        #get the matrix from the object
        mat<-x$get()
        ##Calculate the inverse using matrix multiplication, solve() calculates the invesre 
        inv<-solve(mat,...)
        ##set the inverse to the object
        x$setinverse(inv)
        #Return the matrix
        inv
}
