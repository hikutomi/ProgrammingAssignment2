## This R programming assignment exemplifies advantage of lexical scoping and
## the fact that functions that return objects of type list() also let one
## access to any other objects defined in the environment of the original
## function. The programm consists of two functions, makeCacheMatrix() and
## cacheSolve(), each explained hereinafter.

## makeCacheMatrix: When makeCacheMatrix() function is called, it keeps the
## original matrix x and initializes its inverted matrix i as NULL in the
## parent environment using assignment operator "<<-".
## Then, it defines four functions ("getters" and "setters") by taking
## advantage of lexical scoping. For instance, since, in the "getters",
## x and i are not defined in each function's environment, the values are
## obtained from parent environment (lexical scoping), while in "setters",
## x and i are set in the parent environment by using "<<-" operator.
## The function returns the list type object with each element having "names", 
## so that $ operator can be used to easily extract the behaviors of the object.

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) i <<- solve
        getsolve <- function() i
        list(set = set, get = get,
                setsolve = setsolve,
                getsolve = getsolve)
}


## cacheSolve: When cacheSolve() function is called, if there is already a
## inverted matrix i in the parent environment, it retrieves already stored
## matrix i using getter functoin defined in the makeCacheMatrix() function with
## the message "getting cached data" (saving the re-calculating time). 
## Otherwise, it uses the getters and setters function of the object created 
## by makeCacheMatrix() function, and calculate the inverted matrix and 
## return it. The solve() function is used to calculate the inverted matrix, 
## assuming the original matrix can be invertible.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getsolve()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setsolve(i)
        i
}
