## makeCacheMatrix: This function creates a "matrix" object that can cache its inverse. It's a list containing functions to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set (cache) the inverse of the inverse matrix
## 4. get the cached value of the inverse matrix


makeCacheMatrix <- function(x = matrix()) {
    
    matrix_inverse <- NULL                                ## erases any cached inverse Matrix 
    set_matrix <- function(new_matrix) {                  ## function saves a new_matrix we may want to insert into matrix obj 
        x <<- new_matrix                                ## copies the new_matrix into variable x in parent scope 
        matrix_inverse <<- NULL                         ## erases old cached matrix_inverse from previous matrix in parent scope 
    } 
    get_matrix <- function() x                            ## method/function returns x stored in matrix obj 
    set_matrix_inverse <- function(new_matrix_inverse) matrix_inverse <<- new_matrix_inverse          ##stores incoming new inverse matrix, calculated elsewhere 
    get_matrix_inverse <- function() matrix_inverse       ## method returns stored matrix_inverse 
    list(set_matrix = set_matrix,                         ## exposed methods for getting access to the matrix and 
         get_matrix = get_matrix,                         ## to the matrix_inverse 
         set_matrix_inverse = set_matrix_inverse , 
         get_matrix_inverse = get_matrix_inverse ) 
    
}


## cacheSolve: This function calculates the inverse of the "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should 
## retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    
    matrix_inverse <- x$get_matrix_inverse ()             ## use method to get matrix_inverse from obj 
    if(!is.null(matrix_inverse)) {                        ## if x already has an inverse matrix return it 
        message("getting cached data")                  ## we are done, no calculation is 
        return(matrix_inverse)                          ## needed. "cached inverse matrix" 
    } 
    data <- x$get_matrix()                                ## otherwise get the matrix from the object 
    matrix_inverse <- solve(data, ...)                    ## calculate the inverse 
    x$set_matrix_inverse(matrix_inverse)                  ## store the inverse in the object matrix_inverse 
    matrix_inverse                                        ## return the inverse to the parent scope 
    
}