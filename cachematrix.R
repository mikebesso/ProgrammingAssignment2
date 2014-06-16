## Per the instructions of this assignment, all code is in a single file.  
##
## If allowed, I would have
##
##   * Put the two main functions in separate files
##   * Put the unit test code in a separate folder, with each function
##     in it's own file       
##
## Per the instructions of the assignment, I wrote the following two functions
## makeCacheMatrix() and cacheRandomSquareMatrix().
##
## In addition to the the instructions, I write some unit tests that
##
##   * Validate the code is doing as intended
##   * Measures the impact of caching on performance


##########
## Assignment 2 - First Function
#########

makeCacheMatrix <- function(initial_value = matrix()) {

    #####################
    ## Caches a matrix and that matrix's inverse to speed up performance
    ## when the same inverted matrix will be needed multiple times.
    ##
    ## Args:
    ##   initial_value:
    ##       The initial value of the matrix to cache, with a default of
    ##       a null matrix.
    ##
    ## Returns:
    ##   A list of functions
    ##       set:
    ##           Sets the cached matrix.  Any previous value
    ##           is overwritten, and any cached inverse forgotten.
    ##       get:
    ##           Gets the currently cached matrix.
    ##       set.inverse:
    ##           Sets the inverted matrix, causing it to be cached.  
    ##           Any previous value is overwritten.
    ##       get.inverse:
    ##           Gets the currently cached inverted matrix. 
    ##
    ## Usage:
    ##   m <- makeCacheMatrix(matrix(rnorm(9), nrow = 3, ncol = 3)
    ##   m$set.inverse(cacheSolve(m))
    ##   :
    ##   :
    ##   interesting.calculation <- 
    ##       (2 * m$get.inverse())  %*% (4 * m$get.inverse())
    #####################
    
    
    ############
    ## Function Definitions
    ############
    
    assert.matrix.square <- function(value){
        
        ################
        ## Verify that we have a square matrix, if not, stop with error
        ################

        ## Verify that we have a matrix
        if (!is.matrix(value)) {
            stop("makeCahceMatrix$set() requires a matrix")
        }
        
        ## Verify that the matrix is square
        if (nrow(value) != ncol(value)) {
            stop("makeCahceMatrix$set() requires a square matrix")
        }   
        
    }
    
    
    set <- function(new_value) {
 
        ################
        ## Sets the cached matrix, overwriting previous cache and clearing the 
        ## inverted cache.
        ##    
        ## Args:
        ##    
        ##   new_value:
        ##       The matrix to cache
        ##    
        ##   returns:
        ##       The newly set value
        ################
        
        
        #Verify that we have a square matrix
        assert.matrix.square(new_value) 
    
        ## If we got this far, then we have a square matrix, all is good.
        matrix2invert <<- new_value
        inverted_matrix <<- NULL

        new_value
    }
    
    
    get <- function() {
        
        ################
        ## Gets the cached matrix
        ##
        ## Returns:
        ##   The previously cached matrix
        ################
    
        ## Return the previously cached matrix
        matrix2invert
    }
    
    set.inverse<-function(new_value) {
    
        ################
        ## Sets the cached inverse matrix, overwriting previous cache.
        ##
        ## Args:
        ##
        ##   new_value:
        ##       The inverted matrix to cache
        ##
        ##   returns:
        ##       The newly set value
        ################
        
        
        #Verify that we have a square matrix
        inverted_matrix <<- new_value
        
        #returns the inverted matrix
        inverted_matrix
    }
    
    
    get.inverse<- function() {
        
        ################
        ## Gets the cached inverted matrix
        ##
        ## Returns:
        ##   The previously cached inverted matrix
        ################
        
        ## Return the previously cached inverted matrix
        inverted_matrix
    }
    
    
    
    ############
    ## Constructor Code
    ############
    
    ## Set the initial value
    set(initial_value)

    
    ## Return the setters and getters
    list(
        set = set, 
        get = get, 
        set.inverse = set.inverse, 
        get.inverse=get.inverse
    )
}



##########
## Assignment 2 - Second Function
#########


cacheSolve <- function(cache){

    #####################
    ## Finds the inverse of a cached matrix, and stores the inverse in the cache.
    ##
    ## Args:
    ##
    ##   cache:
    ##       The cache to use.
    ##
    ## Returns:
    ##
    ##   The inverted matrix
    ##
    ## Usage:
    ##
    ##   m <- makeCacheMatrix(matrix(rnorm(9), nrow = 3, ncol = 3)
    ##   m$set.inverse(cacheSolve(m))
    ##   :
    ##   :
    ##   interesting.calculation <- 
    ##       (2 * m$get.inverse())  %*% (4 * m$get.inverse())
    #####################
    
    
    ## Get the cached value
    inverted_matrix <- cache$get.inverse()
    
    ## If the cached value is null, then it wasn't cached yet, 
    ##   cache it now
    if(is.null(inverted_matrix)) {
        inverted_matrix <- solve(cache$get())
        cache$set.inverse(inverted_matrix)
    }

    
    ## Return the inverse
    inverted_matrix

}




##########
## Unit Tests (and helper functions)
#########


cacheRandomSquareMatrix <- function(size = 2){

    #####################
    ## Create a square matrix
    ##    
    ## Args:
    ##    
    ##   Size:
    ##       Number of columns/rows 
    ##    
    ##   Returns:
    ##       A square matrix with random values
    #####################
    
        
    ## Create and return a square matrix with random values
    matrix(rnorm(size^2), nrow = size, ncol = size)
}



testCache <- function(size){
    
    #####################
    ## Unit tests for the assignment 2 matrix
    ##
    ##   Args:
    ##       size:
    ##           The size (columns/rows) of a matrix to test
    ##
    ##   Returns:
    ##       The elapsed time
    #####################
    
    ## Create the cache 
    cache <- makeCacheMatrix()
    
    ## Create a radom matrix, and set the cache
    uninverted_matrix <- cache$set(cacheRandomSquareMatrix(size))
    
    ## Get the matrix, from the cache
    cache.get.returned <- cache$get()
    
    ## Validate the that cache has a matrix    
    if (!is.matrix(cache.get.returned)){
        stop("Failed cached value is not a matrix")
    }
    
    ## Validate that the cache has the same matrix that we set it to
    if (!identical(uninverted_matrix, cache.get.returned)){
        stop("Failed cached value not same as original")
    }
    
    ## Invert the matrix in the cache (first time) 
    ## while getting the time it took    
    run_first_time <- system.time(
        inverted_matrix_first <- cacheSolve(cache)
    )
    
    ## Invert the matrix in the cache (second time from cached value)
    ## while getting the time it took
    run_second_time <- system.time(
        inverted_matrix_second <- cacheSolve(cache)
    )
    

    ## Validate that the first and second inversions are identical
    if (!identical(inverted_matrix_first, inverted_matrix_second))
    {
        stop("Failed inverses not the same")
    }
    
    
    ## Validate that we really did get an inversion (using a tolerance of .000001)
    if(!identical(round(cache.get.returned %*% inverted_matrix_first, 6), diag(size))){
        stop("Failed:  Original * Inverse <> Identity")
    }
    
    ## Return the elapsed time
    run_first_time["elapsed"] - run_second_time["elapsed"]
}


showMetrics <- function(test.start, test.stop, test.timings){

    #####################
    ## Output test results
    ##
    ## Args:
    ##   test.start:
    ##       Sequence start (int)
    ##   test.stop:
    ##       Sequence stop (int)
    ##   test.timings:
    ##       Vector of timings
    #####################
    

    ## Output the results
    
    message(
        sprintf(
            '\nTesting matrices sized from %i to %i', 
            test.start, 
            test.stop
        )
    )
    
    message(
        sprintf(
            '\tPerformance gain between %f and %f', 
            min(test.timings), 
            max(test.timings)
        )
    )
    
    message(
        sprintf(
            '\twith a mean of %f', 
            mean(test.timings)
        )
    )
}



testRange <- function(test.start, test.stop){
    
    #####################
    ## Test the inverse caching using a sequence of square matrices 
    ## of size test.start through test.stop.
    ##
    ## Args:
    ##   test.start:
    ##       Sequence start (int)
    ##   test.stop:
    ##       Sequence stop (int)
    #####################
    
    
    ## Run the tests and collect the timings
    test.timings <- sapply(seq(test.start, test.stop), testCache)
    
    ## Output the results
    showMetrics(test.start, test.stop, test.timings)
    
}



test <- function(){

    #####################
    ## Run a set of tests
    #####################
    
    testRange(1, 10)
    testRange(11, 100)
    testRange(101, 200)
    testRange(201, 500)
}
    



