## This code has two main functions 
## makeCacheMatix() creates a matix which can cache it's inverse 
## cacheSolve computes the inverse if the same is notcomputed 
## else it retrives the value stored i cache

## have written this code comment heavy to be as explicit as possible

## This function creates the matix and handles the cacheing of values 

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL ## in the first call value of inverse is set to NULL
  
  set <- function(y){ ## this function can be called to update value of matrix
    x <<- y  ## value of matrix and inverse is updated  
    i <<- NULL ##'<<-' ensure that the variables in the containing env. are updated
  }
  
  get <- function() { ## retrives the value of x
    x 
  }
  
  setinverse <- function(inverse) { ## updates the inverse
    i <<- inverse  ## '<<-' ensures that value in containing env. is updated
  }
  
  getinverse <- function() { ## retrives the value of inverse
    i 
  }
  
  list ( set=set, ## returns a list with named values
         get=get, ##  where the values are the functions
         setinverse=setinverse,
         getinverse=getinverse)
}


## This function retrives the cached value if it is already calcualted 
## else it calculates the invers and caches it

cacheSolve <- function(x, ...) { ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse() ## assigns the value if inverse after retriving from cache
  if(!is.null(i)){## if i has a value then return the same
    message("Getting cached data")
    return(i) 
  }
  data <- x$get() ## if inverse is not calculated  get the matrix
  i <- solve (data,...) ## calculate inverse of the matrix
  x$setinverse(i) ## update the value of inverse in cache
  i ## display value of inverse
}
