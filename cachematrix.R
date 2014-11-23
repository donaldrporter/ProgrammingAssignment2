#Programming assignment 2 
#Caching the Inverse of a Matrix. 
#Write a pair of functions that calculate and cache the inverse of a matrix.

#Procedure: 
#1)Into makeCacheMatrix(), enter a matrix to be inverted. makeCacheMatrix()
#  will create a variable (named inv) to cache the inverse and return a
#  list containing four functions. Assign this list to a list object,
#  e.g. lname<-makeCacheMatrix(x).

#2)Enter this list into cacheSolve(). cacheSolve(lname) will then calculate and
#  cache the inverse in makeCacheMatrix(x). 

#3)Once the inverse has been cached, use cacheSolve(lname) to retrieve the
#  cached inverse as many times as needed without recalculating repeatedly.

#4)When a new matrix is to be inverted, either create a new list with 
#  makeCacheMatrix() or reset the existing list with lname$set(),
#  a function provided inside makeCacheMatrix() specifically for that purpose. 


# makeCacheMatrix() will create a variable (named inv) to cache the inverse
# and return a list containing four functions.
makeCacheMatrix <- function(x=matrix()) { 
  inv<-NULL                ##initiate empty inv
  set<-function(y) {       ##Used to enter a new matrix, not always used
    x<<-y                  ##assign y to x, note double arrows inside set()
    inv<<-NULL             ##clear previously cached inverse
  }  
  ## The 3 functions below will be called by cacheSolve()
  get<-function() x        ##get matx, the matrix to be inverted
  setinv<-function(inverse) inv<<-inverse  ##store inverse, use double arrow 
  getinv<-function() inv   ##get the cached inverse
  
  list(set = set, get = get,  
       setinv = setinv,       
       getinv = getinv)    ##Put the four functions in a list; return the list.   
}

# If an inverse has not been cached, cacheSolve() will calculate, cache, 
# and return it.
# If an inverse has been previously cached, cacheSolve() will return it.
cacheSolve <- function(x, ...) {
  inv<-x$getinv()                   ## get inv (may or may not be NULL)       
  if(!is.null(inv)) {                   
    message("getting cached data")    
    return(inv)                     ## if inv has a value, return the value                        
  }                                  
  #### if inv is NULL, continue:
  data<-x$get()                     ## get the matrix to be inverted
  i<-solve(data, ...)               ## invert the matrix by using solve()
  x$setinv(i)                       ## store the inverse in inv
  i                                 ## return i
}

