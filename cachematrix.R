makeCacheMatrix <-function(x =matrix()) 
{ 
	##  x: a square invertible matrix 
	##  return: a list containing functions to 
	##  a. set the matrix 
	##  b. get the matrix 
	##  c. set the inverse 
	##  d. get the inverse 
	##  And the list used as the input to cacheSolve()
	
	inv =NULL        
	set =function(y) 
	{ 
		# use "<<-" to assign a value to an object in an environment 		                # different from the current environment.                 
		x <<- y                 
		inv <<-NULL        
	}         
	get =function() x         
	setinverse =function(inverse) 
	inv <<- inverse          
	getinverse =function() inv
	list(set=set, get=get, setinverse=setinverse, getinverse=getinverse) 
} 
#####################
cacheSolve <-function(x, ...) 
{ 
	## x: output of makeCacheMatrix() 
	## return: inverse of the original matrix input to makeCacheMatri
	
	inverse = x$getinverse() 
	
	# if the inverse has already been calculated
 
	if (!is.null(inverse))
	{ 
		# get it from the cache and skips the computation. 
		message("getting cached data") 
		return(inverse) 
	} 
	# otherwise, calculates the inverse  
       
	matx.data = x$get()   
      	inverse =solve(matx.data, ...) 

	# sets the value of the inverse in the cache via the setinv function.  
      
	x$setinverse(inverse) 
	return(inverse) 
}