## Put comments here that give an overall description of what your
## functions do


makeCacheMatrix <- function(x = matrix()){
	m<- NULL # this is where the result of inversion is stored
	set<-function(y){
		x<<-y
		m <<- NULL
	}
      get <- function() x
      setmatrix <- function(solve) m <<- solve
      getmatrix <- function() m # get the inversed matrix from object x
      list(set = set, get = get,
		setmatrix = setmatrix ,
            getmatrix = getmatrix)
}
	
cacheSolve <- function(x, ...) {
        m <- x$getmatrix() # get the inversed matrix from object x
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setmatrix(m)
        m
}
