## Put comments here that give an overall description of what your
## functions do

## This function creates a special matrix that cache the inverse matrix

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


## This function retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
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
