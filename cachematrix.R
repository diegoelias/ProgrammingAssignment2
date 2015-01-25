## The file "cachematrix.R" contains two functions to perform caching a inverse matrix. 
## The "cachematrix.R" has the functions "makeCacheMatrix" and "cacheSolve"

## This function (makeCacheMatrix) creates a special matrix that cache the inverse matrix
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

## This function (cacheSolve) return a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {
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
