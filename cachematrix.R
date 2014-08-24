## Aug 23th, 2014 - Course R Programming (https://class.coursera.org/rprog-006)
## Assignment2 (on a MacOS X v.10.9.4 - R v.3.1.1): Matrix inversion by taking some benefit of caching the inverse of a matrix rather than compute it repeatedly
## Based on Roger Peng functions 'makeVector()' & 'cachamean()'

## makeCacheMatrix(): This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  ## Set the value of the vector
  matx<-NULL
  set<-function(y){
	  x<<-y
	  matx<<-NULL
}

## Get the value of the vector
get<-function() x
setmatrix<-function(solve) matx<<- solve
getmatrix<-function() matx
list(set=set, get=get, setmatrix=setmatrix, getmatrix=getmatrix)
}

#cacheSolve(): This function computes the inverse of the special "matrix" returned by makeCacheMatrix() above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x=matrix(), ...) {
    ## Computes the inverse of the special "matrix" (solve it) returned by 
    ## makeCacheMatrix above
    matx<-x$getmatrix()
    if(!is.null(matx)){
	      message("getting cached data")
	      return(matx)
    }
    matrix<-x$get()
    matx<-solve(matrix, ...)
    x$setmatrix(matx)
    
    ## Retrieve the inverse from the cache
    matx
}

#*********************************************************************
#Testing the functions above
#*********************************************************************
#This is supposed to work (mtx1 is square and invertible)
#mtx1<-matrix(1:4,2,2)
#mtx1
#det(mtx1)
#test1<-makeCacheMatrix()
#(test1$set(mtx1))
#cacheSolve(test1)

#library(MASS)
#ginv(mtx1)
#--------------------------------------------------------------------

#This is supposed to work (mtx2 is square and invertible)
#mtx2<-cbind(c(1,1,4), c(0,3,1), c(4,4,0))
#mtx2
#det(mtx2)
#test2<-makeCacheMatrix()
#(test2$set(mtx2))
#cacheSolve(test2)

#library(MASS)
#ginv(mtx2)
#--------------------------------------------------------------------

#Not supposed to work - mtx2 is not square (no determinant can be calculed)
#mtx3 <- matrix(1:6, 2)
#mtx3
#det(mtx3)
#test3<-makeCacheMatrix()
#(test3$set(mtx3))
#cacheSolve(test3)

#library(MASS)
#ginv(mtx3)
#--------------------------------------------------------------------

#Not supposed to work - mtx3 is not invertible - determinant = 0
#mtx4 <- matrix(1:9,3,3)
#mtx4
#det(mtx4)
#test4<-makeCacheMatrix()
#(test4$set(mtx4))
#cacheSolve(test4)

#library(MASS)
#ginv(mtx4)
#--------------------------------------------------------------------

#Not supposed to work - mtx4 is not invertible - determinant = 0
#mtx5 <- matrix(1:16,4,4)
#mtx5
#det(mtx5)
#test5<-makeCacheMatrix()
#(test5$set(mtx5))
#cacheSolve(test5)

#library(MASS)
#ginv(mtx5)
#--------------------------------------------------------------------
### END
