makeMatrixVector<-function(x = matrix()){
  inverseMatrix <-NULL
  set <-function(y){
    x <<-y
    inverse <<-NULL
  }
  get<-function()x
  setinverse<-function(inverse) inverseMatrix<-inverse
  getinverse<-function()inverseMatrix
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}  

cacheMatrix<-function(x,...){
  inverseMatrix<-x$getinverse()
  if(!is.null(inverseMatrix)){
  message("getting cached data")
  return(inverseMatrix)
}
  data<-x$get()
  inverseMatrix<-solve(data,...)
  x$setinverse(inverseMatrix)
  inverseMatrix
}

## test1 function
x=rbind(c(8, -1/8), c(-1/8, 8))
m=makeMatrixVector(x)
m$get()## this function get the matrix
cacheMatrix(m)## this run cache the inverse matrix
## test2 function
x <- makeMatrixVector(matrix(1:4, 2, 2))## 
x$get()
cacheMatrix(x)
##Another matrix and cache the inverse
B =makeMatrixVector( matrix( c(1,2,3,4), nrow=2,  ncol=2) )
B$get()
cacheMatrix(B)
