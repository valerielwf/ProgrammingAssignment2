## This overall code first does set a matrix and its inverse matrix, and cache these in a outlist.
## Second it checks a new input matrix whether it has inverse matrix, if not then calculate inverse matrix and cache it.

## makeCacheMatrix function inputs a matrix. It outputs a list of 4 subfunctions of this matrix.
## subfunctions includes: set the matrix for x, get the matrix for x, set inverse matrix of x, get inverse matrix of x.
makeCacheMatrix<-function(x){
  c<-NULL
  set <-function(y) {
    x<<-y
    c<<- NULL
  }
  get=function() x 
  setinverse_matrix<- function(x){
    b<-solve(x)
    c<<- b
  }
  getinverse_matrix<- function(y) c
  list(set=set,get=get,setinverse_matrix=setinverse_matrix,getinverse_matrix=getinverse_matrix)
}

## cacheSolve function is first to check whether the input matrix already has inverse matrix in the output list of makeCacheMatrix.
## If do, print the "getting cached data" message
## If not, the function will calculate,cache its inverse matrix, and return its inverse matrix.

cacheSolve<-function(x){
  c <-x$getinverse_matrix()
  if(!is.null(c)){
    message("getting cached data")
    return(c)
  } 
  matrix <- x$get()
  c <- solve(matrix)
  x$setinverse_matrix(matrix)
  c   ## Return a matrix that is the inverse of 'x'
}     
