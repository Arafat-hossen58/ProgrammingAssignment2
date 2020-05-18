#specify the matrix in function as makecachematrix
makeCacheMatrix<-function(x=matrix()){
  m<-NULL
  set<-function(y){
    x<<-y
    m<<-NULL
  } #creating inverse matrix
  get<-function()x
  setinverse<-function(inverse)m<<-inverse
  getinverse<-function()m
  list(set=set,
       get=get,
       setinverse=setinverse,
       getinverse=getinverse)
  
} #calculate the matrix under this function
cacheSolve<-function(x,...){
  m<-x$getinverse()
  if(!is.null(m)){
    meassage("getting cached data")
    return(m)
  }
  data<-x$get()
  m<-solve(data, ...)
  x$setinverse(m)
  m
}
