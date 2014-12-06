makeCacheMatrix <- function(x = matrix())
{
  mat<-NULL
  
  set<-function(y)
  {
    x<<-y
    mat<<-NULL
  }
  
  get<-function() x
  
  set_matrix<-function(solve) mat<<- solve
  
  get_matrix<-function() mat
  
  list(set=set, get=get,  set_matrix=set_matrix, get_matrix=get_matrix)
}

cacheSolve <- function(x=matrix(), ...) 
{
  mat<-x$get_matrix()
  
  if(!is.null(mat))
  {
    message("Getting cached data")
    return(mat)
  }
  
  matrix<-x$get()
  
  mat<-solve(matrix, ...)
  
  x$set_matrix(mat)
  
  mat
}
