## [Put comments here that describe what your functions do]
## funtion do

##there are two functions makeCacheMatrix,makeCacheMatrix
##makeCacheMatrix consists of set,get,setinv, getinv
##libary(MASS) is used to calculate inverse for non squared as well as square matrices
library(MASS)
makeCacheMatrix <- function(x = matrix()) {
  inv<- NULL     #initializing inverse as NULL
  set<-function(y){
    x<<-y
    inv<<-NULL
  }
  get<-function()x
  setinv<-function(inverse)inv<<-inverse
  getinv<-function(){
    inver<-ginv(x)
    inver%*%x     #function to obtain inverse of the matrix
  }
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


##write a short comment describeing this function
##this is use to get the cache data

cacheSolve <- function(x, ...)##gets cache data
{
  inv<-x$getinv()
  if(!is.null(inv)){           #checking whether inverse is NUll
    message("getting cached data")
    return(inv)
  }
  data<-x$get()
  inv<-solve(data,...)    #calculates inverse Value
  x$setinv(inv)
  inv    ## Return a matrix that is the inverse of 'x'
}

