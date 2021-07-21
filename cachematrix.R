## It saves the inversed matrix values into a waiting memory to call the other function to solve it if it isn't
## already solved

## It asks you for a matrix, then we make null matrix to then fill the voids with and if it can be made it stores the 
## inverse matrix, if not then it just saves the values pending to be inversed by the other function

makeCacheMatrix <- function(x = matrix()) {
  paso <- NULL
  crear <- function(y){
    x <<- y
    paso <<- NULL
    
  }
  llamar <- function() x 
  crearinversa <- function(inversa)
    paso <<- inversa
  llamarinversa <- function() paso
  list(crear = crear, llamar = llamar, crearinversa = crearinversa, llamarinversa = llamarinversa)
}


## This one asks for the cache matrix and it solves it to be the inverse one if it isn't already solved, if it is then 
## it just prints it, if not then it solves it then prints it

cacheSolve <- function(x, ...) {
  paso <-  x$llamarinversa()
  
  if(!is.null(paso)){
    message("getting cache data")
    return(paso)
  }
  datos <- x$llamar()
  paso <- solve(datos, ...)
  x$crearinversa(paso)
  paso
}
