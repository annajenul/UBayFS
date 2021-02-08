#checks whether a list object implements proper UBayFS user constraints
checkConstraints <- function(x){

  if(is.null(x)){
    return(TRUE)
  }

  if(!is.list(x)){
    return(FALSE)
  }

  A = x$A
  b = x$b
  rho = x$rho

  if(!is.null(A) | !is.null(b) | !is.null(rho)){
    if(nrow(A) != length(b) | length(b) != length(rho)){
      return(FALSE)
    }
  }

  return(TRUE)

}
