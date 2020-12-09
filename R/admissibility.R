#' @importFrom matrixStats logSumExp
#' @export

admissibility <- function(state, A, b, rho, log = TRUE){
  state = state * length(state) # rescale parameter vector theta by length
  z = (b - A %*% state) * rho

  lterm <- z - apply(cbind(z,0), 1, logSumExp)
  lterm <- sum(lterm)

  if(log){
    return(lterm)
  }
  else{
    return(exp(lterm))
  }

}
