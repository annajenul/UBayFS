#' @importFrom matrixStats logSumExp
#' @export

admissibility <- function(state, A, b, rho, log = TRUE){

  inner = (b - A %*% state) * rho
  inner = cbind(inner, 0)

  lterm <- sum(inner) - sum(apply(inner, 1, logSumExp))

  if(log){
    return(lterm)
  }
  else{
    return(exp(lterm))
  }

}
