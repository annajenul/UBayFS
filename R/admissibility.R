#' @importFrom matrixStats logSumExp
#' @export

admissibility <- function(state, A, b, rho, weights_sum = NULL, isState = FALSE, log = TRUE){

  if(!is.null(weights_sum)){
    rho <- rho * weights_sum
  }

  if(!isState){
    # rescale parameter vector theta by length
    state = state > (1/length(state))
  }

  if(rho[1] < Inf){
    z = (b + 0.5 - A %*% state) * rho
    lprob <- z - apply(cbind(z,0), 1, logSumExp) # 1 + exp(...)
    lprob <- sum(lprob) # product
  }
  else{
    z = (b - A %*% state) >= 0
    lprob <- prod(z)
  }

  if(log){
    return(lprob)
  }
  else{
    return(exp(lprob))
  }

}
