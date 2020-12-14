#' @importFrom matrixStats logSumExp
#' @export

admissibility <- function(state, A, b, rho, log = TRUE){
  state = state > (1/length(state)) # rescale parameter vector theta by length
  cat("STATE:",state, "\n")

  if(rho[1] < Inf){
    z = (b - A %*% state) * rho
    lprob <- z - apply(cbind(z,0), 1, logSumExp)
#    cat("LPROP:",lprob , "\n")
#    print(c(max(abs(lprob)), which.max(abs(lprob))))
    lprob <- sum(lprob)
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
