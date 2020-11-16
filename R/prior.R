#' @importFrom matrixStats logSumExp
#' @export

prior <- function(state, prior.params){

  inner = (prior.params$b - prior.params$A %*% state) * prior.params$rho
  inner = cbind(inner, 0)
  return(sum(inner) - sum(apply(inner, 1, logSumExp)))
}
