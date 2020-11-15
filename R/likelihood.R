#' @importFrom matrixStats logSumExp


likelihood <- function(state, likelihood.params){

  p = seq(0,1,by=0.01)
  L = c()
  for(i in ncol(likelihood.params$counts)){
    if(state[i] == 0){
      L = cbind(L, dbeta(x = (1-p), shape1 = likelihood.params$alpha0[1], shape2 = likelihood.params$beta0[1], log = TRUE))
    }
    else{
      L = cbind(L, dbeta(x = p, shape1 = likelihood.params$alpha0[2], shape2 = likelihood.params$beta0[2], log = TRUE))
    }
  }
  L = rowSums(L)

  return(max(L[ceiling(length(L)/2) : length(L)]) - logSumExp(L))

}
