#' block admissibility function (kappa)
#' @description evaluates the value of the admissibility function kappa for block constraints
#' @param state a binary membership vector describing a feature set
#' @param constraints a list constaining a matrix A and a vector b representing the inequality system Ax<=b, a vector rho, and a block matrix
#' @param log whether the admissibility should be returned on log scale
#' @return an admissibility value
#' @importFrom matrixStats logSumExp
#' @export

block_admissibility = function(state, constraints, log = TRUE){

  # parse input
  block_matrix = constraints$block_matrix

  if(!is.null(block_matrix)){
    state_block = (block_matrix %*% state) > 0
  }
  else{
    state_block = state
  }
  return(admissibility(state_block, constraints, log))

}
