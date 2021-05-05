#' block admissibility function (kappa)
#' @description evaluates the value of the admissibility function kappa for block constraints
#' @param state a binary membership vector describing a feature set
#' @param A the matrix defining the constraint system Ax<=b between blocks
#' @param b the vector defining the constraint system Ax<=b between blocks
#' @param rho the vector of relaxation parameters for the constraint system Ax<=b between blocks
#' @param block_matrix the matrix containing affiliations of features to each block
#' @param weights_sum a scalar denoting the sum of all block weights to scale rho; if NULL, no weighting is performed
#' @param log whether the admissibility should be returned on log scale
#' @return an admissibility value
#' @importFrom matrixStats logSumExp
#' @export

block_admissibility = function(state, A, b, rho, block_matrix, weights_sum = NULL, log = TRUE){

  if(!is.null(block_matrix)){
    state_block = (block_matrix %*% state) > 0
  }
  else{
    state_block = state
  }
  return(admissibility(state_block, A, b, rho, weights_sum, log))

}
