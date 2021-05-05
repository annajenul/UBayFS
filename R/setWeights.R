#' set weights in UBaymodel object
#' @description sets the prior weights in a UBaymodel object
#' @param model a UBaymodel object created using build.UBaymodel
#' @param weights the vector of user-defined prior weights for each feature
#' @param block_list the list of feature indices for each block; only required, if block-wise weights are specified and block_matrix is NULL
#' @param block_matrix the matrix containing affiliations of features to each block; only required, if block-wise weights are specified and block_list is NULL
#' @return a UBaymodel object with updated prior weights
#' @seealso build.UBaymodel
#' @export

setWeights = function(model, weights, block_list = NULL, block_matrix = NULL){

  if(class(model) != "UBaymodel"){
    stop("Wrong class of model")
  }

  if(is.null(weights)){
    stop("Error: weights cannot be empty")
  }


  if(!is.null(block_matrix) | !is.null(block_list)){
    if(is.null(block_matrix)){
      block_matrix = matrix(0, nrow = length(block_list), ncol = max(unlist(block_list)))
      for(i in 1:length(block_list)){
        block_matrix[i,block_list[[i]]] <- 1
      }
    }
    if(nrow(block_matrix) != length(weights)){
      stop("Error: wrong length of weights vector: must match number of blocks, if block_matrix or block_list are provided")
    }
    weights = as.vector(t(block_matrix) %*% weights)
  }

  if((length(weights) > 1) & (length(weights) != ncol(model$data))){
    stop("Error: length of prior weights does not match data matrix")
  }
  else if(length(weights) == 1){
    weights = rep(weights, ncol(model$data))
  }

  if(any(weights <= 0)){
    stop("Error: weights must be positive")
  }

  model$user.params$weights = weights

  return(model)
}
