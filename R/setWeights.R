#' set weights in UBaymodel object
#' @description sets the prior weights in a UBaymodel object
#' @param model a UBaymodel object created using build.UBaymodel
#' @param weights the vector of user-defined prior weights for each feature
#' @return a UBaymodel object with updated prior weights
#' @seealso build.UBaymodel
#' @export

setWeights = function(model, weights){

  if(class(model) != "UBaymodel"){
    stop("Wrong class of model")
  }

  if(is.null(weights)){
    stop("Error: weights cannot be empty")
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
