#' set weights in UBaymodel object
#' @description sets the prior weights in a UBaymodel object
#' @param model a UBaymodel object created using build.model
#' @param weights the vector of user-defined prior weights for each feature
#' @return a UBaymodel object with updated prior weights
#' @seealso build.model
#' @export

set_weight_params = function(model, weights){

  if(class(model) != "UBaymodel"){
    stop("Wrong class of model")
  }

  if(length(weights) != ncol(model$data)){
    stop("Error: length of prior weights does not match data matrix")
  }

  model$user.params$weights <- weights

  return(model)
}
