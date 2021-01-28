#' @export

set_weight_params = function(model, weights){

  model$user.params$weights <- weights

  return(model)
}
