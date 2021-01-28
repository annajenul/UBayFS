#' @export

set_optim_params = function(model, popsize, maxiter){

  model$optim.params <- list(popsize = popsize, maxiter = maxiter)

  return(model)
}
