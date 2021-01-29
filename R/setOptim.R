#' set optimization parameters in UBaymodel object
#' @description sets the optimization parameters in a UBaymodel object
#' @param model a UBaymodel object created using build.UBaymodel
#' @param popsize size of the initial population of the genetic algorithm for model optimization
#' @param maxiter maximum number of iterations of the genetic algorithm for model optimization
#' @return a UBaymodel object with updated optimization parameters
#' @seealso build.UBaymodel
#' @export

setOptim = function(model, popsize, maxiter){

  if(class(model) != "UBaymodel"){
    stop("Wrong class of model")
  }

  if(popsize < 10 | maxiter < 10){
    stop("Error: popsize or maxiter < 10 does not make sense")
  }

  model$optim.params <- list(popsize = popsize, maxiter = maxiter)

  return(model)
}
