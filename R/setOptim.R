#' set optimization parameters in UBaymodel object
#' @description sets the optimization parameters in a UBaymodel object
#' @param model a UBaymodel object created using build.UBaymodel
#' @param popGreedy size of the initial population obtained from Greedy algorithm. Must be smaller or equal to popsize.
#' @param popsize size of the initial population of the genetic algorithm for model optimization
#' @param maxiter maximum number of iterations of the genetic algorithm for model optimization
#' @param constraint_dropout_rate rate of dropping constraints in Greedy algorithm

#' @return a UBaymodel object with updated optimization parameters
#' @seealso build.UBaymodel
#' @export

setOptim = function(model, popGreedy, popsize, maxiter, constraint_dropout_rate){

  if(class(model) != "UBaymodel"){
    stop("Wrong class of model")
  }

  if(popsize < 10 | maxiter < 10){
    stop("Error: popsize or maxiter < 10 does not make sense")
  }

  if(popGreedy > popsize){
    stop("Error: popGreedy must be smaller than popsize")
  }

  if(constraint_dropout_rate > 1 | constraint_dropout_rate < 0){
    stop("Error: constraint_dropout_rate must be in [0,1]")
  }

  model$optim.params <- list(popGreedy = popGreedy,
                             popsize = popsize,
                             maxiter = maxiter,
                             constraint_dropout_rate = constraint_dropout_rate)

  return(model)
}
