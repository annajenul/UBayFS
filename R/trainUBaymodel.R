#' UBayFS feature selection
#' @description genetic algorithm to train UBayFS feature selection model
#' @param x a UBaymodel created by build.UBaymodel
#' @return a UBaymodel with an additional list element output containing the maximum a-posteriori estimate (map)
#' @importFrom GA ga
#' @importFrom DirichletReg ddirichlet
#' @importFrom matrixStats logSumExp
#' @export train

train <- function(x){
  UseMethod("train")
}

#' @method train UBaymodel
#' @export

train.UBaymodel = function(x){

  if(class(x) != "UBaymodel"){
    stop("Wrong class of model")
  }

  # define posterior expected parameter
  theta = posterior_expectation(model)

  if(x$optim.params$method == "GA"){
    print("Running Genetic Algorithm")
    x$output <- train_GA(theta,
                         x$user.params$constraints,
                         x$user.params$block_constraints,
                         x$optim.params,
                         colnames(x$data))
  }
  else if(x$optim.params$method == "MH"){
    print("Running Metropolis-Hastings Algorithm")
    x$output <- train_MH(theta,
                         x$user.params$constraints,
                         x$user.params$block_constraints,
                         x$optim.params,
                         colnames(x$data))
  }
  else{
    stop("Error: method not supported.")
  }

  return(x)
}

train_GA <- function(theta, constraints, block_constraints, optim_params, feat_names){

  target_fct <- function(state){return( - logSumExp(c(theta[state == 0],
                                                 admissibility(state = state,
                                                                            constraints = constraints,
                                                                            weights_sum = NULL,
                                                                            log = TRUE),
                                                 block_admissibility(state = state,
                                                                     constraints = block_constraints,
                                                                     weights_sum = NULL,
                                                                     log = TRUE))))}

  # Greedy algorithm to select starting vectors
  x_start = sampleInitial(post_scores = exp(theta),
                          constraints = constraints,
                          block_constraints = block_constraints,
                          size = optim_params$popsize)


  optim = ga(type = "binary",								# use GA for optimization
             fitness = target_fct,
             lower = 0,
             upper = 1,
             nBits = length(theta),
             maxiter = optim_params$maxiter,
             popSize = optim_params$popsize,
             suggestions = x_start
  )
  x_optim = optim@solution									# extract solution
  if(is.vector(x_optim)){
    x_optim <- as.data.frame(t(x_optim))
  }
  else{
    x_optim <- as.data.frame(x_optim)
  }
  colnames(x_optim) <- feat_names

  return(list(map = x_optim))
}

