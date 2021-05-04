#' UBayFS feature selection
#' @description genetic algorithm to train UBayFS feature selection model using genetic algorithm
#' @param x a UBaymodel created by build.UBaymodel
#' @return a UBaymodel with an additional list element output containing the maximum a-posteriori estimate (map)
#' @import dplyr
#' @importFrom GA ga
#' @importFrom DirichletReg ddirichlet
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

  #define parameters
  n = length(x$ensemble.params$output$counts)			# number of features

  # define constraints
  A = x$user.params$constraints$A
  b = x$user.params$constraints$b
  rho = x$user.params$constraints$rho

  # define block constraints
  A_block = x$user.params$block_constraints$A
  b_block = x$user.params$block_constraints$b
  rho_block = x$user.params$block_constraints$rho
  block_matrix = x$user.params$block_constraints$block_matrix

  # define prior weights
  alpha = as.numeric(x$user.params$weights)

  # define ensemble counts
  delta = as.numeric(x$ensemble.params$output$counts)

  # calculate posterior parameter
  post_param = alpha + delta
  weights_sum = sum(post_param)								# sum of all posterior weights

  # Greedy algorithm to select starting vectors
  x_start = sampleInitial(post_scores = post_param,
                          constraints = x$user.params$constraints,
                          block_constraints = x$user.params$block_constraints,
                          constraint_dropout_rate = x$optim.params$constraint_dropout_rate,
                          size = x$optim.params$popGreedy)

  # optimization using GA
  target_fct = function(state){								# target function for optimization procedure
    return(
        admissibility(state, 									# log-admissibility function
                    A,
                    b,
                    rho,
                    weights_sum,
                    log = TRUE) +
        block_admissibility(state, 									# log-admissibility function
                      A_block,
                      b_block,
                      rho_block,
                      block_matrix,
                      weights_sum / nrow(block_matrix),
                      log = TRUE) +
        ddirichlet(t(state + 0.01), 							# log-dirichlet-density (with small epsilon to avoid errors from 0 probs)
                   alpha = post_param,
                   log = TRUE)
    )
  }
  optim = ga(type = "binary",								# use GA for optimization
             fitness = target_fct,
             lower = 0,
             upper = 1,
             nBits = n,
             maxiter = x$optim.params$maxiter,
             popSize = x$optim.params$popsize,
             suggestions = x_start
  )
  x_optim = optim@solution									# extract solution

  # return result
  x$output = list(map = x_optim[1,])					# return solution as vector

  return(x)
}
