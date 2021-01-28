#' @import dplyr
#' @importFrom GA ga
#' @importFrom DirichletReg ddirichlet
#' @export
# function to perform UBayFS feature selection
train_model = function(model){

  #define parameters
  n = length(model$ensemble.params$output$counts)			# number of features

  # define constraints
  A = model$user.params$constraints$A
  b = model$user.params$constraints$b
  rho = model$user.params$constraints$rho

  # define prior weights
  alpha = as.numeric(model$user.params$weights)

  # define ensemble counts
  delta = as.numeric(model$ensemble.params$output$counts)

  # calculate posterior parameter
  post_param = alpha + delta
  weights_sum = sum(post_param)								# sum of all posterior weights

  # Greedy algorithm to select a starting vector
  x_start = rep(0, n)										# initialize with empty feature set
  initial_importance = order(post_param, decreasing = TRUE)	# order by importance (w.r.t. posterior weights)
  i = 1														# iterate over features in descending order
  for(i in 1:n){
    x_new = x_start
    x_new[initial_importance[i]] = 1						# try to add feature
    if(admissibility(x_new, A, b, rep(Inf, length(b)), log = FALSE) == 1){# verify if constraints are still satified
      x_start = x_new										# if yes, accept feature
    }
    i = i+1
  }

  # optimization using GA
  target_fct = function(state){								# target function for optimization procedure
    return(
      admissibility(state, 									# log-admissibility function
                    A,
                    b,
                    rho,
                    weights_sum,
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
             maxiter = model$optim.params$maxiter,
             popSize = model$optim.params$popsize,
             suggestions = t(x_start)
  )
  x_optim = optim@solution									# extract solution

  # return result
  model$output = list(map = x_optim[1,])					# return solution as vector

  return(model)
}
