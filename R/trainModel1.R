#' @import dplyr
#' @importFrom GA ga
#' @importFrom DirichletReg ddirichlet
#' @export

train_model1 <- function(model, shiny = FALSE){

  #features
  n = ncol(model$ensemble.params$output$full_counts)

  # input parameters
  A = model$user.params$constraints$A
  b = model$user.params$constraints$b
  rho = model$user.params$constraints$rho

  # Dirichlet prior parameters
  alpha = as.numeric(model$user.params$weights)
  delta = model$ensemble.params$output$counts
  param = alpha + delta
  weights_sum = sum(param)
  print(rho * weights_sum)

  # select a starting point via Greedy algorithm
  x_start = rep(0, n)
  initial_importance = order(param, decreasing = TRUE)
  i = 1
  for(i in 1:n){
    x_new <- x_start
    x_new[initial_importance[i]] <- 1
    if(all(A %*% x_new <= b)){
      x_start <- x_new
    }
    i <- i+1
  }

  print(x_start)
  print(admissibility(x_start, A, b, rho, weights_sum, isState = TRUE, log = TRUE))
  print(ddirichlet(t(x_start + 0.01), alpha = param, log = TRUE))

  target_fct <- function(state){
    return(
      admissibility(state, A, b, rho, weights_sum, isState = TRUE, log = TRUE) +
      ddirichlet(t(state + 0.01), alpha = param, log = TRUE)
    )
  }

  print(target_fct(x_start))

  optim <- ga(type = "binary",
                fitness = target_fct,
                lower = 0,
                upper = 1,
                nBits = n,
                suggestions = t(x_start),
                seed = 1
  )
  x_optim <- optim@solution

  print(x_optim)

  model$output <- list(map = x_optim[1,],
                       all_solutions = x_optim)


  return(model)
}
