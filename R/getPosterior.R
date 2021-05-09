#' posterior function
#' @description evaluates the value of the posterior probability density (target function)
#' @param model a UBaymodel object created using build.UBaymodel
#' @param state a binary membership vector describing a feature set
#' @param constraints a list constaining a matrix A and a vector b representing the inequality system Ax<=b and a vector rho
#' @param block_constraints a list containing a relaxed system Ax<=b of user constraints on feature blocks, given as matrix A, vector b and vector or scalar rho (relaxation parameters); see buildConstraints function
#' @param post_param a vector of posterior weights for the Dirichlet distribution
#' @param log whether the admissibility should be returned on log scale
#' @return a posterior probability value
#' @importFrom DirichletReg ddirichlet

posterior = function(state, constraints, block_constraints, post_param, log = TRUE){								# target function for optimization procedure
  return(
    admissibility(state, 									# log-admissibility function
                  constraints,
                  sum(post_param),
                  log = log) +
    block_admissibility(state, 									# log-admissibility function
                        block_constraints,
                        sum(post_param) / nrow(block_constraints$block_matrix),
                        log = log) +
    ddirichlet(t(state + 0.01), 							# log-dirichlet-density (with small epsilon to avoid errors from 0 probs)
                 alpha = post_param,
                 log = log)
  )
}

#' @export

getPosterior <- function(state, model, log = TRUE){
  return(posterior(
    state = state,
    constraints = model$user.params$constraints,
    block_constraints = model$user.params$block_constraints,
    post_param = model$user.params$weights + model$ensemble.params$output$counts,
    log = log
  ))
}
