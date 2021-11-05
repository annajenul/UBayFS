#' posterior function
#' @description evaluates the value of the posterior probability density (target function)
#' @param model a UBaymodel object created using build.UBaymodel
#' @param state a binary membership vector describing a feature set
#' @param post_param a vector of posterior weights for the Dirichlet distribution
#' @param log whether the admissibility should be returned on log scale
#' @return a posterior probability value
#' @importFrom DirichletReg ddirichlet

posterior = function(state, post_param, log = TRUE){								# target function for optimization procedure
  post <-
    ddirichlet(t(state + 0.01), 							# log-dirichlet-density (with small epsilon to avoid errors from 0 probs)
                 alpha = post_param,
                 log = TRUE)
  if(!log){
    post <- exp(post)
  }
  return(post)
}

#' @export

getPosterior <- function(state, model, log = TRUE){
  return(posterior(
    state = state,
    post_param = model$user.params$weights + model$ensemble.params$output$counts,
    log = log
  ))
}
