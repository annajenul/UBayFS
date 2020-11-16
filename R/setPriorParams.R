#' @export


set_prior_params <- function(model, A, b, rho){

  model$prior.params <- list(A=A, b=b, rho=rho)

  return(model)
}
