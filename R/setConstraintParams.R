#' @export


set_constraint_params = function(model, A, b, rho){

  model$user.params$constraints = list(A=A,
                                       b=b,
                                       rho=rho)

  return(model)
}
