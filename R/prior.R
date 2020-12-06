#' @importFrom DirichletReg ddirichlet
#' @export

# deprecated
prior <- function(state, user.params, log = TRUE){

  return(ddirichlet(state, user.params$weights, log = log))

}
