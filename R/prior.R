#' @importFrom DirichletReg ddirichlet
#' @export

prior <- function(state, user.params){

  return(ddirichlet(state, user.params$weights, log = TRUE))

}
