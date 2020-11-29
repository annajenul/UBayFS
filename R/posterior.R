#' @importFrom DirichletReg ddirichlet
#' @export

posterior <- function(state, ensemble.params, prior.params){

  return(ddirichlet(state, user.params$weights + ensemble.params$output$counts, log = TRUE))

}
