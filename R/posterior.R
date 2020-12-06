#' @importFrom DirichletReg ddirichlet
#' @export

# deprecated
posterior <- function(state, ensemble.params, prior.params){

  return(ddirichlet(state, user.params$weights + ensemble.params$output$counts, log = TRUE))

}
