posterior <- function(state, likelihood.params, prior.params){
  return(likelihood(state, likelihood.params) + prior(state, prior.params))
}
