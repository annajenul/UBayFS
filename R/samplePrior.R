#' @importFrom DirichletReg rdirichlet
#' @importFrom parallel detectCores
#' @importFrom parallel makeCluster
#' @importFrom parallel stopCluster
#' @importFrom pbapply pbapply
#' @export

sample.prior <- function(user.params, n.samples){

  # sample from Dirichlet prior distribution
  sample <- rdirichlet(n.samples, user.params$weights)

  # acceptance-rejection w.r.t. admissibility
  ## create parallel cluster
  cl <- parallel::makeCluster(parallel::detectCores() - 1)

  ## create acceptance probabilities
  acception_probs <- pbapply(sample, 1, admissibility,
                     A = user.params$constraints$A,
                     b = user.params$constraints$b,
                     rho = user.params$constraints$rho,
                     log = FALSE,
                     cl = cl)
  ## sample acceptance
  sample_selected <- apply(cbind(acceptance_probs, 1-acceptance_probs), 1, sample, x = c(TRUE, FALSE), size = 1)

  ## stop cluster
  parallel::stopCluster(cl)

  ## if no sample selected
  if(!any(sample_selected)){
    stop("Error: no sample selected")
  }

  return(sample[sample_selected,])

}
