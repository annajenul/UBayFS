#' @importFrom parallel detectCores
#' @importFrom parallel makeCluster
#' @importFrom parallel stopCluster
#' @importFrom pbapply pbapply
#' @importFrom DirichletReg ddirichlet
#' @importFrom shiny incProgress
#' @export

# deprecated
sample.posterior <- function(user.params, ensemble.params, sampling.params, shiny = FALSE){

  # implements an independence-chain version of Metropolis Hastings algorithm

  # initialize parameters
  fullSample <- matrix(, nrow = 0, ncol = length(user.params$weights))
  sample <- matrix(,nrow = sampling.params$sample_size, ncol = length(user.params$weights))
  fullSampleVals <- c()
  f_theta_t <- rep(-Inf, nrow(sample))
  t <- 0

  ## create parallel cluster
  cl <- parallel::makeCluster(parallel::detectCores() - 1)

  ## create acceptance probabilities
  for(t in 1:sampling.params$t_max){
    print(paste0("iteration ", t))

    # sample from prior as proposal density
    sample0 <- sample.prior(user.params, sampling.params)

    # evaluate likelihood and kappa
    likelihood_vals <- pbapply(sample0, 1, likelihood,
                               ensemble.params = ensemble.params,
                               log = TRUE,
                               cl = cl)
    kappa_vals <- pbapply(sample0, 1, admissibility,
                          A = user.params$constraints$A,
                          b = user.params$constraints$b,
                          rho = user.params$constraints$rho,
                          log = TRUE,
                          cl = cl)

    f_theta_star <- likelihood_vals + kappa_vals # full nominator (in log-scale)

    MH_ratio <- f_theta_star - f_theta_t # log-MH_ratio
    MH_ratio <- apply(cbind(MH_ratio, 0), 1, min) # bound by 1 (0 in log scale) above

    accept_inds <- apply(cbind(exp(MH_ratio), 1-exp(MH_ratio)), 1, sample, x = c(TRUE, FALSE), size = 1, replace = TRUE) # indices with accepted updates
    print(sum(accept_inds) / length(accept_inds))

    # update accepted entries
    sample[accept_inds,] <- sample0[accept_inds,]
    f_theta_t[accept_inds] <- f_theta_star[accept_inds]
    if(t > sampling.params$t_bi){
      fullSample <- rbind(fullSample,sample)
      fullSampleVals <- c(fullSampleVals,f_theta_t)
    }
    if(shiny){
      incProgress(amount = 1/sampling.params$t_max, detail = "running MCMC")
    }
  }

  ## stop cluster
  parallel::stopCluster(cl)

  return(list(
    sample = fullSample,
    vals = fullSampleVals + ddirichlet(fullSample, alpha = user.params$weights, log = TRUE)
  ))

}
