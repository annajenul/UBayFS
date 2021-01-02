#' @importFrom pbapply pbapply
#' @importFrom DirichletReg ddirichlet
#' @importFrom DirichletReg rdirichlet
#' @importFrom shiny incProgress
#' @export

sample.posterior1 <- function(user.params, ensemble.params, sampling.params, shiny = FALSE){

  # implements an hybrid Gibbs-MH (Metropolis-Hastings) algorithm

  # input parameters
  A = user.params$constraints$A
  b = user.params$constraints$b
  rho = user.params$constraints$rho

  weights = as.numeric(user.params$weights)
  weight_sum = sum(weights)
  weights = weights / weight_sum

  n = ncol(ensemble.params$output$full_counts)
  N = sampling.params$sample_size

  # parameters for gamma (method of moments: expectation = variance = rho)
  shape = rho
  scale = 1

  # initialize parameters (alpha initialized with proposal density)
  fullTheta <- matrix(, nrow = 0, ncol = n)
  alpha <- t(rmultinom(N, size = ceiling(weight_sum), prob = weights)) # cols .. features, rows .. instances
  #rho <- do.call(rbind, rep(list(rho), N))
  t <- 0

  updateAlpha <- function(theta, alpha, rho){
    # implements MH-step

    # sample from Multinomial as proposal density
    alpha_new <- t(rmultinom(N, size = ceiling(weight_sum), prob = weights))

    # compute MH-ratio
    mh_ratio <- colSums(apply(alpha_new + 1, 1, ddirichlet, x = theta, log = TRUE)) + apply(alpha_new, 1, admissibility, A, b, rho, log = TRUE)
              - colSums(apply(alpha + 1, 1, ddirichlet, x = theta, log = TRUE)) - apply(alpha, 1, admissibility, A, b, rho, log = TRUE)

    mh_ratio <- sapply(mh_ratio, function(x){return(min(x,0))})

    # accept with prob equal to mh_ratio
    accepted_updates <- pbapply(cbind(exp(mh_ratio), 1-exp(mh_ratio)), 1, sample, x = c(TRUE, FALSE), size = 1, replace = TRUE) # indices with accepted updates

    # update alpha
    alpha[accepted_updates,] <- alpha_new[accepted_updates,]

    return(alpha)
  }

  updateTheta <- function(alpha){
    # implements conditional distribution

    # sample from conditional distribution of theta
    theta_new <- rdirichlet(N, colSums(alpha + ensemble.params$output$counts + 1)) # +1 necessary to avoid zeros

    return(theta_new)
  }

  updateRho <- function(alpha, rho){
    # implements MH step

    # sample from Multivariate Logistic as proposal density
    rho_new <- apply(cbind(shape, scale), 1, function(x){return(rgamma(n = 1, shape = x[1], scale = x[2]))})

    # compute MH-ratio
    mh_ratio <- sum(apply(alpha, 1, admissibility, rho = rho_new, A = A, b = b, log = TRUE))
              - sum(apply(alpha, 1, admissibility, rho = rho, A = A, b = b, log = TRUE))

    mh_ratio <- sapply(mh_ratio, function(x){return(min(x,0))})

    # accept with prob equal to mh_ratio
    #accepted_updates <- pbapply(cbind(exp(mh_ratio), 1-exp(mh_ratio)), 1, sample, x = c(TRUE, FALSE), size = 1, replace = TRUE) # indices with accepted updates
    accepted_update <- sample(x = c(TRUE, FALSE), prob = c(exp(mh_ratio), 1-exp(mh_ratio)), size = 1, replace = TRUE) # indices with accepted updates

    # update alpha
    #rho[accepted_updates,] <- rho_new[accepted_updates,]

    if(accepted_update){
      rho = rho_new
    }

    return(rho)
  }

  ## create acceptance probabilities
  for(t in 1:sampling.params$t_max){
    print(paste0("iteration ", t))

    theta <- updateTheta(alpha)
    alpha <- updateAlpha(theta, alpha, rho)
    rho <- updateRho(alpha, rho)

    if(t > sampling.params$t_bi){
      print(dim(fullTheta))
      print(dim(theta))
      fullTheta <- rbind(fullTheta, theta)
    }

    if(shiny){
      incProgress(amount = 1/sampling.params$t_max, detail = "running MCMC")
    }
  }

  return(fullTheta)

}
