#' @importFrom pbapply pbapply
#' @importFrom DirichletReg ddirichlet
#' @importFrom DirichletReg rdirichlet
#' @importFrom polyapost hitrun
#' @importFrom shiny incProgress
#' @export

sample.posterior2 <- function(user.params, ensemble.params, sampling.params, t_burn, shiny = FALSE){

  #features
  n = ncol(ensemble.params$output$full_counts)
  #samples
  N = sampling.params$sample_size


  # input parameters
  A = user.params$constraints$A
  b = user.params$constraints$b
  rho = user.params$constraints$rho[1]

  if(!is.numeric(user.params$weights)){
  prior_weights = as.numeric(user.params$weights)
  }
  else{
    prior_weights = user.params$weights
  }

  # S1
  alpha = prior_weights
  delta = colSums(ensemble.params$output$full_counts)
  param = alpha + as.vector(delta)
  S1 = rdirichlet(n = N, alpha = as.vector(param))

  out = hitrun(alpha = alpha, a1=A, b1=b, nbatch=t_burn, blen = 1)
  print("Burnin simulation over")
  out = hitrun(out, nbatch = N+t_burn, blen=1)
  theta2_prior = out$batch
  print("Simulation complete")

  S2 = NULL
  x0 = theta2_prior[1,]
  S2 = rbind(S2,x0)
  for (i in 2:nrow(theta2_prior)) {
    if(shiny){
      incProgress(amount = 1/nrow(theta2_prior), detail = "running MCMC")
    }

    R = min(exp(dmultinom(x = delta, prob = theta2_prior[i,], log = TRUE) - dmultinom(x = delta, prob = S2[(i-1),], log = TRUE)),1)
    prob = sample(c(0,1), size=1, prob=c((1-R), R))
    if(prob ==1){
      S2 = rbind(S2,theta2_prior[i,])
    }
    else{
      S2 = rbind(S2, S2[(i-1),])
    }
  }


  S2 = S2[-c(1:t_burn),]


  s_sample = sample(c(0,1), N, replace=TRUE, prob = c( (1/(1+rho)), (rho/(1+rho)) ))

  # attention: this is ordered!
  S = rbind(S1[s_sample==0, ], S2[s_sample==1, ])

  return(S)

}
