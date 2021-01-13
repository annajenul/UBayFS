#' @importFrom pbapply pbapply
#' @importFrom DirichletReg ddirichlet
#' @importFrom DirichletReg rdirichlet
#' @importFrom polyapost hitrun
#' @importFrom shiny incProgress
#' @export

sample.posterior2 <- function(user.params, ensemble.params, sampling.params, shiny = FALSE){

  #features
  n = ncol(ensemble.params$output$full_counts)


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

  # Dirichlet prior parameters
  alpha = prior_weights
  delta = colSums(ensemble.params$output$full_counts)
  param = alpha + as.vector(delta)

  # sampling parameters
  N = sampling.params$t_max
  t_bi <- sampling.params$t_bi
  theta2_prior = matrix(, ncol = n, nrow=0)
  batch_size = 100000
  imax = 10000
  i = 0

  out = hitrun(alpha = alpha, a1=A, b1=b, nbatch=t_bi, blen = 1)

  while ((nrow(theta2_prior) < N) & (i < imax)) {
    print(nrow(theta2_prior))
    out = hitrun(out, nbatch = batch_size, blen=1)


    oT = t((out$batch > 1/ncol(out$batch)))
    mat = A %*% oT - matrix(b, nrow = length(b), ncol = ncol(oT))
    rows = out$batch[colSums(mat <= 0) == nrow(mat), ]

    theta2_prior <- rbind(theta2_prior, rows)
    i = i + 1

    #admissible <- apply(out$batch, 1, function(x){return(
    #  all( (A %*% (x > (1/length(x))) - b) <= 0) )})
    #theta2_prior <- rbind(theta2_prior, out$batch[admissible,])
    # i = i+1
  }

  if(nrow(theta2_prior) < N){stop("Constraint too hard.")}
  else{ theta2_prior = theta2_prior[1:N, ]}


  # out = hitrun(alpha = alpha, a1=A, b1=b, nbatch=t_burn, blen = 1)
  # print("Burnin simulation over")
  # out = hitrun(out, alpha = alpha, a1=A, b1=b, nbatch = N, blen=1)
  # theta2_prior = out$batch
  # # add additional constraint here
  # size_admissibility <- apply(theta2_prior, 1, function(x){return(
  #   all( (A %*% (x > (1/length(x))) - b) <= 0) )})
  # theta2_prior <- theta2_prior[size_admissibility,]
  # cat("Lenght of theta2_prior", dim(theta2_prior))
  # if(sum(size_admissibility) < 100){
  #   stop("Size constraint too hard: fewer than 100 samples left")
  # }
  # else{
  #   print(paste0(sum(size_admissibility), " samples used \n"))
  # }
  # additional constraint end
  print("Simulation complete")

  # sample S1
  S1 = rdirichlet(n = nrow(theta2_prior), alpha = as.vector(param))

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


  s_sample = sample(c(0,1), nrow(S1), replace=TRUE, prob = c( (1/(1+rho)), (rho/(1+rho)) ))

  # attention: this is ordered!
  S = rbind(S1[s_sample==0, ], S2[s_sample==1, ])

  return(S)

}
