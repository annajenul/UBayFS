#' @importFrom pbapply pbapply
#' @importFrom gtools ddirichlet
#' @importFrom gtools rdirichlet
#' @importFrom polyapost hitrun
#' @importFrom shiny incProgress
#' @export

sample.posterior <- function(user.params, ensemble.params, sampling.params, shiny = FALSE){

  #features
  n = ncol(ensemble.params$output$full_counts)

  # input parameters
  A = user.params$constraints$A
  b = user.params$constraints$b
  rho = user.params$constraints$rho[1]

  # Dirichlet prior parameters
  alpha = as.numeric(user.params$weights)
  delta = ensemble.params$output$counts
  param = alpha + delta

  # sampling parameters
  N = sampling.params$t_max
  t_bi <- sampling.params$t_bi
  theta2_prior = matrix(, ncol = n, nrow = 0)
  batch_size = 1e5
  imax = 1e4
  i = 0

  #out = hitrun(alpha = alpha, a1 = A, b1 = b, nbatch = t_bi, blen = 1)

  while ((nrow(theta2_prior) < N) & (i < imax)) {

    #out = hitrun(out, nbatch = batch_size, blen = 1)
    out <- list(batch = rdirichlet(n = batch_size, alpha = alpha))

    feat_sample = t(out$batch > (1 / ncol(out$batch)))
    mat = A %*% feat_sample - matrix(b, nrow = length(b), ncol = ncol(feat_sample))
    rows = out$batch[colSums(mat <= 0) == nrow(mat), ]

    theta2_prior <- rbind(theta2_prior, rows)
    i = i + 1

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
  S1 <- rdirichlet(n = nrow(theta2_prior), alpha = as.vector(param))

  # initialize S2
  x0 <- theta2_prior[1,]
  S2 <- t(x0)

  # help variable
  sum_trans <- 0

  for(i in 2:nrow(theta2_prior)) {
    if(shiny){
      incProgress(amount = 1/nrow(theta2_prior), detail = "running MCMC")
    }

    R = min(exp(dmultinom(x = delta, prob = theta2_prior[i,], log = TRUE) + ddirichlet(x = S2[(i-1), ], alpha = alpha)
                - dmultinom(x = delta, prob = S2[(i-1), ], log = TRUE) - ddirichlet(x = theta2_prior[i,], alpha = alpha)),
            1)
    trans = sample(c(TRUE, FALSE), size=1, prob=c(R, 1-R))

    if(trans){
      S2 = rbind(S2,theta2_prior[i,])
      sum_trans[length(sum_trans)] <- sum_trans[length(sum_trans)] + 1
    }
    else{
      S2 = rbind(S2, S2[(i-1),])
      sum_trans <- c(sum_trans, 0)
    }
  }

  s_sample = sample(c(0,1), nrow(S1), replace=TRUE, prob = c( (1/(1+rho)), (rho/(1+rho)) ))

  # attention: this is ordered!
  S = rbind(S1[s_sample==0, ], S2[s_sample==1, ])

  return(S)

}
