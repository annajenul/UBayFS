#' @importFrom pbapply pbapply
#' @importFrom DirichletReg ddirichlet
#' @importFrom DirichletReg rdirichlet
#' @importFrom polyapost hitrun
#' @importFrom shiny incProgress
#' @export

sample.posterior3 <- function(user.params, ensemble.params, sampling.params, shiny = FALSE){

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
  weights_sum = sum(param)

  # sampling parameters
  N = sampling.params$t_max

  # sample S1
  #S1 <- rdirichlet(n = N, alpha = as.vector(param))

  # sample S2
  t_bi <- sampling.params$t_bi
  #batch_size = 1e5
  #imax = 1e4
  #i = 0
  size_constraint_index <- which(apply(A == 1,1,all))
  A_prep <- A[-size_constraint_index,]
  b_prep <- b[-size_constraint_index]
  out = hitrun(alpha = param, a1 = A_prep, b1 = b_prep / ncol(A), nbatch = t_bi, blen = 1) # initialize Hit-Run algorithm
  #S2 = matrix(, ncol = n, nrow = 0)
  #S <- rdirichlet(N, alpha = param)

  #while ((nrow(S2) < N) & (i < imax)) {
  #  print(paste0("batch_no", i, "- rows found", nrow(S2)))
    out = hitrun(out, nbatch = N, blen = 1)
    S <- out$batch
  #  out<- list(batch = rdirichlet(batch_size, alpha = param))
  #  feat_sample = t(out$batch > (1 / ncol(out$batch)))
  #  mat = A %*% feat_sample - matrix(b, nrow = length(b), ncol = ncol(feat_sample))
  #  rows = out$batch[colSums(mat <= 0) == nrow(mat), ]
  #  S2 <- rbind(S2, rows)
  #  i = i + 1
  #}
  #if(nrow(S2) < N){stop("Constraint too hard.")}
  #else{ S2 = S2[1:N, ]}

  #s_sample = sample(c(0,1), nrow(S1), replace=TRUE, prob = c( (1/(1+rho)), (rho/(1+rho)) ))
  # attention: this is ordered!
  #S = rbind(S1[s_sample==0, ], S2[s_sample==1, ])
  #return(S)
  print("Simulation complete")

  # help variable
  sum_trans <- 0

  theta <- t(S[1,])
  for(i in 2:nrow(S)) {
    if(shiny){
      incProgress(amount = 1/nrow(S), detail = "running MCMC")
    }

    R = min(exp(admissibility(S[i,], A_prep, b_prep, rho, weights_sum, isState = FALSE, log = TRUE) -
                admissibility(theta[i-1,], A_prep, b_prep, rho, weights_sum, isState = FALSE, log = TRUE)),
            1)
    trans = sample(c(TRUE, FALSE), size=1, prob=c(R, 1-R))

    if(trans){
      theta = rbind(theta,S[i,])
      if(sum_trans[length(sum_trans)] > 100){
        print(which(theta[i-1, ] > (1/ncol(theta))))
      }
      sum_trans <- c(sum_trans, 0)
    }
    else{
      theta = rbind(theta, theta[(i-1),])
      sum_trans[length(sum_trans)] <- sum_trans[length(sum_trans)] + 1
    }
  }

  return(theta)

}
