#' @importFrom matrixStats logSumExp
#' @export


likelihood <- function(state, likelihood.params){

  # counts and max counts from ensemble feature selectors
  counts = likelihood.params$counts
  max_counts = likelihood.params$max_counts

  # success probability for important (i) and unimportant (u) features
  theta_i = seq(0.01, 0.99, by=0.01)
  theta_u = theta_i

  Theta = expand.grid(theta_i = theta_i, theta_u = theta_u)
  Theta = Theta[Theta$theta_i > Theta$theta_u,]

  # vector of likelihood terms
  # one entry for each combination of theta_i, theta_u
  L = apply(Theta, 1, function(x){
    return(sum(c(dbinom(x = counts[state==1],
                        prob = x[1],
                        size = max_counts,
                        log = TRUE),
                 dbinom(x = counts[state==0],
                        prob = x[2],
                        size = max_counts,
                        log = TRUE))))

  })

  # Version 1 (integral)
  L_mat = matrix(nrow = length(theta_i), ncol= length(theta_u))
  L_mat[lower.tri(L_mat, diag = FALSE)] = L

  # distance between support points (equal for x and y axes)
  dx = 1/(length(theta_i)-1)
  # weights for trapezoidal rule along theta_u axis
  W1 = matrix(c(1, rep(2, length(theta_i)-2), 1), nrow = length(theta_i), ncol = length(theta_i))
  W1[upper.tri(W1, diag = TRUE)] = 0
  diag(W1[-1,]) = 1
  # weights for trapezoidal rule along theta_i axis
  W2 = matrix(c(1, rep(2, length(theta_i)-2), 1), nrow = length(theta_i), ncol = length(theta_i), byrow = TRUE)
  W2[upper.tri(W2, diag = TRUE)] = 0
  diag(W2[-1,]) = 1
  # joint weights for 2D-trapezoidal rule
  W = W1 * W2

  # weight L_mat by W
  # multiply with dx/2 * dy/2 (dx=dy)
  # standardize by total number of features
  return(logSumExp(L_mat + log(W), na.rm = TRUE) + log(dx^2 / 4) - log(length(counts)))

  #Version 2 (maximization)
  #return(max(L_mat) - log(length(counts)))

}
