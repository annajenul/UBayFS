#' admissibility function (kappa)
#' @description evaluates the value of the admissibility function kappa
#' @param state a binary membership vector describing a feature set
#' @param constraints a list constaining a matrix A and a vector b representing the inequality system Ax<=b and a vector rho
#' @param weights_sum a scalar denoting the sum of all feature weights to scale rho; if NULL, no weighting is performed
#' @param log whether the admissibility should be returned on log scale
#' @return an admissibility value
#' @importFrom matrixStats logSumExp
#' @export

admissibility = function(state, constraints, weights_sum = NULL, log = TRUE){

  # parse input
  A = constraints$A
  b = constraints$b
  rho = constraints$rho

  # check input
  if(!is.null(A)){
    if(ncol(A) != length(state) | nrow(A) != length(b) | length(b) != length(rho)){
      stop("Error: dimensions of constraints do not match")
    }
  }

  if(is.null(A)){
    if(log){
      return(0)
    }
    else{
      return(1)
    }
  }

  if(!is.null(weights_sum)){        			# scale rho by weights_sum
    rho <- rho * weights_sum
  }

  ind_inf = (rho == Inf)

  # case 1: rho < Inf
  if(any(!ind_inf)){
    z = (-b[!ind_inf] - 0.5 + A[!ind_inf,] %*% state) * rho[!ind_inf]			# compute exponential term (nom) for each constraint (in log-scale)
    lprob1 = z - apply(cbind(z,0), 1, logSumExp) # log(nom) - log(1 + nom)
    lprob1 = sum(lprob1)							# product over all constraints
  }
  else{
    lprob1 = 0
  }

  # case 2: rho = Inf
  if(any(ind_inf)){
    z = (b[ind_inf] - A[ind_inf,] %*% state) < 0
    lprob2 = log(prod(z))						# 1, if all constraints are fulfilled, 0 else
  }
  else{lprob2 = 0}

  if(log){										# return admissibility in log or original scale
    return(lprob1 + lprob2)
  }
  else{
    return(exp(lprob1 + lprob2))
  }
}
