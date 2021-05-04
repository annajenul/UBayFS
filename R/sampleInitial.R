#' initial feature set sampling using probabilistic Greedy algorithm
#' @description sample initial solutions using a probabilistic version of Greedy algorithm
#' @param post_scores a vector of posterior scores (prior scores + likelihood) for each feature
#' @param constraints a list containing feature-wise constraints
#' @param block_constraints a list containing block-wise constraints
#' @param size initial number of samples to be created. The output sample size can be lower, since duplicates are removed.
#' @return a matrix containing initial feature sets as rows.
#' @export

sampleInitial <- function(post_scores, constraints, block_constraints, constraint_dropout_rate, size){

  n = length(post_scores) # number of features
  num_feat_constraints = ifelse(is.null(constraints$A), 0, nrow(constraints$A))
  num_constraints = num_feat_constraints +
                    ifelse(is.null(block_constraints$A), 0, nrow(block_constraints$A))

  full_admissibility <- function(state, constraints, block_constraints, constraint_dropout, log = TRUE){
    active_constraints <- which(constraint_dropout == 1)
    active_feat_constraints <- active_constraints[active_constraints <= num_feat_constraints]
    active_block_constraints <- active_constraints[active_constraints > num_feat_constraints] - num_feat_constraints

    res = ifelse(log, 0, 1)
    if(!is.null(constraints) & (length(active_feat_constraints) > 0)){

      a = admissibility(state,
                        constraints$A[active_feat_constraints,, drop = FALSE],
                        constraints$b[active_feat_constraints],
                        rep(Inf, length(active_feat_constraints)),
                        log = log)
      res = ifelse(log, res + a, res * a)
    }
    if(!is.null(block_constraints) & length(active_block_constraints) > 0){
      a = block_admissibility(state,
                              block_constraints$A[active_block_constraints,, drop = FALSE],
                              block_constraints$b[active_block_constraints],
                              rep(Inf, length(active_block_constraints)),
                              block_constraints$block_matrix,
                              log = log)
      res = ifelse(log, res + a, res * a)
    }
    return(res)
  }

  # shuffle features n times (sampling without replacement), with probability proportional to post_scores
  feature_orders = t(
                        replicate(size,
                               sample.int(
                                  n = n,
                                  size = n,
                                  replace = FALSE,
                                  prob = post_scores + 0.01) # add a small constant to account for unobserved elements
                        )
                     )


  x_start = t(apply(feature_orders, 1, function(order){
    constraint_dropout <- sample(x = c(0,1),
                                 size = num_constraints,
                                 replace = TRUE,
                                 prob = c(constraint_dropout_rate, 1-constraint_dropout_rate)
    )
    i = 1														# iterate over features in descending order
    x = rep(0, n)
    for(i in 1:n){
      x_new = x
      x_new[order[i]] = 1						# try to add feature
      if(full_admissibility(x_new,
                            constraints,
                            block_constraints,
                            constraint_dropout,
                            log = FALSE) == 1){# verify if constraints are still satisfied
        x = x_new										# if yes, accept feature
      }
      i = i+1
    }
    return(x)
  }))

  return(unique(x_start)) # unique removes duplicated rows (feature sets) before returning
}
