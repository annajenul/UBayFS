#' Evaluate  a feature set
#' @description Evaluates a feature set under the UBayFS model framework.
#' @param state a binary membership vector describing a feature set
#' @param model a `UBaymodel` object created using \link{build.UBaymodel}
#' @param method type of correlation (`pearson`,`kendall`, or `spearman`)
#' @param log whether the admissibility should be returned on log scale
#' @return a posterior probability value
#' @importFrom stats cor
#' @export

evaluateFS <- function(state, model, method = 'spearman', log = FALSE){
  if(sum(state) > 1){
    c <- abs(cor(model$data[, state == 1], method = method))
  }
  else{
    c <- NA
  }
  post_scores <- posteriorExpectation(model)
  log_post <- logSumExp(post_scores[state == 1])
  loss <- getNegLoss(state, model, log = FALSE) - model$lambda # substract lambda due to transformation of utility
  if(log){loss = log(loss)}

  # calculate number of violated constraints
  num_viol_const <- 0
  for(const in model$constraint.params){
    num_viol_const <- num_viol_const + sum((const$A %*% ((const$block_matrix %*% state) > 0)) > const$b)
  }

  # calculate output metrics
  vec <- c(
    sum(state),
    round(loss, 3),
    ifelse(log, round(log_post, 3), round(exp(log_post), 3)),
    round(admissibility(state,
                        constraint_list = model$constraint.params,
                        log = log), 3),
    num_viol_const,
    ifelse(is.matrix(c), round((sum(c) - sum(diag(c))) / (sum(state) * (sum(state) - 1)),3), NA))
  colnames <- c('cardinality', 'total utility', 'posterior feature utility', 'admissibility', 'number of violated constraints', 'avg feature correlation')
  if(log){
    colnames[2:4] <- paste('log', colnames[2:4])
  }
  names(vec) <- colnames

  return(vec)
}

#' @describeIn evaluateFS Evaluate multiple feature sets
#' @export

evaluateMultiple <- function(state, model, method = 'spearman', log = TRUE){
  return(apply(state, 1, evaluateFS, model = model, method = method, log = log))
}

