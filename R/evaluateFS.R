#' evaluate feature set
#' @description evaluates a feature set under the UBayFS model framework
#' @param model a UBaymodel object created using build.UBaymodel
#' @param state a binary membership vector describing a feature set
#' @param log whether the admissibility should be returned on log scale
#' @return a posterior probability value
#' @importFrom stats cor
#' @export

evaluateFS <- function(state, model, method = "spearman", log = TRUE){
  if(sum(state) > 1){
    c <- abs(cor(model$data[, state == 1], method = method))
  }
  else{
    c <- NA
  }
  post_scores <- posteriorExpectation(model)
  log_post <- logSumExp(post_scores[state == 1])
  loss <- getLoss(state, model)

  # calculate output metrics
  return(c(
    size = sum(state),
    loss = ifelse(log, round(loss, 2), round(exp(loss), 2)),
    posterior = ifelse(log, round(log_post, 2), round(exp(log_post, 2))),
    admissibility = round(admissibility(state,
                                            constraints = model$constraint.params$constraints,
                                            log = log), 2),
    block_admissibility = round(block_admissibility(state,
                                                    constraints = model$constraint.params$block_constraints,
                                                    log = log), 2),
    avg_feature_correlation = ifelse(is.matrix(c), round((sum(c) - sum(diag(c))) / (sum(state) * (sum(state) - 1)),2), NA)))
}
