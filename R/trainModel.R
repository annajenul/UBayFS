#' @import dplyr
#' @export

train_model <- function(model, shiny = FALSE){

  sample <- sample.posterior2(model$user.params, model$ensemble.params, model$sampling.params, t_burn = model$sampling.params$t_bi, shiny = shiny)
  feat_set <- sample > 1/ncol(sample)

  df = data.frame(feat_set) %>% group_by_all %>% count
  best_set = feat_set[which(df$n == max(df$n)), ] * 1
  model$output <- list(map = best_set,
                       median = best_set)
  return(model)
}
