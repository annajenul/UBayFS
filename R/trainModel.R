#' @import dplyr
#' @export

train_model <- function(model, shiny = FALSE){

  sample <- sample.posterior(model$user.params, model$ensemble.params, model$sampling.params, shiny = shiny)
  cat("Sampling done")
  feat_set <- sample > 1/ncol(sample)

  df = data.frame(feat_set) %>% group_by_all %>% count
  best_set = as.vector(as.matrix(df[which(df$n == max(df$n)), -ncol(df)] * 1))

  # welche zeilen haben das best_set (in inds)
  inds = apply(feat_set, 1, function(x){return(all(x==best_set))})
  success_probs = apply(sample[inds,], 2, mean)
  print(paste0("best feature set (frequency abs/rel)", sum(inds), "/", sum(inds) / nrow(sample)))

  model$output <- list(map = best_set,
                       success_probs = success_probs)
  return(model)
}
