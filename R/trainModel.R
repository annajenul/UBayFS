#' @importFrom GA binary2decimal
#' @importFrom GA decimal2binary
#' @export

train_model <- function(model, shiny = FALSE){

  sample <- sample.posterior2(model$user.params, model$ensemble.params, model$sampling.params, shiny = shiny)

  feat_set <- sample > 1/ncol(sample)
  feat_set_binary <- apply(feat_set, 1, function(x){return(binary2decimal(x))})

  vals <- tabulate(feat_set_binary)
  print(max(vals))
  best_set <- decimal2binary(which.max(vals), ncol(sample))

  model$output <- list(map = best_set,
                       median = best_set)

  #model$output <- list(map = sample[which.max(vals),],
  #                     median = sample[which.min(abs(vals - median(vals))),])
  return(model)
}
