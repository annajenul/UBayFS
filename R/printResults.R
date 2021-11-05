#' feature selection results
#' @description displays and summarizes the results of UBayFS after feature selection.
#' @param model a UBaymodel object created using build.UBaymodel after training
#' @importFrom knitr kable
#' @export

printResults <- function(model){

  feature_sets = apply(model$output$feature_set, 1, function(x){
    paste0(which(x == 1), collapse = ",")
  })

  cat("=== MAP feature sets ===\n")
  print(kable(cbind(feature_sets, t(model$output$metrics))))


  cat("=== MAP feature set similarities ===\n")
  print(model$output$mutual_similarity)
}
