#' @importFrom GA ga
#' @export

selectFeatures <- function(UBaymodel){
  opt_state <- GA::ga(type = "binary", fitness = posterior,
                      UBaymodel$ensemble.params,
                      UBaymodel$user.params,
                      lower = 0,
                      upper = 1,
                      nBits = ncol(UBaymodel$data),
                      monitor = TRUE
                )

  return(opt_state)
}

