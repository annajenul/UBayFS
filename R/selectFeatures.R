#' @importFrom GA ga
#' @export

selectFeatures <- function(RentABaymodel){
  opt_state <- GA::ga(type = "binary", fitness = posterior,
                      RentABaymodel$likelihood.params,
                      RentABaymodel$prior.params,
                      lower = 0,
                      upper = 1,
                      nBits = ncol(RentABaymodel$data)
                      )

  return(opt_state)
}
