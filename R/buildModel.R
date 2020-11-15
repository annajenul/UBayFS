#' @import RENT
#' @export


build.model <- function(data, target, reg.param = NULL, K = 100,
                        testsize = 0.25, A, b, rho, verbose = TRUE){

  if(!is.matrix(data)){
    data <- as.matrix(data)
  }
  if(is.null(reg.param)){
    print("auto-selecting regularization paramters")
    reg.param <- select.reg.param(data, target, model.type)
  }

  RENT_model = RENT::build.model(data = data, target = target, reg.param = reg.param,
                                 K=K, testsize = testsize,
                                 verbose = verbose)
  RENT_model = RENT::train.model(RENT_model)
  counts = apply(RENT::get.weights.matrix(RENT_model) != 0, 2, sum)

  obj <- list(
    data=data,
    target=target,
    prior.params = list(A=A, b=b, rho=rho),
    likelihood.params = list(testsize=testsize, K=K, reg.param=reg.param, counts = counts),
    verbose=verbose,
    RENT_model = RENT_model
  )
  class(obj) <- "RentABaymodel"
  return(obj)
}
