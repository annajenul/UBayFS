#' prints a UBayFS model
#' @description print details of an UBayFS model
#' @param x a UBaymodel object created using build.UBaymodel
#' @param ... unused
#' @import utils
#' @export

print.UBaymodel <- function(x,...){

  if(class(x) != "UBaymodel"){
    stop("Wrong class of x")
  }

  print("data")
  print(str(x$data))
  print(str(x$target))
  print("user.params")
  print(x$user.params)
  print("constraint.params")
  print(x$constraint.params)
  print("ensemble.params")
  print(x$ensemble.params)
  print("optim.params")
  print(x$optim.params)
  if(!is.null(x$output)){
    getResults(x)
  }

}
